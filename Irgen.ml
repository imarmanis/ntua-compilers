open Llvm
open Ast
open Symbol
open Types
open Error

let context = global_context ()
let the_module = create_module context ".alan source file"
let builder = builder context
let int_type = i64_type context
let char_type = i8_type context
let void_type = void_type context
let bool_type = i1_type context

let qq () = dump_module the_module; raise Terminate

let rec string_of_type x = match x with
    | TYPE_none -> "none"
    | TYPE_int -> "int"
    | TYPE_byte -> "byte"
    | TYPE_proc -> "proc"
    | TYPE_array (y, _) -> "array of " ^ (string_of_type y)

let rec to_llvm_type x = match x with
| TYPE_int -> int_type
| TYPE_byte -> char_type
| TYPE_array (t,n) -> array_type (to_llvm_type t) n
| TYPE_proc -> void_type
| _ -> internal "Trying to get an invalid llvm type"; raise Terminate

let param_to_llvm_type x = match x.pmode with
    | PASS_BY_VALUE -> to_llvm_type x.ptype
    | PASS_BY_REFERENCE ->
    begin
        match x.ptype with
        | TYPE_array (t, n) -> pointer_type (to_llvm_type t)
        | t -> pointer_type (to_llvm_type t)
    end

let declare_lib () =
    let declare_func name ret_type arg_l =
        let ft = function_type ret_type (Array.of_list arg_l) in
        ignore(declare_function name ft the_module)
    in
    declare_func "writeInteger" void_type [int_type];
    declare_func "writeByte" void_type [char_type];
    declare_func "writeChar" void_type [char_type];
    declare_func "writeString" void_type [pointer_type char_type];
    declare_func "readInteger" int_type [];
    declare_func "readByter" char_type [];
    declare_func "readChar" char_type [];
    declare_func "readString" void_type [int_type; pointer_type char_type];
    declare_func "extend" int_type [char_type];
    declare_func "shrink" char_type [int_type];
    declare_func "strlen" int_type [pointer_type char_type];
    declare_func "strcmp" int_type [pointer_type char_type; pointer_type char_type];
    declare_func "strcpy" void_type [pointer_type char_type; pointer_type char_type];
    declare_func "strcat" void_type [pointer_type char_type; pointer_type char_type]

let rec add_ft ast =
    let locals_list =
        let g h tl = match h with
        | VarDef x -> (to_llvm_type x.vtype) :: tl
        | FuncDef _ -> tl
        in List.fold_right g ast.def_list []
    in
    let param_list = List.map param_to_llvm_type ast.par_list in
    let par_frame_ptr =
        match ast.parent_func with
        | Some par ->
            let ft = match par.frame_t with
            | Some x -> x
            | None -> internal "Trying to access a frame_t that isn't set yet";
                        raise Terminate
            in [ pointer_type ft ]
        | None -> []
    in
    let ft_struct = named_struct_type context ("ft_" ^ ast.nested_name) in
    ast.frame_t <- Some ft_struct;
    let types_array = Array.of_list (List.concat [par_frame_ptr; param_list; locals_list]) in
    struct_set_body ft_struct types_array false;
    let h x = match x with
    | FuncDef f -> add_ft f
    | VarDef _ -> ()
    in List.iter h ast.def_list

let to_icmp_type x = match x with
    | Eq, _ -> Icmp.Eq
    | Neq, _ -> Icmp.Ne
    | Lt, TYPE_int -> Icmp.Slt
    | Lte, TYPE_int -> Icmp.Sle
    | Gt, TYPE_int -> Icmp.Sgt
    | Gte, TYPE_int -> Icmp.Sge
    | Lt, TYPE_byte -> Icmp.Ult
    | Lte, TYPE_byte -> Icmp.Ule
    | Gt, TYPE_byte -> Icmp.Ugt
    | Gte, TYPE_byte -> Icmp.Uge
    | _ -> internal "Invalid compareOp, TYPE_ combination";
            raise Terminate

let rec get_fr_ptr fr_ptr x = match x with
    | 0 -> fr_ptr
    | n ->
        let link_ptr = build_struct_gep fr_ptr 0 "link_ptr" builder in
        let link = build_load link_ptr "link" builder in
        get_fr_ptr link (n - 1)

let rec gen_fcall str frame f =
    let gen_param frame (arg, is_byref) =
        if (is_byref)  then
            begin
            let lval = match arg.kind with
            | Lval x -> x
            | _ -> internal "Semantic analysis error (parameter passed by ref \
                                but is not an lvalue"; raise Terminate
            in match lval with
            | Id lid -> gen_lval_id frame lid
            | StringLit s ->
                    let the_string = build_global_string s "string-lit" builder in
                    build_struct_gep the_string 0 "string_to_char_ptr" builder
            end
        else gen_expr frame arg
    in
    let func = match lookup_function f.full_name the_module with
    | Some x -> x
    | None -> internal "Semantic analysis error (calling undefined function)";
                raise Terminate
    in
    let expr_list =
        if (Array.length (params func) = List.length f.fargs) then
            List.map (gen_param frame) f.fargs
        else
            let nest_link = get_fr_ptr frame (f.fnest_diff + 1)
            in nest_link::(List.map (gen_param frame) f.fargs)
    in
        let expr_array = Array.of_list expr_list in
        build_call func expr_array str builder

and gen_lval_id frame l =
    let frame_ptr = get_fr_ptr frame l.nest_diff in
    let elem_ptr = build_struct_gep frame_ptr l.offset "elem_ptr" builder in
    let the_elem_ptr =
        if (l.is_ptr) then build_load elem_ptr "deref" builder
        else match l.ltype, l.ind with
        | TYPE_array _, None | _, Some _ -> build_struct_gep elem_ptr 0 "array_to_ptr" builder
        | _ -> elem_ptr
    in
    match l.ind with
    | Some expr ->
        let ind_expr = gen_expr frame expr in
        build_gep the_elem_ptr [|ind_expr|] "array_elem" builder
    | None  -> the_elem_ptr


and gen_expr frame x = match x.kind with
    | IntConst c -> const_int int_type c
    | CharConst c -> const_int char_type (int_of_char c)
    | Lval l ->
        begin match l with
        | Id l ->
            let lval = gen_lval_id frame l in
            build_load lval "expr" builder
        | StringLit _ -> internal "Trying to generate expression from string literal";
                            raise Terminate
        end
    | FuncCall f ->  gen_fcall "ret_val" frame f
    | Pos p -> gen_expr frame p
    | Neg n -> build_neg (gen_expr frame n) "neg" builder
    | BinOp (expr1, op, expr2) ->
        let expr_type = expr1.etype in
        let expr1 = gen_expr frame expr1 in
        let expr2 = gen_expr frame expr2 in
        let build_fn, txt = begin match op, expr_type with
        | Plus,_ -> build_add, "add"
        | Minus,_ -> build_sub, "sub"
        | Times,_ -> build_mul, "mul"
        | Div,TYPE_int -> build_sdiv, "sdiv"
        | Div,TYPE_byte -> build_udiv, "udiv"
        | Mod,TYPE_int -> build_srem, "srem"
        | Mod,TYPE_byte -> build_urem, "urem"
        | _ -> internal "Invalid binaryOp, TYPE_ combination"; raise Terminate
        end in build_fn expr1 expr2 txt builder

let rec gen_cond frame cond = match cond with
    | True -> const_int bool_type 1
    | False -> const_int bool_type 0
    | Not ncond -> build_not (gen_cond frame ncond) "not" builder

    | Compare (expr1, op, expr2) ->
            let op_type = expr1.etype in
            let expr1 = gen_expr frame expr1 in
            let expr2 = gen_expr frame expr2 in
            build_icmp (to_icmp_type (op, op_type)) expr1 expr2 "cmp" builder

    | LogOp (cond1, op, cond2) ->
            let cond1 = gen_cond frame cond1 in
            let start_bb = insertion_block builder in
            let the_function = block_parent start_bb in
            let eval_sec_bb = append_block context "second-cond" the_function in
            let merge_bb = append_block context "merge" the_function in
            if (op = And) then ignore(build_cond_br cond1 eval_sec_bb merge_bb builder)
            else ignore(build_cond_br cond1 merge_bb eval_sec_bb builder);

            position_at_end eval_sec_bb builder;
            let cond2 = gen_cond frame cond2 in
            let new_eval_bb = insertion_block builder in
            ignore(build_br merge_bb builder);

            position_at_end merge_bb builder;
            let inc_from_start = match op with
            | Or -> const_int bool_type 1
            | And -> const_int bool_type 0
            in build_phi [(inc_from_start, start_bb);(cond2, new_eval_bb)] "and-or_phi" builder

let rec gen_stmt frame stmt = match stmt with
    | Assign (lval, expr) ->
            let lval = gen_lval_id frame lval in
            let expr = gen_expr frame expr in
            ignore(build_store expr lval builder)

    | Compound stmt_list -> List.iter (gen_stmt frame) stmt_list

    | VoidFuncCall f -> ignore(gen_fcall "" frame f)

    | IfElse (cond, then_stmt, opt_else_stmt) ->
            let cond_val = gen_cond frame cond in
            let start_bb = insertion_block builder in
            let the_function = block_parent start_bb in

            let then_bb = append_block context "then" the_function in
            let merge_bb = append_block context "ifcont" the_function in

            position_at_end then_bb builder;
            gen_stmt frame then_stmt;
            ignore(build_br merge_bb builder);

            begin match opt_else_stmt with
            | Some else_stmt ->
                let else_bb = append_block context "else" the_function in

                position_at_end else_bb builder;
                gen_stmt frame else_stmt;
                ignore(build_br merge_bb builder);

                position_at_end start_bb builder;
                ignore(build_cond_br cond_val then_bb else_bb builder)

            | None ->
                position_at_end start_bb builder;
                ignore(build_cond_br cond_val then_bb merge_bb builder)
            end;

            position_at_end merge_bb builder

    | While (cond, stmt) ->
            let start_bb = insertion_block builder in
            let the_function = block_parent start_bb in
            let cond_bb = append_block context "loopcond" the_function in
            let loop_bb = append_block context "loopbody" the_function in
            let merge_bb = append_block context "loop_cont" the_function in
            ignore(build_br cond_bb builder);

            position_at_end cond_bb builder;
            let cond_val = gen_cond frame cond in
            ignore(build_cond_br cond_val loop_bb merge_bb builder);

            position_at_end loop_bb builder;
            gen_stmt frame stmt;
            ignore(build_br cond_bb builder);

            position_at_end merge_bb builder

    | Return (Some expr) ->
            let ret_val = gen_expr frame expr in
            ignore(build_ret ret_val builder)

    | Return None -> ignore(build_ret_void builder)

    | NOp -> ()




let rec gen_func f isOuter =
    let helper x = match x with
    | FuncDef f -> gen_func f false
    | _ -> ()
    in  List.iter helper f.def_list;
    let param_list =
        if isOuter
            then (List.map param_to_llvm_type f.par_list)
        else
            let parent_f = match f.parent_func with
            | Some x -> x
            | None -> internal "Trying to get parent function but pointer is not set";
                        raise Terminate
            in let parent_ft = match parent_f.frame_t with
            | Some x -> x
            | None -> internal "Trying to get parent's frame_t but pointer is not set";
                        raise Terminate
            in (pointer_type parent_ft)::(List.map param_to_llvm_type f.par_list)
    in
    let param_array = Array.of_list param_list in
    let ret_type = to_llvm_type f.ret_type in
    let func_t = function_type ret_type param_array in
    let func = match lookup_function f.nested_name the_module with
        | None -> declare_function f.nested_name func_t the_module
        | Some f -> f (* error *)
    in
    let bb = append_block context "entry" func in
    position_at_end bb builder;
    let frame_type = match f.frame_t with
    | Some x -> x
    | None -> internal "Trying to get frame_t but pointer is not set";
                raise Terminate
    in
    let frame = build_alloca frame_type "frame" builder in
    let i = ref 0 in
    let store_param param =
        let elem_ptr = build_struct_gep frame !i "frame_elem" builder in
        ignore(build_store param elem_ptr builder); incr i;
        in
    iter_params store_param func;
    List.iter (gen_stmt frame) f.comp_stmt;
    match f.ret_type with
    | TYPE_proc -> ignore(build_ret_void builder)
    | TYPE_int -> ignore(build_ret (const_int int_type 0) builder)
    | TYPE_byte -> ignore(build_ret (const_int char_type 0) builder)
    | _ -> internal "Function %s returns invalid type" f.nested_name; raise Terminate

let irgen func =
    declare_lib ();
    add_ft func;
    gen_func func true;
    dump_module the_module;

    if (false) then
        begin
            let ft = function_type int_type ([||]) in
            let f = match lookup_function "main" the_module with
            | None -> declare_function "main" ft the_module
            | Some f -> f
            in
            let bb = append_block context "entry" f in
            position_at_end bb builder;
            let ret_val = const_int int_type 42 in
            let wr_i = match lookup_function "writeInteger" the_module with
            | None -> raise Terminate
            | Some f -> f
            in
            ignore(build_call wr_i [|ret_val|] "" builder);
            let _ = build_ret ret_val builder in
            dump_module the_module
        end
    else ()

