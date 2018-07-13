open Llvm
open Ast
open Symbol
open Types
open Error

let context = global_context ()
let the_module = create_module context "alan compiler"
let builder = builder context
let int_type = i64_type context
let char_type = i8_type context
let void_type = void_type context

let rec to_llvm_type x = match x with
| TYPE_int -> int_type
| TYPE_byte -> char_type
| TYPE_array (t,n) -> array_type (to_llvm_type t) n

let declare_lib () =
    let ft = function_type void_type [|int_type|] in
    ignore(declare_function "writeInteger" ft the_module)

let rec add_ft ast =
    let locals_list =
        let g h tl = match h with
        | VarDef x -> (to_llvm_type x.vtype) :: tl
        | FuncDef _ -> tl
        in List.fold_right g ast.def_list []
    in
    let param_list = 
        let g x = match x.pmode with
        | PASS_BY_VALUE -> to_llvm_type x.ptype
        | PASS_BY_REFERENCE ->
        begin
            match x.ptype with 
            | TYPE_array (t, n) -> pointer_type (to_llvm_type t)
            | t -> pointer_type (to_llvm_type t)
        end
        in List.map g ast.par_list
    in
    let par_frame_ptr =
        match ast.parent_func with
        | Some par ->
            let Some ft = par.frame_t in [ pointer_type ft ]
        | None -> []
    in
    let ft_struct = named_struct_type context ("ft_" ^ ast.nested_name) in
    ast.frame_t <- Some ft_struct;
    let types_array = Array.of_list (List.concat [par_frame_ptr; param_list; locals_list]) in
    struct_set_body ft_struct types_array false;
    print_string (string_of_lltype ft_struct);
    print_string "\n";
    let h x = match x with
    | FuncDef f -> add_ft f
    | VarDef _ -> ()
    in List.iter h ast.def_list

let rec check_func x =
    let rec g y = match y with
    | Assign (a,_) -> print_string ("var : " ^ a.lname ^" , no : " ^ (string_of_int a.offset) ^ " depth = " ^ (string_of_int a.nest_diff) ^ " &&&\n")
    | Compound s -> List.iter g s
    | _ -> () 
    in
    List.iter g x.comp_stmt;
    let h x =
            match x with
            | FuncDef d -> check_func d
            | _ -> ()
    in List.iter h x.def_list

let irgen func =
    declare_lib ();
    add_ft func;
    if (true) then
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

