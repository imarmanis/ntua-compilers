%{
open Ast
open Types
open Error
open Identifier
open Symbol
open Parsing

let rstack = Stack.create ()

let dstack = Stack.create ()

let rec string_of_type x = match x with
    | TYPE_none -> "none"
    | TYPE_int -> "int"
    | TYPE_byte -> "byte"
    | TYPE_proc -> "proc"
    | TYPE_array (y, _) -> "array of " ^ (string_of_type y)

let set_parent ast =
    let rec f func =
        let g = function
        | FuncDef d -> d.parent_func <- Some func; f d
        | VarDef _ -> () ;
        in List.iter g func.def_list
    in f ast

let addLib () =
    let addFunc name arg_list rtype =
        let func = newFunction (id_make name) true in
        openScope ();
        let addArg (arg_name, arg_type, is_ref)  =
            let mode = match is_ref with
            | true -> PASS_BY_REFERENCE
            | false -> PASS_BY_VALUE
            in ignore (newParameter (id_make arg_name) arg_type mode func true)
        in
        List.iter addArg arg_list;
        endFunctionHeader func rtype;
        closeScope ()
    in
    addFunc "writeInteger" [("n", TYPE_int, false)] TYPE_proc;
    addFunc "writeByte" [("b", TYPE_byte, false)] TYPE_proc;
    addFunc "writeChar" [("b", TYPE_byte, false)] TYPE_proc;
    addFunc "writeString" [("n", TYPE_array (TYPE_byte, 0), true)] TYPE_proc;

    addFunc "readInteger" [] TYPE_int;
    addFunc "readByte" [] TYPE_byte;
    addFunc "readChar" [] TYPE_byte;
    addFunc "readString" [("n", TYPE_int, false); ("s", TYPE_array (TYPE_byte, 0), true)] TYPE_proc;

    addFunc "extend" [("b", TYPE_byte, false)] TYPE_int;
    addFunc "shrink" [("i", TYPE_int, false)] TYPE_byte;

    addFunc "strlen" [("s", TYPE_array (TYPE_byte, 0), true)] TYPE_int;
    addFunc "strcmp" [("s1", TYPE_array (TYPE_byte, 0), true); ("s2", TYPE_array (TYPE_byte, 0), true)] TYPE_int;
    addFunc "strcpy" [("trg", TYPE_array (TYPE_byte, 0), true); ("src", TYPE_array (TYPE_byte, 0), true)] TYPE_proc;
    addFunc "strcat" [("trg", TYPE_array (TYPE_byte, 0), true); ("src", TYPE_array (TYPE_byte, 0), true)] TYPE_proc

let sema_func (fname, par_list, ret_type) =
    let f = newFunction (id_make fname ) true in
    openScope ();
    let set_param par =
        match par.ptype, par.pmode with
        | TYPE_array _, PASS_BY_VALUE -> begin
            error "%aIn function %s: array type cannot be passed by value"
                print_position (position_point (symbol_start_pos())) fname;
                ignore(newParameter (id_make par.pname) par.ptype PASS_BY_REFERENCE f true);
        end
        | _ -> ignore(newParameter (id_make par.pname) par.ptype par.pmode f true);
    in
    List.iter set_param par_list;
    endFunctionHeader f ret_type;
    Stack.push ret_type rstack;
    let full_name = match f.entry_info with
    | ENTRY_function f -> f.function_name
    | _ -> internal "Unreachable: entry_info of function not a function"; raise Terminate
    in (fname, full_name, par_list, ret_type)

let sema_fcall (fname, arg_list) =
    let func = lookupEntry (id_make fname) LOOKUP_ALL_SCOPES true in
    let nd =  (!currentScope.sco_nesting -1) - func.entry_scope.sco_nesting in
    match func.entry_info with
    | ENTRY_function f ->
        let full_name = f.function_name in
        let rec valid_args expr_list entry_list bool_list = begin
            match expr_list, entry_list with
            | expr::t1, entry::t2 ->
                let entry_type, is_byref =
                begin match entry.entry_info with
                | ENTRY_parameter param ->
                        if (param.parameter_mode = PASS_BY_REFERENCE) then
                            begin match expr.kind with
                            | Lval _ -> ()
                            | _ ->  error "%aIn function call %s: only an l-value can be passed by ref"
                            print_position (position_point (symbol_start_pos())) fname
                            end;
                        param.parameter_type, (param.parameter_mode = PASS_BY_REFERENCE )
                | _ -> internal "Unreachable :entrty of parameter_list not a paramter"; TYPE_none, false
                end in
                if not (equalType expr.etype entry_type)
                then begin
                    error "%aIn function call %s, expected parameter of type %s, got %s"
                    print_position (position_point (symbol_start_pos()))
                    fname (string_of_type entry_type) (string_of_type expr.etype);
                end;
                valid_args t1 t2 (is_byref::bool_list)
            | [], [] ->
              newFuncCallRec (fname, full_name, List.combine arg_list (List.rev bool_list), nd, f.function_result)
            | _, _ ->
                error "%aIn function call %s, wrong number of parameters"
                print_position (position_point (symbol_start_pos())) fname;
                newFuncCallRec (fname, full_name, List.combine arg_list
                (List.rev bool_list),
                                nd, f.function_result)
        end in
        valid_args arg_list f.function_paramlist []
    | _ -> fatal "%aIdentifier %s is not a function"
           print_position (position_point (symbol_start_pos())) fname;
           raise Terminate

let sema_binop x y z =
        if not (equalType x y)
        then error "%aType mismatch of operands in %s operator"
        print_position (position_point (symbol_start_pos())) z

let sema_lval (lval_name, opt_expr) =
    let is_array =
        match opt_expr with
        | Some x->
            if not (x.etype = TYPE_int)
            then error "%aIndex of array must be int, not %s"
            print_position (position_point (rhs_start_pos 3))
            (string_of_type x.etype);
            true
        | None -> false
    in
    let entry = lookupEntry (id_make lval_name) LOOKUP_ALL_SCOPES true in
    let nd = (!currentScope).sco_nesting - entry.entry_scope.sco_nesting in
    let (is_ptr, ar_off, idtype) =
        match entry.entry_info with
        | ENTRY_variable var_info ->
                (false, var_info.variable_offset, var_info.variable_type)
        | ENTRY_parameter par_info ->
            begin match par_info.parameter_mode with
            | PASS_BY_VALUE ->
                (false, par_info.parameter_offset, par_info.parameter_type)
            | PASS_BY_REFERENCE ->
                (true, par_info.parameter_offset, par_info.parameter_type)
            end
        | _ -> fatal "%a%s is not a variable or parameter identifier"
                print_position (position_point (symbol_start_pos ())) lval_name;
                raise Terminate
    in match idtype with
    | TYPE_array (t, _) when is_array ->
            newLValRec (lval_name, t, is_ptr, ar_off, nd, opt_expr)
    | t when is_array ->
        error "%aIdentifier %s is not an array, cannot apply []"
        print_position (position_point (symbol_start_pos ())) lval_name;
        newLValRec (lval_name, t, is_ptr, ar_off, nd, opt_expr)
    | r ->
        newLValRec (lval_name, r, is_ptr, ar_off, nd, opt_expr)
%}

%token T_byte T_else T_false T_if T_int T_proc T_ref T_ret
%token T_while T_true T_assign T_plus T_minus T_times
%token T_div T_mod T_excl T_perc T_amp T_bar T_eq T_neq
%token T_lt T_gt T_lte T_gte T_lpar T_rpar T_lbra T_rbra
%token T_lcbra T_rcbra T_comma T_colon T_semicol T_eof
%token <string> T_id T_string
%token <int> T_cint
%token <char> T_char

%left T_bar
%left T_amp
%nonassoc T_eq T_neq T_gt T_lt T_gte T_lte
%left T_plus T_minus
%left T_times T_div T_perc
%right T_pos T_neg T_excl

%nonassoc T_then
%nonassoc T_else

%start program
%type <Ast.s_func_def> program

%%

init:
    {
        initSymbolTable 997;
        addLib ()
    }
program:
    init func_def T_eof {
        (*
        begin
            try
             let main_entry = lookupEntry (id_make "main") LOOKUP_CURRENT_SCOPE false in
                match main_entry.entry_info with
                | ENTRY_function _ -> ()
                | _ -> internal "variable or parameter in outer scope called main"
            with Not_found -> error "The outermost function must be called main"
        end;
        *)
        set_parent $2;
        $2
    }
    ;

define_func:
    | T_id T_lpar T_rpar T_colon r_type {
        sema_func ($1, [], $5)
    }
    | T_id T_lpar fpar_list T_rpar T_colon r_type {
        let par_list = List.rev $3 in
        sema_func ($1, par_list, $6)
    }
    ;

dpush:
    {
        Stack.push (false, false) dstack
    }
    ;

dpop:
    {
        ignore(Stack.pop dstack)
    }
    ;

func_def:
    define_func local_defs dpush compound_stmt dpop {
        ignore(Stack.pop rstack);
        closeScope ();
        let (name, full_name, plist, rtype) = $1
        in newFuncRec (name, full_name, plist, rtype, List.rev $2, $4)
    }
    ;

local_defs:
    | /* nothing */ { [] }
    | local_defs local_def { $2::$1 }
    ;

fpar_list:
    fpar_def { [$1] }
    | fpar_list T_comma fpar_def { $3::$1 }
    ;

fpar_def:
    | T_id T_colon type_ { newParRec ($1, $3, PASS_BY_VALUE) }
    | T_id T_colon T_ref type_ { newParRec ($1, $4, PASS_BY_REFERENCE) }
    ;

data_type:
    T_int { TYPE_int }
    | T_byte { TYPE_byte }
    ;

type_:
    data_type { $1 }
    | data_type T_lbra T_rbra { TYPE_array ($1, 0) }
    ;

r_type:
    data_type { $1 }
    | T_proc { TYPE_proc }
    ;

local_def:
    func_def { FuncDef $1 }
    | var_def { VarDef $1 }
    ;

var_def:
    | T_id T_colon data_type T_semicol {
        ignore(newVariable (id_make $1) $3 true);
        newVarRec ($1, $3)
    }
    | T_id T_colon data_type T_lbra T_cint T_rbra T_semicol {
        let t = TYPE_array ($3, $5) in
        ignore(newVariable (id_make $1) t true);
        newVarRec ($1, t)
    }
    ;

stmt:
    | T_semicol { NOp }
    | l_value T_assign expr T_semicol {
        match $1 with
        | StringLit _ ->
            fatal "%aCannot assign to string"
            print_position (position_point (rhs_start_pos 1));
            raise Terminate
        | Id lvalid  -> begin
            match lvalid.ltype with
            | TYPE_array _ ->
                error "%aCannot assign to array"
                print_position (position_point (rhs_start_pos 1));
                Assign (lvalid, $3)
            | _ ->
                if not (equalType $3.etype lvalid.ltype)
                then error "%aType mismatch in assignment"
                print_position (position_point (symbol_start_pos ()));
                Assign (lvalid, $3)
        end
    }
    | compound_stmt { Compound $1 }
    | func_call T_semicol {
        if not (equalType TYPE_proc $1.rtype)
        then
            warning "%aNon-void return value %s ignored"
            print_position (position_point (symbol_start_pos ()))
            (string_of_type $1.rtype);
        VoidFuncCall $1
    }
    | T_if T_lpar cond T_rpar dpush stmt dpop %prec T_then { IfElse ($3, $6, None) }
    | T_if T_lpar cond T_rpar dpush stmt dpop T_else dpush stmt dpop { IfElse ($3, $6, Some $10) }
    | T_while T_lpar cond T_rpar dpush stmt dpop { While ($3, $6) }
    | T_ret T_semicol {
        if not (equalType TYPE_proc (Stack.top rstack))
        then error "%aFunction expected to return void, got %s"
        print_position (position_point (symbol_start_pos()))
        (string_of_type (Stack.top rstack));
        ignore(Stack.pop dstack);
        Stack.push (true, false) dstack;
        Return None
    }
    | T_ret expr T_semicol {
        if not (equalType $2.etype (Stack.top rstack))
        then error "%aFunction expected to return %s, got %s"
        print_position (position_point (symbol_start_pos ()))
        (string_of_type (Stack.top rstack)) (string_of_type $2.etype);
        ignore(Stack.pop dstack);
        Stack.push (true, false) dstack;
        Return (Some $2)
    }
    ;

compound_stmt:
    T_lcbra stmts T_rcbra { List.rev $2 }
    ;

check_dead:
    {
        match Stack.pop dstack with
        | (true, silent) ->
                if (not silent) then warning "%aStatement[s] in the same block after the return [expr] will be discarded (unreachable)"
                print_position (position_point (symbol_start_pos ()));
                Stack.push (true, true) dstack;
                true
        | (false,_) -> Stack.push (false, false) dstack; false
    }
    ;

stmts :
    | /* nothing */ { [] }
    | stmts check_dead stmt {
        if ($2) then $1 else $3::$1
    }
    ;

func_call:
    | T_id T_lpar T_rpar { sema_fcall ($1, []) }
    | T_id T_lpar expr_list T_rpar { sema_fcall ($1, List.rev $3) }
    ;

expr_list:
    | expr { [$1] }
    | expr_list T_comma expr { $3::$1 }
    ;

expr:
    | T_cint {
        { kind = IntConst $1; etype = TYPE_int }
    }
    | T_char {
        { kind = CharConst $1; etype = TYPE_byte}
    }
    | l_value {
        let l_type = match $1 with
        | StringLit s -> TYPE_array (TYPE_byte, (String.length s) + 1)
        | Id lid -> lid.ltype
        in { kind = Lval $1; etype = l_type }
    }
    | T_lpar expr T_rpar { $2 }
    | func_call {
        { kind = FuncCall $1; etype = $1.rtype }
    }
    | T_plus expr %prec T_pos {
        { kind = Pos $2; etype = $2.etype }
    }
    | T_minus expr %prec T_pos {
        { kind = Neg $2; etype = $2.etype }
    }
    | expr T_plus expr {
        sema_binop $1.etype $3.etype "+";
        { kind = BinOp ($1, Plus, $3); etype = $1.etype }
    }
    | expr T_minus expr {
        sema_binop $1.etype $3.etype "-";
        { kind = BinOp ($1, Minus, $3); etype = $1.etype }
    }
    | expr T_times expr {
        sema_binop $1.etype $3.etype "*";
        { kind = BinOp ($1, Times, $3); etype = $1.etype }
    }
    | expr T_div expr {
        sema_binop $1.etype $3.etype "/";
        { kind = BinOp ($1, Div, $3); etype = $1.etype }
    }
    | expr T_perc expr {
        sema_binop $1.etype $3.etype "%";
        { kind = BinOp ($1, Mod, $3); etype = $1.etype }
    }
    ;

l_value:
    | T_id { Id (sema_lval ($1, None)) }
    | T_id T_lbra expr T_rbra { Id (sema_lval ($1, Some $3)) }
    | T_string { StringLit $1 }
    ;

cond:
    | T_true { True }
    | T_false { False }
    | T_lpar cond T_rpar { $2 }
    | T_excl cond { Not $2 }
    | expr T_eq expr {
        sema_binop $1.etype $3.etype "==";
        Compare ($1, Eq, $3)
    }
    | expr T_neq expr {
        sema_binop $1.etype $3.etype "!=";
        Compare ($1, Neq, $3)
    }
    | expr T_lt expr {
        sema_binop $1.etype $3.etype "<";
        Compare ($1, Lt, $3)
    }
    | expr T_lte expr {
        sema_binop $1.etype $3.etype "<=";
        Compare ($1, Lte, $3)
    }
    | expr T_gt expr {
        sema_binop $1.etype $3.etype ">";
        Compare ($1, Gt, $3)
    }
    | expr T_gte expr {
        sema_binop $1.etype $3.etype ">=";
        Compare ($1, Gte, $3)
    }
    | cond T_amp cond { LogOp ($1, And, $3) }
    | cond T_bar cond { LogOp ($1, Or, $3) }
    ;

%%
