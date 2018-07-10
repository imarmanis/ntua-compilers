%{
open Ast
open Types
open Error
open Identifier
open Symbol

let rstack = Stack.create ()

let rec string_of_type x = match x with
    | TYPE_none -> "none"
    | TYPE_int -> "int"
    | TYPE_byte -> "byte"
    | TYPE_proc -> "proc"
    | TYPE_array (y, _) -> "array of " ^ (string_of_type y)

let addLib () =
    let addFunc name arg_list rtype =
        let func = newFunction (id_make name) true in
        openScope ();
        let addArg (arg_name, arg_type, is_ref)  =
            let mode = match is_ref with
            | true -> PASS_BY_REFERENCE
            | false -> PASS_BY_REFERENCE
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

let sema_func ((fname, par_list, ret_type) as func) =
    let f = newFunction (id_make fname ) true in
    openScope ();
    let set_param par =
        ignore(newParameter (id_make par.pname) par.ptype par.pmode f true) in
    List.iter set_param par_list;
    endFunctionHeader f ret_type;
    Stack.push ret_type rstack;
    func

let sema_fcall (fname, arg_list) =
    let func = lookupEntry (id_make fname) LOOKUP_ALL_SCOPES true in
    let nd = func.entry_scope.sco_nesting - !currentScope.sco_nesting in
    match func.entry_info with
    | ENTRY_function f ->
        let rec valid_args expr_list entry_list = begin
            match expr_list, entry_list with
            | expr::t1, entry::t2 ->
                let entry_type =
                begin match entry.entry_info with
                | ENTRY_parameter param -> param.parameter_type
                | _ -> internal "Unreachable :entrty of parameter_list not a paramter"; TYPE_none
                end in
                if not (equalType expr.etype entry_type)
                then begin
                    error "In function call %s, expected parameter of type %s, got %s"
                    fname (string_of_type entry_type) (string_of_type expr.etype);
                end;
                valid_args t1 t2
            | [], [] -> newFuncCallRec (fname, arg_list, nd, f.function_result)
            | _, _ ->
                error "In function call %s, wrong number of parameters" fname;
                newFuncCallRec (fname, arg_list, nd, f.function_result)
        end in
        valid_args arg_list f.function_paramlist
    | _ -> error "Identifier %s is not a function" fname; raise Terminate

let sema_binop x y z =
        if not (equalType x y)
        then error "Type mismatch of operands in %s operator" z

let sema_lval (lval_name, opt_expr) =
    let is_array =
        match opt_expr with
        | Some x->
            if not (x.etype = TYPE_int)
            then error "Index must be int, got %s" (string_of_type x.etype);
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
        | _ ->
         error "In left side of assignment, %s not a declared variable"
         lval_name; (false, 0, TYPE_none)
    in match idtype with
    | TYPE_array (t, _) when is_array ->
            newLValRec (lval_name, t, is_ptr, ar_off, nd, opt_expr)
    | t when is_array ->
        error "Identifier %s not an array, cannot apply []" lval_name;
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

%start program
%type <Ast.s_func_def> program

%%

init:
    {
        initSymbolTable 997;
        openScope ();
        addLib ();
    }
program:
    init func_def T_eof {
        closeScope ();
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

func_def:
    define_func local_defs compound_stmt {
        ignore(Stack.pop rstack);
        closeScope ();
        match $1 with (name, plist, rtype) ->
            newFuncRec (name, plist, rtype, $2, $3)
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
        match $1.ltype with
        | TYPE_array _ -> error "Cannot assign to array"; Assign ($1, $3)
        | _ -> if not (equalType $3.etype $1.ltype)
            then error "Type mismatch in assignment"; Assign ($1, $3)
    }
    | compound_stmt { Compound $1 }
    | func_call T_semicol {
        if not (equalType TYPE_proc $1.rtype)
        then (warning "Non void return value %s ignored" (string_of_type $1.rtype));
        VoidFuncCall $1
    }
    | T_if T_lpar cond T_rpar stmt { IfElse ($3, $5, None) }
    | T_if T_lpar cond T_rpar stmt T_else stmt { IfElse ($3, $5, Some $7) }
    | T_while T_lpar cond T_rpar stmt { While ($3, $5) }
    | T_ret T_semicol {
        if not (equalType TYPE_proc (Stack.top rstack))
        then error "Function expected to return void, got %s" (string_of_type
        (Stack.top rstack));
        Return None
    }
    | T_ret expr T_semicol {
        if not (equalType $2.etype (Stack.top rstack))
        then error "Function expected to return %s, got %s"
        (string_of_type $2.etype) (string_of_type (Stack.top rstack));
        Return (Some $2)
    }
    ;

compound_stmt:
    T_lcbra stmts T_rcbra { List.rev $2 }
    ;

stmts :
    /* nothing */ { [] }
    | stmts stmt { $2::$1 }
    ;

func_call:
    | T_id T_lpar T_rpar { sema_fcall ($1, []) }
    | T_id T_lpar expr_list T_rpar { sema_fcall ($1, List.rev $3) }
    ;

expr_list:
    expr { [$1] }
    | expr_list T_comma expr { $3::$1 }
    ;

expr:
    T_cint {
        { kind = IntConst $1; etype = TYPE_int }
    }
    | T_char {
        { kind = CharConst $1; etype = TYPE_byte}
    }
    | l_value {
        { kind = Lval $1; etype = $1.ltype}
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
    | T_string {
        { kind = StringLit $1; etype = TYPE_array (TYPE_byte, (String.length $1) + 1) }
    }
    ;

l_value:
    T_id { sema_lval ($1, None) }
    | T_id T_lbra expr T_rbra { sema_lval ($1, Some $3) }
    ;

cond:
    T_true { True }
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
