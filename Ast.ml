type s_func_def = {
    id : string;
    nested_name : string;
    par_list : s_fpar_def list;
    ret_type : Types.typ;
    def_list : s_local_def list;
    comp_stmt : s_stmt list;
    mutable frame_t  : Llvm.lltype option;
    mutable parent_func : s_func_def option
}

and s_fpar_def = {
    pname : string;
    ptype : Types.typ;
    pmode  : Symbol.pass_mode
}


and s_local_def =
    | FuncDef of s_func_def
    | VarDef of s_var_def

and s_local_defs = s_local_def list

and s_var_def = {
    vname : string;
    vtype : Types.typ
}

and s_stmt =
    | Assign of s_l_value * s_expr
    | Compound of s_stmt list
    | VoidFuncCall of s_func_call
    | IfElse of s_cond * s_stmt * s_stmt option
    | While of s_cond * s_stmt
    | Return of s_expr option
    | NOp

and s_l_value = {
    lname : string;
    ltype : Types.typ;
    is_ptr : bool;
    offset : int;
    nest_diff : int;
    ind : s_expr option
}

and expr_k =
    | IntConst of int
    | CharConst of char
    | Lval of s_l_value
    | FuncCall of s_func_call
    | StringLit of string
    | Pos of s_expr
    | Neg of s_expr
    | BinOp of s_expr * s_bin_op * s_expr

and s_expr = {
    kind : expr_k;
    etype : Types.typ
}

and s_bin_op =
    | Plus | Minus | Times
    | Div  | Mod

and s_comp_op =
    | Eq | Neq | Lt
    | Lte | Gt | Gte

and s_log_op = And | Or

and s_func_call = {
    fname : string;
    full_name : string;
    fargs : (s_expr * bool) list;
    fnest_diff : int;
    rtype : Types.typ
}


and s_cond =
    | True | False
    | Not of s_cond
    | Compare of s_expr * s_comp_op * s_expr
    | LogOp of s_cond * s_log_op * s_cond
(*
 * Just to remember what is what
and s_data_type = Types.typ
and s_type = Types.typ
and s_r_type = Types.typ
and s_comp_stmt = s_stmt list
and s_fpar_list = s_fpar_def list
*)

let newFuncRec (n, full_n, pl, rt, ld, s) = {
    id = n; par_list = pl; ret_type = rt; nested_name = full_n;
    def_list = ld; comp_stmt = s; parent_func = None; frame_t = None
    }

let newParRec (n, t, m) = {
    pname = n; ptype = t; pmode = m
    }

let newVarRec (n, t) = { vname = n; vtype = t }


let newLValRec (n, t, ptr, off, nd, opt_ind) =
    {
    lname = n;
    ltype = t;
    is_ptr = ptr;
    offset = off;
    nest_diff = nd;
    ind = opt_ind
    }

let newFuncCallRec (n, f_n, a, d, rt) = {
    fname = n; fargs = a; full_name = f_n;
    fnest_diff = d; rtype = rt
    }
