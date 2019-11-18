{
open Parser
open Error

let special c = match c with
  | 'n' -> '\n'
  | 't' -> '\t'
  | 'r' -> '\r'
  | '0' -> Char.chr 0
  | '\\' -> '\\'
  | '\'' -> '\''
  | '"' -> '"'
  | _   -> assert false
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let hex = digit | ['a'-'f' 'A'-'F']
let white = [' ' '\t' '\r']
let common = [^ '\'' '"' '\\' '\n']
let escape = '\\' (['n' 't' 'r' '0' '\\' '\'' '"'] | ('x' hex hex ))

rule lexer = parse
    "byte"      { T_byte }
  | "else"      { T_else }
  | "false"     { T_false }
  | "if"        { T_if }
  | "int"       { T_int }
  | "proc"      { T_proc }
  | "reference" { T_ref }
  | "return"    { T_ret }
  | "while"     { T_while }
  | "true"      { T_true }
  | letter (letter | digit | '_')* as v     { T_id v }
  | digit+ as n                             { T_cint (int_of_string n) }
  | '\'' (common as chr) '\''               { T_char chr }
  | '\'' (escape as esc) '\''               {
      match (String.length esc) with
      | 2 -> T_char (special esc.[1])
      | 4 -> T_char (Char.chr (Scanf.sscanf (String.sub esc 2 2) "%x" (fun x -> x)))
      | _ -> assert false
  }
  | '"'         { strings "" lexbuf }
  | '='         { T_assign }
  | '+'         { T_plus }
  | '-'         { T_minus }
  | '*'         { T_times }
  | '/'         { T_div }
  | '%'         { T_perc }
  | '&'         { T_amp }
  | '!'         { T_excl }
  | '|'         { T_bar }
  | "=="        { T_eq }
  | "!="        { T_neq }
  | '<'         { T_lt }
  | '>'         { T_gt }
  | "<="        { T_lte }
  | ">="        { T_gte }
  | '('         { T_lpar }
  | ')'         { T_rpar }
  | '['         { T_lbra }
  | ']'         { T_rbra }
  | '{'         { T_lcbra }
  | '}'         { T_rcbra }
  | ','         { T_comma }
  | ':'         { T_colon }
  | ';'         { T_semicol }
  | "--" [^ '\n']* { Lexing.new_line lexbuf; lexer lexbuf }
  | "(*"        { comments 1 lexbuf }
  | white +     { lexer lexbuf }
  | '\n'        { Lexing.new_line lexbuf; lexer lexbuf}
  | eof         { T_eof }
  | _ as c      {
      fatal "Unknown character with ascii code: %d\n" (int_of_char c);
      raise Terminate
  }

 and comments level = parse
  | "(*"        { comments (level + 1) lexbuf }
  | "*)"        {
      if level = 1 then lexer lexbuf
      else comments (level - 1) lexbuf
  }
  | '\n'        { Lexing.new_line lexbuf; comments level lexbuf }
  | '*'         { comments level lexbuf }
  | [^ '*' '\n']+ { comments level lexbuf }
  | eof         {
      fatal "Comment not closed, depth : %d\n" level ;
      T_eof
  }

 and strings current = parse
  | '"'    { T_string current }
  | '\n'   {
      warning "Discarding newline found in string literal";
      strings current lexbuf
  }
  | common as chr { strings (current ^ (String.make 1 chr)) lexbuf }
  | escape as esc {
    if (String.length esc == 2) then
        strings (current ^ (String.make 1 (special esc.[1]))) lexbuf
    else if (String.length esc == 4) then
        strings (current ^ (String.make 1 (Char.chr (Scanf.sscanf (String.sub esc 2 2) "%x" (fun x-> x))))) lexbuf
    else assert false
  }
  | _ as x { warning "Invalid character with ascii code %d inside string literal\n" (int_of_char x); strings current lexbuf }
