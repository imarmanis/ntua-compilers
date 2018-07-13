open Lexer
open Parser
open Error
open Format

let main =
    let lexbuf = Lexing.from_channel stdin in
    let ast = Parser.program Lexer.lexer lexbuf in
    printf "%d Warnings, %d Errors\n" !numWarnings !numErrors;
    if (!numErrors <> 0) then print_string "Aborting...\n"
    else begin
        print_string "No errors, continuing...\n";
        ignore (Irgen.irgen ast)
    end
