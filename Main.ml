open Lexer
open Parser
open Error
open Format

let main =
    let lexbuf = Lexing.from_channel stdin in
    let ast =
        try Parser.program Lexer.lexer lexbuf
        with Terminate ->
            printf "%d Warning(s), %d Error(s)\n" !numWarnings !numErrors;
            exit 1
    in
    if (!numErrors <> 0) then begin
        printf "%d Warning(s), %d Error(s) : aborting...\n" !numWarnings !numErrors;
        exit 1
    end else begin
        ignore (Irgen.irgen ast);
        exit 0
    end
