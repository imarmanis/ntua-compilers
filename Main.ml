open Lexer
open Parser
open Error
open Format
open String
open Arg
open Llvm

(* MODIFY ACCORDINGLY IF NEEDED *)
let llc_exe = "llc"

let o_flag = ref false
let f_flag = ref false
let i_flag = ref false
let l_flag = ref false
let anon_flag = ref false
let inp_name = ref None

let usage_msg = sprintf "Usage: %s [-O] [-f|-i| [-l] filename]" Sys.argv.(0)
let parse_list =
    [("-O", Set o_flag, "optimization flag");
    ("-l", Tuple [Set l_flag; Clear i_flag; Clear f_flag],
    "pass file.s to ./as-ld.py : clang file.s /lib/lib.a -o file");
    ("-f", Tuple [Set f_flag; Clear anon_flag; Clear i_flag],
    "emit target code to stdout, from stdin");
    ("-i", Tuple [Set i_flag; Clear anon_flag; Clear f_flag],
    "emit intermediate code to stdout, from stdin")]

let main =
    let anon_f = fun s ->
        anon_flag := true; i_flag := false; f_flag := false; inp_name := Some s
    in parse parse_list anon_f usage_msg;
    if not (!f_flag || !i_flag || !anon_flag) then begin
        print_endline usage_msg; exit 1
    end;
    let infile_prefix, input_channel =
        if (!anon_flag) then
            match !inp_name with
            | None -> print_string (usage_msg ^ "\n"); exit 1
            | Some n ->
                try
                    let prefix s =
                        let s = Filename.basename s in
                        if (length (Filename.extension s) < 2) then begin
                            printf "Input file must be <name.extension>\n"; exit 1
                        end else Filename.remove_extension s
                    in
                    let a = prefix n in
                    let b =  open_in n in
                    a,b
                with Sys_error _ ->
                    printf "Failed to open input file: %s\n" n;
                    exit 1
        else "stdin", stdin
    in let lexbuf = Lexing.from_channel input_channel in
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
        let () = try Irgen.irgen ast !o_flag
                 with Terminate -> exit 1
        in
        let ir_string = string_of_llmodule Irgen.the_module in
        if (!anon_flag) then begin
            let outp_name = infile_prefix ^ ".ll" in
            let output_channel =
                try open_out outp_name
                with Sys_error _ ->
                    printf "Failed to open output file: %s\n" outp_name;
                    exit 1
            in output_string output_channel ir_string;
            let cmd = Printf.sprintf "%s %s -o %s" llc_exe outp_name (infile_prefix ^ ".s")
            in ignore(Unix.handle_unix_error Unix.open_process cmd);
            if (!l_flag) then
                let cmd = Printf.sprintf "%s %s" "./as-ld.py" (infile_prefix ^ ".s")
                in ignore(Unix.handle_unix_error Unix.open_process cmd);
            close_in input_channel;
            close_out output_channel
        end else if (!i_flag) then begin
            output_string stdout ir_string
        end else if (!f_flag) then begin
            let llc_in = Unix.handle_unix_error Unix.open_process_out llc_exe in
            output_string llc_in ir_string
        end;
        exit 0
    end
