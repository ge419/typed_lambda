module Types = Typed_lambda.Types
module Typecheck = Typed_lambda.Typecheck
module Parser = Typed_lambda.Parser
module Interpreter = Typed_lambda.Interpreter

let process_string src =
  try
    let ast = Parser.parse_string src in
    (* typecheck *)
    let ty = Typecheck.typeof [] ast in
    let ty_str = Types.string_of_ty ty in
    let v = Interpreter.eval [] ast in
    Printf.printf "OK: type=%s, value=%s\n" ty_str (Interpreter.string_of_value v)
  with
  | Parser.Parse_error msg ->
      Printf.printf "PARSE ERROR: %s\n" msg
  | Typecheck.Type_error msg ->
      Printf.printf "TYPE ERROR: %s\n" msg
  | Failure msg ->
      Printf.printf "RUNTIME ERROR: %s\n" msg

let run_file fname =
  let ic = open_in fname in
  let src = really_input_string ic (in_channel_length ic) in
  close_in ic;
  process_string src


let repl () =
  Printf.printf "typed_lambda REPL. End multiline input with ';;' on its own line.\n";
  let rec loop env =
    print_string "> "; flush stdout;
    match read_line () with
    | exception End_of_file -> ()
    | line ->
        let trimmed = String.trim line in
        if trimmed = "" then loop env
        else if trimmed = ":q" || trimmed = ":quit" then (Printf.printf "bye\n"; ())
        else if String.length trimmed >= 5 && String.sub trimmed 0 5 = ":type" then
          let rest = String.trim (String.sub trimmed 5 (String.length trimmed - 5)) in
          if rest = "" then (Printf.printf "Usage: :type <expr>\n"; loop env)
          else
            (try
               let ast = Parser.parse_string rest in
               let ty = Typecheck.typeof [] ast in
               Printf.printf "type: %s\n" (Types.string_of_ty ty);
             with
             | Parser.Parse_error msg -> Printf.printf "PARSE ERROR: %s\n" msg
             | Typecheck.Type_error msg -> Printf.printf "TYPE ERROR: %s\n" msg);
          loop env
        else
          let buf = Buffer.create 256 in
          Buffer.add_string buf line;
          Buffer.add_char buf '\n';
          let rec read_more () =
            print_string "| "; flush stdout;
            match read_line () with
            | exception End_of_file -> ()
            | l ->
                if String.trim l = ";;" then ()
                else (Buffer.add_string buf l; Buffer.add_char buf '\n'; read_more ())
          in
          read_more ();
          let src = Buffer.contents buf in
          process_string src;
          loop env
  in
  loop []

let () =
  if Array.length Sys.argv >= 2 then
    run_file Sys.argv.(1)
  else
    repl ()
