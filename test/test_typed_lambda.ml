open OUnit2

module Ast = Typed_lambda.Ast
module Parser = Typed_lambda.Parser
module Types = Typed_lambda.Types
module Typecheck = Typed_lambda.Typecheck

(* Minimal interpreter for testing *)
type value =
  | VInt of int
  | VBool of bool
  | VClosure of string * Ast.expr * env
and env = (string * value) list

let rec lookup (x : string) (env : env) : value =
  match env with
  | [] -> failwith ("Unbound variable: " ^ x)
  | (y,v) :: rest -> if x = y then v else lookup x rest

let rec eval (env : env) (e : Ast.expr) : value =
  match e with
  | Ast.Int n -> VInt n
  | Ast.Bool b -> VBool b
  | Ast.Add (a,b) ->
      (match eval env a, eval env b with
       | VInt va, VInt vb -> VInt (va + vb)
       | _ -> failwith "Runtime type error: + expects ints")
  | Ast.Sub (a,b) ->
      (match eval env a, eval env b with
       | VInt va, VInt vb -> VInt (va - vb)
       | _ -> failwith "Runtime type error: - expects ints")
  | Ast.Var x -> lookup x env
  | Ast.Let (x,e1,e2) ->
      let v1 = eval env e1 in
      eval ((x,v1)::env) e2
  | Ast.LetAnn (x, _ty, e1, e2) ->
      let v1 = eval env e1 in
      eval ((x,v1)::env) e2
  | Ast.Lam (x,_ty,body) ->
      VClosure (x, body, env)
  | Ast.App (f,a) ->
      let vf = eval env f in
      let va = eval env a in
      (match vf with
       | VClosure (param, body, clo_env) ->
           eval ((param, va) :: clo_env) body
       | _ -> failwith "Runtime type error: trying to call a non-function")
  | Ast.If (c,t,e) ->
      match eval env c with
      | VBool true -> eval env t
      | VBool false -> eval env e
      | _ -> failwith "Runtime type error: if condition must be bool"

let rec find_project_root start_dir =
  let examples = Filename.concat start_dir "examples" in
  if Sys.file_exists examples && Sys.is_directory examples then
    start_dir
  else
    let parent = Filename.dirname start_dir in
    if parent = start_dir then start_dir else find_project_root parent

let read_file filename =
  let cwd = try Sys.getcwd () with _ -> "." in
  let proj_root = find_project_root cwd in
  let full_path = Filename.concat proj_root filename in
  let ic = open_in_bin full_path in
  let data = really_input_string ic (in_channel_length ic) in
  close_in ic;
  data


let sorted_lambda_files dir =
  let cwd = try Sys.getcwd () with _ -> "." in
  let proj_root = find_project_root cwd in
  let full_dir = Filename.concat proj_root dir in
  try
    Sys.readdir full_dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".lambda")
    |> List.sort String.compare
  with Sys_error _ -> []

let run_and_check_src src =
  let ast = Parser.parse_string src in
  let ty  = Typecheck.typeof [] ast in
  let v   = eval [] ast in
  (ty, v)

let tests_from_good_dir dir =
  sorted_lambda_files dir
  |> List.map (fun file ->
         let path = Filename.concat dir file in
         ("good: " ^ file) >:: fun _ ->
           try
             let src = read_file path in
             let _ = run_and_check_src src in
             ()
           with
           | Parser.Parse_error msg ->
               assert_failure (Printf.sprintf "Parse error in %s: %s" path msg)
           | Typecheck.Type_error msg ->
               assert_failure (Printf.sprintf "Type error in %s (expected success): %s"
                                 path msg)
           | Failure msg ->
               assert_failure (Printf.sprintf "Runtime error in %s: %s" path msg)
       )

let tests_from_bad_dir dir =
  sorted_lambda_files dir
  |> List.map (fun file ->
         let path = Filename.concat dir file in
         ("bad: " ^ file) >:: fun _ ->
           try
             let src = read_file path in
             let _ = run_and_check_src src in
             assert_failure (Printf.sprintf "Expected TYPE ERROR in %s but succeeded" path)
           with
           | Typecheck.Type_error _ -> ()
           | Parser.Parse_error msg ->
               assert_failure (Printf.sprintf "Parse error in %s (expected type error): %s"
                                 path msg)
           | Failure msg ->
               assert_failure (Printf.sprintf "Runtime error in %s (expected type error): %s"
                                 path msg)
       )

let () =
  let good_tests = tests_from_good_dir "examples/good" in
  let bad_tests  = tests_from_bad_dir "examples/bad" in
  let suite = "typed_lambda examples" >::: (good_tests @ bad_tests) in
  run_test_tt_main suite
