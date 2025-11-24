(* test/test_typed_lambda.ml *)
open OUnit2

module Ast = Typed_lambda.Ast
module Parser = Typed_lambda.Parser
module Types = Typed_lambda.Types
module Typecheck = Typed_lambda.Typecheck

(* local evaluator matching bin/main.ml semantics (CBV) *)
type value =
  | VInt of int
  | VBool of bool
  | VClosure of string * Ast.expr * env
and env = (string * value) list

let rec lookup (x : string) (env : env) : value =
  match env with
  | [] -> failwith ("Unbound variable: " ^ x)
  | (y,v)::rest -> if x = y then v else lookup x rest

let rec eval (env : env) (e : Ast.expr) : value =
  match e with
  | Ast.Int n -> VInt n
  | Ast.Bool b -> VBool b
  | Ast.Add (a, b) ->
      (match eval env a, eval env b with
       | VInt va, VInt vb -> VInt (va + vb)
       | _ -> failwith "Runtime type error: + expects ints")
  | Ast.Sub (a, b) ->
      (match eval env a, eval env b with
       | VInt va, VInt vb -> VInt (va - vb)
       | _ -> failwith "Runtime type error: - expects ints")
  | Ast.Var x -> lookup x env
  | Ast.Let (x, e1, e2) ->
      let v1 = eval env e1 in
      let env' = (x, v1)::env in
      eval env' e2
  | Ast.Lam (x, _ty, body) -> VClosure (x, body, env)
  | Ast.App (f,a) ->
      let vf = eval env f in
      let va = eval env a in
      (match vf with
       | VClosure (param, body, clo_env) ->
           eval ((param, va) :: clo_env) body
       | _ -> failwith "Runtime type error: trying to call a non-function")
  | Ast.If (c,t,e) ->
      (match eval env c with
       | VBool true -> eval env t
       | VBool false -> eval env e
       | _ -> failwith "Runtime type error: if condition must be bool")

let string_of_value = function
  | VInt n -> string_of_int n
  | VBool true -> "true"
  | VBool false -> "false"
  | VClosure _ -> "<fun>"

(* helper: parse, typeof, eval *)
let run_and_check src =
  let ast = Parser.parse_string src in
  let ty = Typecheck.typeof [] ast in
  let v = eval [] ast in
  (ty, v)

(* assertions *)
let assert_ok src expected_ty expected_val _ctx =
  try
    let (ty, v) = run_and_check src in
    let ty_s = Types.string_of_ty ty in
    assert_equal expected_ty ty_s ~msg:("type mismatch for: " ^ src);
    (* compare value as string *)
    assert_equal expected_val (string_of_value v) ~msg:("value mismatch for: " ^ src)
  with
  | Typecheck.Type_error msg ->
      assert_failure ("Expected OK but got TYPE ERROR: " ^ msg)
  | Failure msg ->
      assert_failure ("Expected OK but got RUNTIME ERROR: " ^ msg)
  | Parser.Parse_error msg ->
      assert_failure ("Parse error: " ^ msg)

let assert_type_error src _ctx =
  try
    let (_ty, _v) = run_and_check src in
    assert_failure ("Expected TYPE ERROR but succeeded: " ^ src)
  with
  | Typecheck.Type_error _ -> ()  (* expected *)
  | Failure msg -> assert_failure ("Expected TYPE ERROR but runtime error: " ^ msg)
  | Parser.Parse_error msg -> assert_failure ("Parse error: " ^ msg)

(* tests based on examples/ files *)
let tests = "typed_lambda tests" >::: [
  "ex1" >:: (assert_ok "1 + (7 - 2)" "int" "6");
  "ex2" >:: (assert_ok "let id = fun (x : int) -> x in id 42" "int" "42");
  "ex3" >:: (assert_ok "let add = fun (a : int) -> fun (b : int) -> a + b in ((add 2) 5)" "int" "7");
  "ex4" >:: (assert_ok "let apply = fun (f : int -> int) -> fun (x : int) -> f x in let inc = fun (n : int) -> n + 1 in ((apply inc) 5)" "int" "6");
  "ex5" >:: (assert_ok "let twice = fun (f : int -> int) -> fun (x : int) -> f (f x) in let inc = fun (n : int) -> n + 1 in ((twice inc) 5)" "int" "7");
  "ex6" >:: (assert_ok "let compose = fun (f : int -> int) -> fun (g : int -> int) -> fun (x : int) -> f (g x) in let inc = fun (n : int) -> n + 1 in let double = fun (n : int) -> (n + n) in (((compose inc) double) 3)" "int" "7");
  "tyerr1" >:: (assert_type_error "let id = fun (x : int) -> x in id (fun (y : int) -> y)");
  "tyerr2" >:: (assert_type_error "let x = fun (y : int) -> y in x + 3");
  "runerr1" >:: (assert_type_error "let x = 5 in x 3");
  "runerr2" >:: (assert_type_error "let id = fun (x : int) -> x in (id 1) 2");
  "bool1" >:: (assert_ok "if true then 1 else 2" "int" "1");
  "bool2" >:: (assert_type_error "let f = fun (x : int) -> if (x - 1) then 1 else 0 in f 3");
  "if_type_err" >:: (assert_type_error "if 1 then 2 else 3");
]

let () =
  run_test_tt_main tests
