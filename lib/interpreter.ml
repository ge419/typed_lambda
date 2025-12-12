open Ast
open Types

(* Value types with security labels *)
type value =
  | VInt of int * label
  | VBool of bool * label
  | VClosure of string * expr * env  (* Function closure: parameter name, function body, and the environment at the time of function definition *)
and env = (string * value) list

(* Retrieve label *)
let label_of_value = function
  | VInt (_, l) -> l
  | VBool (_, l) -> l
  | VClosure _ -> Public  (* closures treated as public *)

(* Set label *)
let set_label v l =
  match v with
  | VInt (n, _) -> VInt (n, l)
  | VBool (b, _) -> VBool (b, l)
  | VClosure _ -> v

(* Lookup x in env *)
let rec lookup (x : string) (env : env) : value =
  match env with
  | [] -> failwith ("Unbound variable: " ^ x)
  | (y,v)::rest -> if x = y then v else lookup x rest

(* Takes current environment and expression, return a value *)
let rec eval (env : env) (e : expr) : value =
  match e with
  | Int n -> VInt (n, Public)
  | Bool b -> VBool (b, Public)
  | Add (a, b) ->
      (match eval env a, eval env b with
       | VInt (va, l1), VInt (vb, l2) ->
           VInt (va + vb, join_label l1 l2)
       | _ -> failwith "Runtime type error: + expects ints")
  | Sub (a, b) ->
      (match eval env a, eval env b with
       | VInt (va, l1), VInt (vb, l2) ->
           VInt (va - vb, join_label l1 l2)
       | _ -> failwith "Runtime type error: - expects ints")
  | Var x -> lookup x env
  | Let (x, e1, e2) ->
      let v1 = eval env e1 in
      let env' = (x, v1)::env in
      eval env' e2
  | LetAnn (x, _decl_ty, e1, e2) ->
      let v1 = eval env e1 in
      let env' = (x, v1)::env in
      eval env' e2
  | Lam (x, _ty, body) -> VClosure (x, body, env)
  | App (f,a) ->
      let vf = eval env f in
      let va = eval env a in
      (match vf with
       | VClosure (param, body, clo_env) ->
           eval ((param, va) :: clo_env) body
       | _ -> failwith "Runtime type error: trying to call a non-function")
  | If (c,t,e) ->
      (match eval env c with
       | VBool (true, lc) ->
           let vt = eval env t in
           let lr = join_label lc (label_of_value vt) in
           set_label vt lr
       | VBool (false, lc) ->
           let ve = eval env e in
           let lr = join_label lc (label_of_value ve) in
           set_label ve lr
       | _ -> failwith "Runtime type error: if condition must be bool")

(* Function to convert value to string for readability *)
let string_of_value = function
  | VInt (n, l) -> string_of_int n ^ "@" ^ string_of_label l
  | VBool (b, l) -> string_of_bool b ^ "@" ^ string_of_label l
  | VClosure _ -> "<fun>"

(* Helper function that prints the value type and the actual value *)
let eval_to_string ?(env : env = []) ast =
  let v = eval env ast in
  (string_of_value v, v)