open Ast

(* Define value types which represent ; value types imply that it does not need to be evaluated *)
type value =
  | VInt of int (* Integer *)
  | VBool of bool (* Boolean *)
  | VClosure of string * expr * env (* Function closure: parameter name, function body, and the environment at the time of function definition *)
and env = (string * value) list

(* Lookup x on env *)
let rec lookup (x : string) (env : env) : value =
  match env with
  | [] -> failwith ("Unbound variable: " ^ x)
  | (y,v)::rest -> if x = y then v else lookup x rest

(* Takes current environment and expression, return a value *)
let rec eval (env : env) (e : expr) : value =
  match e with
  | Int n -> VInt n
  | Bool b -> VBool b
  | Add (a, b) ->
      (match eval env a, eval env b with
       | VInt va, VInt vb -> VInt (va + vb)
       | _ -> failwith "Runtime type error: + expects ints")
  | Sub (a, b) ->
      (match eval env a, eval env b with
       | VInt va, VInt vb -> VInt (va - vb)
       | _ -> failwith "Runtime type error: - expects ints")
  | Var x -> lookup x env
  | Let (x, e1, e2) ->
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
       | VBool true -> eval env t
       | VBool false -> eval env e
       | _ -> failwith "Runtime type error: if condition must be bool")

(* Function to convert value to string for readability *)
let string_of_value = function
  | VInt n -> string_of_int n
  | VBool true -> "true"
  | VBool false -> "false"
  | VClosure _ -> "<fun>"

(* Helper function that prints the value type and the actual value *)
let eval_to_string ?(env : env = []) ast =
  let v = eval env ast in
  (string_of_value v, v)
