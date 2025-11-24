open Ast
open Types

(* Define an exception for type errors *)
exception Type_error of string

(* Type environment; list of a pair of (name, type) *)
type tyenv = (string * ty) list

(* Return the type corresponding to variable x from type environment; type error if it's not there; recursive*)
let rec lookup_ty (env: tyenv) x =
  match env with
  | [] -> raise (Type_error ("Unbound variable in type environment: " ^ x))
  | (y,t)::rest -> if x = y then t else lookup_ty rest x

(* Determines the type of agiven expression within a type environment; recursive*)
  let rec typeof (env: tyenv) (e: expr) : ty =
  match e with
  | Int _ -> TInt
  | Bool _ -> TBool
  | Var x -> lookup_ty env x
  | Add (a,b) | Sub (a,b) ->
      let ta = typeof env a in
      let tb = typeof env b in
      begin match ta, tb with
      | TInt, TInt -> TInt
      | _ ->
          raise (Type_error ("Type error: + and - expect ints. Got " ^
                             string_of_ty ta ^ " and " ^ string_of_ty tb))
      end
  | Let (x, e1, e2) -> 
      let t1 = typeof env e1 in (* Find type of e1, add e1 to type environment, find type of e2 *)
      typeof ((x,t1)::env) e2
  | Lam (x, ty_param, body) ->
      (* parameter type is given; extend env *)
      let env' = (x, ty_param)::env in
      let tbody = typeof env' body in
      TFun (ty_param, tbody)
  | App (f,a) ->
      let tf = typeof env f in
      let ta = typeof env a in
      begin match tf with
      | TFun (tparam, tret) ->
          if tparam = ta then tret
          else raise (Type_error ("Argument type mismatch: expected " ^ string_of_ty tparam ^
                                  " but got " ^ string_of_ty ta))
      | _ -> raise (Type_error ("Type error: trying to call a non-function"))
      end
  | If (c,t,e) ->
      let tc = typeof env c in
      if tc <> TBool then raise (Type_error ("Type error: condition of if must be bool, got " ^ string_of_ty tc));
      let tt = typeof env t in
      let te = typeof env e in
      if tt = te then tt
      else raise (Type_error ("Type error: branches of if have different types: " ^
                              string_of_ty tt ^ " vs " ^ string_of_ty te))
