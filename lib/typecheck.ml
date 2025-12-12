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

(* Check for same shape *)
let rec same_shape t1 t2 =
  match (t1, t2) with
  | (TInt _, TInt _) -> true
  | (TBool _, TBool _) -> true
  | (TFun (p1, r1), TFun (p2, r2)) -> same_shape p1 p2 && same_shape r1 r2
  | _ -> false

(* Join two types in terms of their labels *)
let rec join_ty t1 t2 =
  match (t1, t2) with
  | (TInt l1, TInt l2) -> TInt (join_label l1 l2)
  | (TBool l1, TBool l2) -> TBool (join_label l1 l2)
  | (TFun (p1, r1), TFun (p2, r2)) -> TFun (join_ty p1 p2, join_ty r1 r2)
  | _ ->
      raise (Type_error ("Cannot join incompatible types: " ^ string_of_ty t1 ^ " and " ^ string_of_ty t2))

(* Extract label from type *)
let rec set_label_on_ty ty lbl =
  match ty with
  | TInt _ -> TInt lbl
  | TBool _ -> TBool lbl
  | TFun (p, r) -> TFun (set_label_on_ty p lbl, set_label_on_ty r lbl)

(* Check if argument type flows to parameter type *)
let arg_flows_to_param arg param =
  if not (same_shape arg param) then false
  else leq_label (label_of_ty arg) (label_of_ty param)

(* Determines the type of agiven expression within a type environment; recursive*)
let rec typeof (env: tyenv) (e: expr) : ty =
  match e with
  | Int _ -> TInt Public
  | Bool _ -> TBool Public
  | Var x -> lookup_ty env x
  | Add (a,b) | Sub (a,b) ->
      let ta = typeof env a in
      let tb = typeof env b in
      begin match (ta, tb) with
      | (TInt l1, TInt l2) ->
          TInt (join_label l1 l2)
      | _ ->
          raise (Type_error ("Type error: + and - expect ints. Got " ^
                             string_of_ty ta ^ " and " ^ string_of_ty tb))
      end
  | Let (x, e1, e2) ->
      let t1 = typeof env e1 in
      typeof ((x, t1) :: env) e2
  | Lam (x, ty_param, body) ->
      let env' = (x, ty_param) :: env in
      let tbody = typeof env' body in
      TFun (ty_param, tbody)
  | App (f, a) ->
      let tf = typeof env f in
      let ta = typeof env a in
      begin match tf with
      | TFun (tparam, tret) ->
          if not (same_shape tparam ta) then
            raise (Type_error ("Argument shape mismatch: expected " ^ string_of_ty tparam ^
                               " but got " ^ string_of_ty ta))
          else if not (arg_flows_to_param ta tparam) then
            raise (Type_error ("Argument label flow violation: expected argument that flows to " ^
                               string_of_ty tparam ^ " but got " ^ string_of_ty ta))
          else
            tret
      | _ ->
          raise (Type_error ("Type error: trying to call a non-function: " ^ string_of_ty tf))
      end
  | If (c, t, e_else) ->
      (* Typecheck condition and extract its label *)
      let tc = typeof env c in
      let cond_label =
        match tc with
        | TBool l -> l
        | _ -> raise (Type_error ("Type error: condition of if must be bool, got " ^ string_of_ty tc))
      in
      let tt = typeof env t in
      let te = typeof env e_else in
      if same_shape tt te then
        let joined = join_ty tt te in
        let final = set_label_on_ty joined (join_label (label_of_ty joined) cond_label) in
        final
      else
        raise (Type_error ("Type error: branches of if have different shapes: " ^
                           string_of_ty tt ^ " vs " ^ string_of_ty te))
  | LetAnn (x, declared_ty, e1, e2) ->
    let inferred_ty = typeof env e1 in
    if not (same_shape declared_ty inferred_ty) then
      raise (Type_error "LetAnn shape mismatch");
    if not (leq_label (label_of_ty inferred_ty) (label_of_ty declared_ty)) then
      raise (Type_error "Security label violation in LetAnn");
    typeof ((x, declared_ty) :: env) e2
