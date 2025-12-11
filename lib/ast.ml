(* Abstract Syntax Tree (AST) definition for a simply typed functional language *)

(* Expression definitions *)
type expr =
  | Int of int (* Integer *)
  | Bool of bool (* Boolean *)
  | Var of string (* Variable *)
  | Add of expr * expr (* Addition *)
  | Sub of expr * expr (* Subtraction *)
  | Let of string * expr * expr (* Let binding e.g. let x = 3 in x + 1 --> x / Int 3 / Add (Var x, Int 1) *)
  | Lam of string * Types.ty * expr (* Lambda abstraction e.g. fun (x : int) -> x + 1  --> Lam (x, TInt, Add (Var x, Int 1)) *)
  | App of expr * expr (* Function application e.g. f 3 --> App (Var f, Int 3) *)
  | If of expr * expr * expr (* If expression e.g. if cond then e1 else e2 --> If (Var cond, e1, e2) *)
  | LetAnn of string * Types.ty * expr * expr (* Let binding with type annotation.*)

(* Function to convert expressions to string for readability; recursive *)
let rec string_of_expr = function
  | Int n -> string_of_int n
  | Bool true -> "true"
  | Bool false -> "false"
  | Var x -> x
  | Add (a,b) -> "(" ^ string_of_expr a ^ " + " ^ string_of_expr b ^ ")"
  | Sub (a,b) -> "(" ^ string_of_expr a ^ " - " ^ string_of_expr b ^ ")"
  | Let (x, e1, e2) ->
      "let " ^ x ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
  | Lam (x, ty, body) ->
      "(fun (" ^ x ^ " : " ^ Types.string_of_ty ty ^ ") -> " ^ string_of_expr body ^ ")"
  | App (f,a) ->
      "(" ^ string_of_expr f ^ " " ^ string_of_expr a ^ ")"
  | If (c,t,e) ->
      "if " ^ string_of_expr c ^ " then " ^ string_of_expr t ^ " else " ^ string_of_expr e
  | LetAnn (x, ty, e1, e2) ->
    "let " ^ x ^ " : " ^ Types.string_of_ty ty ^ " = " ^
    string_of_expr e1 ^ " in " ^ string_of_expr e2