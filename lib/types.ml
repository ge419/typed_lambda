(* Type system definition *)

(* Security labels *)
type label = Public | Secret

(* Function to convert labels to string for readability *)
let string_of_label = function
  | Public -> "public"
  | Secret -> "secret"

(* Function to join two labels; Secret dominates Public *)
let join_label l1 l2 =
  match (l1, l2) with
  | (Secret, _) | (_, Secret) -> Secret
  | (Public, Public) -> Public

(* Security label order *)
let leq_label l1 l2 =
  match l1, l2 with
  | Public, Public -> true
  | Public, Secret -> true
  | Secret, Secret -> true
  | Secret, Public -> false

(* Defines types*)
type ty =
  | TInt (* Type of integers *)
  | TBool (* Type of booleans *)
  | TFun of ty * ty (* Function type from one type to another *)

(* type ty = 
  | TInt of label (* Type of integers with security label *)
  | TBool of label (* Type of booleans with security label *)
  | TFun of ty * ty (* Function type from one type to another *) *)

(* Function to convert types to string for readability; recursive *)
let rec string_of_ty = function
  | TInt -> "int"
  | TBool -> "bool"
  | TFun (a,b) ->
      let str_a = match a with
        | TFun _ -> "(" ^ string_of_ty a ^ ")"
        | _ -> string_of_ty a
      in
      str_a ^ " -> " ^ string_of_ty b

(* let rec label_of_ty = function
  | TInt l -> l
  | TBool l -> l
  | TFun (_param, ret) -> label_of_ty ret *)

  (* let rec string_of_ty = function
  | TInt -> "int@" ^ string_of_label l
  | TBool -> "bool@" ^ string_of_label l
  | TFun (a,b) ->
      let str_a = match a with
        | TFun _ -> "(" ^ string_of_ty a ^ ")"
        | _ -> string_of_ty a
      in
      str_a ^ " -> " ^ string_of_ty b *)
