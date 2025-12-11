type label = Public | Secret

let string_of_label = function
  | Public -> "public"
  | Secret -> "secret"

let join_label l1 l2 =
  match (l1, l2) with
  | (Secret, _) | (_, Secret) -> Secret
  | (Public, Public) -> Public

let leq_label l1 l2 =
  match (l1, l2) with
  | (Public, Public) -> true
  | (Public, Secret) -> true
  | (Secret, Secret) -> true
  | (Secret, Public) -> false

type ty =
  | TInt of label
  | TBool of label
  | TFun of ty * ty

let rec label_of_ty = function
  | TInt l -> l
  | TBool l -> l
  | TFun (_param, ret) -> label_of_ty ret

let string_of_ty ty =
  let rec aux = function
    | TInt l -> "int@" ^ string_of_label l
    | TBool l -> "bool@" ^ string_of_label l
    | TFun (a, b) ->
        let sa =
          match a with
          | TFun _ -> "(" ^ aux a ^ ")"
          | _ -> aux a
        in
        sa ^ " -> " ^ aux b
  in
  aux ty
