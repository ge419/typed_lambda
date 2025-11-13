type expr = 
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
let rec string_of_expr = function
  | Int n -> string_of_int n
  | Add (e1, e2) -> "(" ^ string_of_expr e1 ^ " + " ^ string_of_expr e2 ^ ")"
  | Sub (e1, e2) -> "(" ^ string_of_expr e1 ^ " - " ^ string_of_expr e2 ^ ")"