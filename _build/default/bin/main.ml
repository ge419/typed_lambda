open Typed_lambda.Ast

let example = Add (Int 3, Sub (Int 5, Int 2))

let () =
  Printf.printf "The expression is: %s\n" (string_of_expr example)