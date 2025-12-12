open Ast
open Types

exception Parse_error of string

type token =
  | INT of int
  | IDENT of string (* identifiers *)
  | LET | IN | FUN
  | IF | THEN | ELSE
  | ARROW (* -> *)
  | COLON (* : *)
  | EQ (* = *)
  | LPAREN | RPAREN (* ( | )*)
  | PLUS | MINUS (* + | - *)
  | TRUE | FALSE
  | AT (* @ *)
  | EOF

(* Checks if a character starts with an identifier *)
let is_ident_start = function
  | 'a'..'z' | 'A'..'Z' -> true
  | '_' -> true
  | _ -> false

let is_ident_char c =
  is_ident_start c || (c >= '0' && c <= '9')

let tokenize (s : string) : token list =
  let len = String.length s in
  (* Inner helper that skips white space *)
  let rec skip_ws i =
    if i >= len then i
    else match s.[i] with
    | ' ' | '\t' | '\n' | '\r' -> skip_ws (i+1)
    | _ -> i
  in
  (* Inner helper that reads numbers *)
  let rec number i acc =
    if i < len then
      let c = s.[i] in
      if c >= '0' && c <= '9' then number (i+1) (acc * 10 + (Char.code c - Char.code '0'))
      else (i, acc)
    else (i, acc)
  in
  (* Inner helper that reads identifier characters *)
  let rec ident i acc =
    if i < len && is_ident_char s.[i] then ident (i+1) (acc ^ (String.make 1 s.[i]))
    else (i, acc)
  in
  let rec aux i acc =
    let i = skip_ws i in
    if i >= len then List.rev (EOF :: acc)
    else
      match s.[i] with
      | '(' -> aux (i+1) (LPAREN :: acc)
      | ')' -> aux (i+1) (RPAREN :: acc)
      | '+' -> aux (i+1) (PLUS :: acc)
      | '-' ->
          if i+1 < len && s.[i+1] = '>' then aux (i+2) (ARROW :: acc)
          else aux (i+1) (MINUS :: acc)
      | ':' -> aux (i+1) (COLON :: acc)
      | '=' -> aux (i+1) (EQ :: acc)
      | '@' -> aux (i+1) (AT :: acc)
      | c when (c >= '0' && c <= '9') ->
          let (j, n) = number i 0 in
          aux j (INT n :: acc)
      | c when is_ident_start c ->
          let (j, id) = ident (i+1) (String.make 1 c) in
          let tok =
            match id with
            | "let" -> LET
            | "in" -> IN
            | "fun" -> FUN
            | "if" -> IF
            | "then" -> THEN
            | "else" -> ELSE
            | "true" -> TRUE
            | "false" -> FALSE
            | _ -> IDENT id
          in
          aux j (tok :: acc)
      | c ->
          raise (Parse_error (Printf.sprintf "Unexpected character: '%c' at pos %d" c i))
  in
  aux 0 []
;;

(* Returns parsed value of type 'a and remaining tokens given a token list *)
type 'a parser = token list -> ('a * token list)

(* Check if next token is tok *)
let expect tok tokens =
  match tokens with
  | [] -> raise (Parse_error "Unexpected end of input")
  | t::ts -> if t = tok then ts else raise (Parse_error "Unexpected token during expect")

(* Parse type expression *)
let rec parse_type tokens =
  let (t1, rest) = parse_atomic_type tokens in
  parse_type_tail t1 rest

(* If next token is an arrow, parse the right hand side type, build TFun *)
and parse_type_tail tleft tokens =
  match tokens with
  | ARROW :: rest ->
      let (tright, rest') = parse_type rest in
      (TFun (tleft, tright), rest')
  | _ -> (tleft, tokens)

and parse_atomic_type tokens =
  match tokens with
  | IDENT "int" :: AT :: IDENT label :: rest ->
      let l = (match label with "public" -> Public | "secret" -> Secret | _ -> raise (Parse_error ("Unknown label: " ^ label))) in
      (TInt l, rest)
  | IDENT "bool" :: AT :: IDENT label :: rest ->
      let l = (match label with "public" -> Public | "secret" -> Secret | _ -> raise (Parse_error ("Unknown label: " ^ label))) in
      (TBool l, rest)
  | IDENT "int" :: rest -> (TInt Public, rest) (* default: public *)
  | IDENT "bool" :: rest -> (TBool Public, rest) (* default: public *)
  | LPAREN :: rest ->
      let (t, rest') = parse_type rest in
      (match rest' with
       | RPAREN :: rest'' -> (t, rest'')
       | _ -> raise (Parse_error "Expected ')' in type"))
  | _ -> raise (Parse_error "Unexpected token in type")

let rec parse_expr tokens = parse_nonseq tokens

and parse_nonseq tokens =
  parse_let tokens

(* Parse let tokens (let, letann) *)
and parse_let tokens =
  match tokens with
  | LET :: IDENT x :: COLON :: _ ->
      let (ty_x, rest_after_ty) = parse_type (List.tl (List.tl (List.tl tokens))) in
      (match rest_after_ty with
       | EQ :: rest_after_eq ->
           let (e1, rest1) = parse_expr rest_after_eq in
           (match rest1 with
            | IN :: rest2 ->
                let (e2, rest3) = parse_expr rest2 in
                (LetAnn (x, ty_x, e1, e2), rest3)
            | _ -> raise (Parse_error "Expected 'in' after annotated let"))
       | _ -> raise (Parse_error "Expected '=' after annotated let type"))
  | LET :: IDENT x :: EQ :: rest ->
      let (e1, rest1) = parse_expr rest in
      (match rest1 with
       | IN :: rest2 ->
           let (e2, rest3) = parse_expr rest2 in
           (Let (x, e1, e2), rest3)
       | _ -> raise (Parse_error "Expected 'in' after let"))
  | _ -> parse_lambda tokens

and parse_lambda tokens =
  match tokens with
  | FUN :: LPAREN :: IDENT x :: COLON :: _ ->
      let rec drop_n n lst =
        match n, lst with
        | 0, _ -> lst
        | _, [] -> []
        | n, _::tl -> drop_n (n-1) tl
      in
      let tokens_after_param = drop_n 4 tokens in
      let (param_ty, rest) = parse_type tokens_after_param in
      (match rest with
       | RPAREN :: ARROW :: rest2 ->
           let (body, rest3) = parse_expr rest2 in
           (Lam (x, param_ty, body), rest3)
       | _ -> raise (Parse_error "Malformed lambda with annotated param"))
  | FUN :: IDENT _ :: ARROW :: _ ->
    raise (Parse_error "Lambda parameters must include a type annotation: fun (x : <type>) -> ...")
  | _ -> parse_if tokens

and parse_if tokens =
  match tokens with
  | IF :: rest ->
      let (cond, rest1) = parse_expr rest in
      (match rest1 with
       | THEN :: rest2 ->
           let (ethen, rest3) = parse_expr rest2 in
           (match rest3 with
            | ELSE :: rest4 ->
                let (eelse, rest5) = parse_expr rest4 in
                (If (cond, ethen, eelse), rest5)
            | _ -> raise (Parse_error "Expected 'else' in if expression"))
       | _ -> raise (Parse_error "Expected 'then' in if expression"))
  | _ -> parse_sum tokens

and parse_sum tokens =
  let (tleft, rest) = parse_app tokens in
  parse_sum_tail tleft rest

and parse_sum_tail left tokens =
  match tokens with
  | PLUS :: rest ->
      let (right, rest2) = parse_app rest in
      parse_sum_tail (Add (left, right)) rest2
  | MINUS :: rest ->
      let (right, rest2) = parse_app rest in
      parse_sum_tail (Sub (left, right)) rest2
  | _ -> (left, tokens)

and parse_app tokens =
  let (first, rest) = parse_atom tokens in
  parse_app_tail first rest

and parse_app_tail func tokens =
  match tokens with
  | INT _ :: _ | IDENT _ :: _ | LPAREN :: _ | FUN :: _ | IF :: _ | TRUE :: _ | FALSE :: _ ->
      let (arg, rest) = parse_atom tokens in
      parse_app_tail (App (func, arg)) rest
  | _ -> (func, tokens)

and parse_atom tokens =
  match tokens with
  | INT n :: rest -> (Int n, rest)
  | TRUE :: rest -> (Bool true, rest)
  | FALSE :: rest -> (Bool false, rest)
  | IDENT id :: rest -> (Var id, rest)
  | LPAREN :: rest ->
      let (e, rest1) = parse_expr rest in
      (match rest1 with
       | RPAREN :: rest2 -> (e, rest2)
       | _ -> raise (Parse_error "Expected ')'"))
  | _ -> raise (Parse_error "Unexpected token in expression")

let parse_string (s : string) : expr =
  let toks = tokenize s in
  match parse_expr toks with
  | (e, remaining) ->
      (match remaining with
       | [] -> e
       | [EOF] -> e
       | EOF :: _ -> e
       | _ -> raise (Parse_error "Unexpected tokens after end of expression"))
