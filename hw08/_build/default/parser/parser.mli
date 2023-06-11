
(* The type of tokens. *)

type token = 
  | RIGHT_PARENTHESIS
  | PLUS
  | NUMBER of (int)
  | NEG
  | MINUS
  | LEFT_PARENTHESIS
  | KW_LET
  | KW_IN
  | ID of (string)
  | EQ
  | EOF

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val parse: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)
