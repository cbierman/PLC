exception Error

type token = 
  | STAR
  | SQUOTE
  | RPAREN
  | LPAREN
  | INT
  | IDT of (string)
  | EOF
  | COMMA
  | BOOL
  | ARROW


val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (OcamlType.otype)