{

open Lexing

open OcamlParser

open OcamlType

exception SyntaxError of string
    
}

let var = ['a'-'z'] | ['a'-'z' '0'-'9']*

rule token = parse
  | '\'' 	{ SQUOTE }
  | '(' 	{ LPAREN }
  | ')' 	{ RPAREN }
  | ',' 	{ COMMA }
  | '*' 	{ STAR }
  | var 	{ IDENT }
  | "->"	{ ARROW }
  | "int"	{ INT }
  | "bool"	{ BOOL }
  | eof { EOF }
  | _ { raise Missing }
