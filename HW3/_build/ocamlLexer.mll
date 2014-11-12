{

open Lexing

open OcamlParser

open OcamlType

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1 }

    
}

let var = ['a'-'z'] ['a'-'z' '0'-'9']*

rule token = parse
  | '\'' 	{ SQUOTE }
  | '(' 	{ LPAREN }
  | ')' 	{ RPAREN }
  | ',' 	{ COMMA }
  | '*' 	{ STAR }
  | '['   { LBRACKET }
  | ']'   { RBRACKET }
  | ';'   { SMCOLON }
  | var 	{ IDT Lexing.lexeme }
  | "->"	{ ARROW }
  | "int"	{ INT }
  | "bool"	{ BOOL }
  | eof { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
