{

open Lexing

open ExprParser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1 }

}

let int = '-'? ['0'-'9']+ 

let var = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let white = [' ' '\t']+

let newline = '\r' | '\n' | "\r\n"

rule token = 

  parse

  | '('     { LPAREN }

  | ')'     { RPAREN }

  | "let"   { LET }

  | '='     { EQUALS }

  | "in"    { IN }

  | "end"   { END }

  | '+'     { PLUS }

  | '*'     { TIMES }

  | white   { token lexbuf }

  | newline { next_line lexbuf; token lexbuf }

  | var     { VAR (Lexing.lexeme lexbuf) }
  
  | int     { INT (int_of_string (Lexing.lexeme lexbuf)) }

  | eof     { EOF }

  | _       { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

