{

open Lexing

open RomanParser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1 }


}

let white = [' ' '\t']+

let newline = '\r' | '\n' | "\r\n"

rule token = parse 
  | 'I' {ONE}
  | 'V' {FIVE}
  | 'X' {TEN}
  | 'L' {FIFTY}
  | 'C' {HUNDRED}
  | 'D' {FVHUNDRED}
  | 'M' {THOUSAND}
  | newline {EOF}
  | white {EOF}
  | eof { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
