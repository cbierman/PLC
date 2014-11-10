{

open Lexing

open OcamlParser

open OcamlType

exception SyntaxError of string
    
}

rule token = parse
  | eof { EOF }
  | _ { raise Missing }
