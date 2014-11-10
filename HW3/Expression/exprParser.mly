%{

open Expr

%}

%token <int> INT
%token <string> VAR
%token LPAREN
%token RPAREN
%token LET
%token EQUALS
%token IN
%token END
%token PLUS
%token TIMES
%token EOF

%left PLUS 
%left TIMES

%start <Expr.expr> main
%%

main:

  (* S -> E EOF *)
  | e = expr EOF { e }

expr:

  (* E -> LET VAR EQUALS E IN E END *)
  | LET v = VAR EQUALS e1 = expr IN e2 = expr END { Let (v, e1, e2) }

  (* E -> E PLUS E *)
  | e1 = expr PLUS e2 = expr { Plus (e1, e2) } 

  (* E -> E TIMES E *)
  | e1 = expr TIMES e2 = expr { Times (e1, e2) } 

  (* E -> LPAREN E RPAREN *)
  | LPAREN e = expr RPAREN { e }

  (* E -> INT *)
  | i = INT { Int i }

  (* E -> VAR *)
  | v = VAR { Var v }
