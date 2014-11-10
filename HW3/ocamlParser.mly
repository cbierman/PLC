%{

open OcamlType
  
%}
%token SQUOTE LPAREN RPAREN COMMA STAR
%token ARROW INT BOOL
%token IDENT
%token EOF

%start <OcamlType.otype> main
%right ARROW
%%

main: 
	| e = typexpr EOF { e }
	| { raise Missing }

typexpr:
	(* Do we need to even use the "base case" ones since type figures it out for you and you could potentially have nested cases in the base cases? *)
	| SQUOTE v = ident { TVar ( v )}
	| INT { Int }
	| BOOL { Bool }
	| LPAREN e = typexpr RPAREN { e }
	| e1 = typexpr ARROW e2 = typexpr { Arrow ( e1, e2) }
	| e1 = typexpr STAR e2 = typexpr { Pair ( e1, e2) }
	| e = typeconstr { e }
	| e1 = typexpr e2 = typeconstr { Type (e1, e2) }
	| LPAREN e1 = typexpr RPAREN e2 = typeconstr { Type (e1, e2)}
	| LPAREN e1 = typexpr COMMA e2 = typexpr RPAREN e3 = typeconstr { Type ( Type(e1,e2), e3) }

typeconstr:
	| e = ident { e }
	| {raise Missing}

ident:
	| e = IDENT { Tvar (e) }