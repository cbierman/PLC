%{

open OcamlType
  
%}
%token SQUOTE LPAREN RPAREN COMMA STAR
%token ARROW INT BOOL
%token <string> IDENT
%token EOF

%start <OcamlType.otype> main
%%

main: 
	| e = typexpr EOF { e }
	| { raise Missing }

typexpr:
	| SQUOTE v = ident { TVar v }
	| INT { Int }
	| BOOL { Bool }
	| LPAREN e = typexpr RPAREN { e }
	| e1 = typexpr ARROW e2 = typexpr { Arrow ( e1, e2) }
	| e1 = typexpr STAR e2 = typexpr { Pair ( e1, e2) }
	| e = typeconstr { TVar( e ) }
	| e1 = typexpr e2 = typeconstr { Type (e2, [e1]) }
	| LPAREN e1 = typexpr RPAREN e2 = typeconstr { Type (e2, [e1]) }
	| LPAREN e1 = typexpr COMMA e2 = typexpr RPAREN e3 = typeconstr { Type ( e3, [e1; e2] ) }

typeconstr:
	| e = ident { e }
	| { raise Missing }

ident:
	| e = IDENT { TVar e  }