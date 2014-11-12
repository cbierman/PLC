%{

open OcamlType
  
%}

%token SQUOTE LPAREN RPAREN COMMA STAR
%token ARROW INT BOOL
%token <string>IDT
%token EOF

%start <OcamlType.otype> main
%right IDT
%right ARROW
%right STAR
%right COMMA

%%

main: 
	| e = typexpr EOF { e }

typexpr:
	| SQUOTE v = ident { TVar v }
	| INT { Int }
	| BOOL { Bool }
	| LPAREN e = typexpr RPAREN { e }
	| e1 = typexpr ARROW e2 = typexpr { Arrow ( e1, e2) }
	| e1 = typexpr STAR e2 = typexpr { Pair ( e1, e2) }
	| e = typeconstr { TVar( e ) }
	| e1 = typexpr e2 = typeconstr { Type (e2, [e1]) }
	
	| LPAREN e1 = l RPAREN e3 = typeconstr { Type ( e3,  [e1] ) }


typeconstr:
	| e = ident { e }

ident:
	| e = IDT { e }

l:
	| t = elements { t }
	| e = typexpr { e }

elements:
	| e = l { e }
	| e1 = l COMMA e2 = elements { Type ( e1, [e2]) }

