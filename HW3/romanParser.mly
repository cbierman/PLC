
%token EOF
%token ONE FIVE TEN HUNDRED THOUSAND
%token FIFTY FVHUNDRED 
%start <int> roman
%%

roman:
  | thousnds hundrds tens units EOF { $1+$2+$3+$4 }
  

thousnds:
	| THOUSAND thousnds { 1000 + $2; }
	| { 0; }

leth300:
	| HUNDRED 			{ 100; }
	| HUNDRED HUNDRED 		{ 200; }
	| HUNDRED HUNDRED HUNDRED 	{ 300; }
	| 				{ 0; }

hundrds:
	| leth300 			{ $1; }
	| HUNDRED FVHUNDRED 		{ 400; }
	| HUNDRED THOUSAND 		{ 900; }
	| FVHUNDRED leth300 		{ 500 + $2; }

leth30:
	| TEN 				{ 10; }
	| TEN TEN 			{ 20; }
	| TEN TEN TEN 			{ 30; }
	| { 0; }

tens:
	| leth30 			{ $1; }
	| TEN FIFTY  			{ 40; }
	| TEN HUNDRED 			{ 90; }
	| FIFTY leth30 			{ 50 + $2; }

leth3:
	| ONE 				{ 1; }
	| ONE ONE 			{ 2; }
	| ONE ONE ONE 			{ 3; }
	| 				{ 0; }

units:
	| leth3 { $1; }
	| ONE FIVE { 4; }
	| FIVE leth3 { 5 + $2; }
	| ONE TEN { 9; }
