%{

open OcamlType
  
%}

%token EOF

%start <OcamlType.otype> main
%%

main: { raise Missing }
