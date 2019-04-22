%{
%}

/* Tokens */

%token EQ SEQ
%token EOF
%token<string> STR

%start ss
%type <(string * string * bool) list> ss

%%

ss:
  | ss s { $2::$1 }
  | s    { [$1] }
  ;

s:
  | STR EQ STR { $1, $3, true }
  | STR SEQ STR { $1, $3, false }
  ;