%{
%}

/* Tokens */

%token EQ SEQ
%token EOF
%token<string> STR

%start ss
%type <(string * string * bool) list> ss

%start is
%type <string list> is

%%

ss:
  | ss s { $2::$1 }
  | s    { [$1] }
  ;

s:
  | STR EQ STR { $1, $3, true }
  | STR SEQ STR { $1, $3, false }
  ;

is:
  | is i { $2::$1 }
  | i    { [$1] }
  ;

i:
  | STR  { $1 }
  ;
