%{
%}

/* Tokens */

%token EQ SEQ COMMA
%token EOF
%token<string> STR

%start ss
%type <(string * string * bool * string option) list> ss

%start is
%type <string list> is

%%

ss:
  | ss s { $2::$1 }
  | s    { [$1] }
  ;

s:
  | STR EQ STR { $1, $3, true, None }
  | STR EQ STR COMMA STR { $1, $3, true, Some $5 }
  | STR SEQ STR { $1, $3, false, None }
  | STR SEQ STR COMMA STR { $1, $3, false, Some $5 }
  ;

is:
  | is i { $2::$1 }
  | i    { [$1] }
  ;

i:
  | STR  { $1 }
  ;
