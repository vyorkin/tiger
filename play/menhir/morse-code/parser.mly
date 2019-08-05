%token <string> CODE
%token SEP
%token EOL

%start <string> main

%%

let main :=
  ~ = separated_nonempty_list(SEP, CODE); EOL; <Morse.decode>
