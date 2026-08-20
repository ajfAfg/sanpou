%{
open Json_ast
%}

%token <string> STRING
%token <int> INT
%token TRUE FALSE
%token LBRACE RBRACE LBRACKET RBRACKET
%token COLON COMMA
%token EOF

%start <Json_ast.t> json

(* Menhir type inference is disabled (see dune), so every symbol — including
   the standard-library instances — is typed by hand. *)
%type <Json_ast.t> value
%type <Json_ast.t list> separated_nonempty_list(COMMA, value)
%type <Json_ast.t list> loption(separated_nonempty_list(COMMA, value))
%type <string * Json_ast.t> field
%type <(string * Json_ast.t) list> separated_nonempty_list(COMMA, field)
%type <(string * Json_ast.t) list> loption(separated_nonempty_list(COMMA, field))
%%

json:
  | v=value EOF { v }

value:
  | s=STRING { String s }
  | i=INT { Int i }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | LBRACKET elems=separated_list(COMMA, value) RBRACKET { Array elems }
  | LBRACE fields=separated_list(COMMA, field) RBRACE { Object fields }

field:
  | key=STRING COLON v=value { (key, v) }
