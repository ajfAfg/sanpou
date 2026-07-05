%{
open Generic_ast

let loc_of_pos (pos : Lexing.position) : loc =
  { line = pos.pos_lnum; col = pos.pos_cnum - pos.pos_bol + 1 }

let mk pos desc = { desc; loc = loc_of_pos pos }
%}

%token <int> INTV
%token <string> ID
%token TRUE FALSE
%token DEF VAR FN MOD FAIR PROCESS IN SELF
%token WHILE IF ELSE RETURN BREAK CONTINUE AWAIT FORALL EXISTS EITHER OR WITH
%token PLUS MINUS MULT DIV PERCENT NOT LT GT LTEQ GTEQ EQ EQEQ NEQ ANDAND OROR
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token SEMI COMMA DOTDOT ARROW
%token EOF

(* Precedence is encoded structurally in the stratified expr rules
   (or_expr > and_expr > comparison_expr > add_expr > mult_expr), so no
   %left declarations are needed. *)

%start <Surface_ast.program> program

(* Menhir type inference is disabled (see dune), so every symbol — including
   the standard-library instances — is typed by hand. *)
%type <Surface_ast.module_def list> list(module_def)
%type <Surface_ast.module_def> module_def
%type <Surface_ast.item list> list(item)
%type <Surface_ast.item> item
%type <Surface_ast.body> body list(step) else_clause or_arm
%type <Surface_ast.body list> nonempty_list(or_arm)
%type <Surface_ast.body option> option(else_clause)
%type <Surface_ast.step> step
%type <Surface_ast.simple_stmt> simple_stmt
%type <Surface_ast.simple_stmt list> separated_nonempty_list(COMMA, simple_stmt)
%type <Surface_ast.assign_target> assign_target
%type <Surface_ast.block_stmt> block_stmt if_stmt
%type <Surface_ast.expr> expr or_expr and_expr comparison_expr add_expr
%type <Surface_ast.expr> mult_expr postfix_expr primary_expr subscript_index
%type <Surface_ast.expr list> nonempty_list(subscript_index)
%type <Surface_ast.expr list> separated_nonempty_list(COMMA, expr)
%type <Surface_ast.expr list> loption(separated_nonempty_list(COMMA, expr))
%type <Generic_ast.id list> separated_nonempty_list(COMMA, ID)
%type <Generic_ast.id list> loption(separated_nonempty_list(COMMA, ID))
%type <Generic_ast.fairness> fairness_marker
%%

program:
  | modules=list(module_def) EOF { modules }

module_def:
  | MOD mod_name=ID LBRACE items=list(item) RBRACE
      { { mod_name; items; mod_loc = loc_of_pos $startpos } }

item:
  | DEF name=ID EQ value=expr SEMI
      { mk $startpos (ConstDef { name; value }) }
  | DEF name=ID LPAREN params=separated_list(COMMA, ID) RPAREN EQ body_expr=expr SEMI
      { mk $startpos (FunDef { name; params; body_expr }) }
  | VAR name=ID EQ value=expr SEMI
      { mk $startpos (VarDecl { name; init = InitValue value }) }
  | VAR name=ID IN lo=expr DOTDOT hi=expr SEMI
      { mk $startpos (VarDecl { name; init = InitRange (lo, hi) }) }
  | FN name=ID LPAREN params=separated_list(COMMA, ID) RPAREN LBRACE body=body RBRACE
      { mk $startpos (ProcDef { name; params; body }) }
  | fairness=fairness_marker PROCESS name=ID EQ proc=ID IN lo=expr DOTDOT hi=expr SEMI
      { mk $startpos (Process { name; proc; fairness; lo; hi }) }

(* `fair+` lexes as FAIR PLUS; there is no dedicated token. *)
fairness_marker:
  | (* empty *) { Unfair }
  | FAIR { WeakFair }
  | FAIR PLUS { StrongFair }

body:
  | steps=list(step) { steps }

step:
  | stmts=separated_nonempty_list(COMMA, simple_stmt) SEMI
      { mk $startpos (SimpleStep stmts) }
  | SEMI
      { mk $startpos EmptyStep }
  | stmt=block_stmt
      { mk $startpos (BlockStep stmt) }
  | VAR name=ID EQ value=expr SEMI
      { mk $startpos (VarStep (name, value)) }
  (* The body is a single atomic step: simple statements only, so the
     chosen binder never has to survive past one action. *)
  | WITH LPAREN binder=ID IN lo=expr DOTDOT hi=expr RPAREN
      LBRACE stmts=separated_nonempty_list(COMMA, simple_stmt) SEMI RBRACE
      { mk $startpos (WithStep { binder; lo; hi; stmts }) }

simple_stmt:
  | target=assign_target EQ value=expr
      { mk $startpos (Assign (target, value)) }
  | name=ID LPAREN args=separated_list(COMMA, expr) RPAREN
      { mk $startpos (Call (name, args)) }
  | RETURN value=expr
      { mk $startpos (Return value) }
  | BREAK
      { mk $startpos Break }
  | CONTINUE
      { mk $startpos Continue }
  | AWAIT cond=expr
      { mk $startpos (Await cond) }

assign_target:
  | name=ID
      { VarTarget name }
  | name=ID indices=nonempty_list(subscript_index)
      { SubscriptTarget (name, indices) }

subscript_index:
  | LBRACKET index=expr RBRACKET { index }

block_stmt:
  | WHILE LPAREN cond=expr RPAREN LBRACE body=body RBRACE
      { While { cond; body } }
  | s=if_stmt { s }
  | EITHER LBRACE first=body RBRACE rest=nonempty_list(or_arm)
      { Either (first :: rest) }

or_arm:
  | OR LBRACE body=body RBRACE { body }

if_stmt:
  | IF LPAREN cond=expr RPAREN LBRACE body=body RBRACE else_body=option(else_clause)
      { If { cond; body; else_body } }

(* [else if] desugars to an else body holding the nested if as its only
   step, so downstream passes only ever see plain if/else. *)
else_clause:
  | ELSE LBRACE body=body RBRACE { body }
  | ELSE s=if_stmt { [ mk $startpos(s) (BlockStep s) ] }

expr:
  | e=or_expr { e }

or_expr:
  | e=and_expr { e }
  | e1=or_expr OROR e2=and_expr { mk $startpos (BinOp (Or, e1, e2)) }

and_expr:
  | e=comparison_expr { e }
  | e1=and_expr ANDAND e2=comparison_expr { mk $startpos (BinOp (And, e1, e2)) }

comparison_expr:
  | e=add_expr { e }
  | e1=comparison_expr LT e2=add_expr { mk $startpos (BinOp (Lt, e1, e2)) }
  | e1=comparison_expr GT e2=add_expr { mk $startpos (BinOp (Gt, e1, e2)) }
  | e1=comparison_expr EQEQ e2=add_expr { mk $startpos (BinOp (Eq, e1, e2)) }
  | e1=comparison_expr NEQ e2=add_expr { mk $startpos (BinOp (Neq, e1, e2)) }
  | e1=comparison_expr LTEQ e2=add_expr { mk $startpos (BinOp (LtEq, e1, e2)) }
  | e1=comparison_expr GTEQ e2=add_expr { mk $startpos (BinOp (GtEq, e1, e2)) }

add_expr:
  | e=mult_expr { e }
  | e1=add_expr PLUS e2=mult_expr { mk $startpos (BinOp (Plus, e1, e2)) }
  | e1=add_expr MINUS e2=mult_expr { mk $startpos (BinOp (Minus, e1, e2)) }

mult_expr:
  | e=postfix_expr { e }
  | e1=mult_expr MULT e2=postfix_expr { mk $startpos (BinOp (Mult, e1, e2)) }
  | e1=mult_expr DIV e2=postfix_expr { mk $startpos (BinOp (Div, e1, e2)) }
  | e1=mult_expr PERCENT e2=postfix_expr { mk $startpos (BinOp (Mod, e1, e2)) }

postfix_expr:
  | e=primary_expr { e }
  | lhs=postfix_expr LBRACKET index=expr RBRACKET
      { mk $startpos (Subscript (lhs, index)) }

primary_expr:
  | MINUS rhs=primary_expr { mk $startpos (UnOp (Neg, rhs)) }
  | NOT rhs=primary_expr { mk $startpos (UnOp (Not, rhs)) }
  | IF LPAREN cond=expr RPAREN LBRACE then_e=expr RBRACE
      ELSE LBRACE else_e=expr RBRACE
      { mk $startpos (IfExpr (cond, then_e, else_e)) }
  (* The braces delimit the body, so a quantifier composes as an ordinary
     primary expression — unlike TLA+'s \A/\E, whose bodies extend as far
     right as possible. *)
  | FORALL LPAREN binder=ID IN lo=expr DOTDOT hi=expr RPAREN
      LBRACE body=expr RBRACE
      { mk $startpos (Quant { quant = Forall; binder; lo; hi; body }) }
  | EXISTS LPAREN binder=ID IN lo=expr DOTDOT hi=expr RPAREN
      LBRACE body=expr RBRACE
      { mk $startpos (Quant { quant = Exists; binder; lo; hi; body }) }
  | value=INTV { mk $startpos (IntLit value) }
  | TRUE { mk $startpos (BoolLit true) }
  | FALSE { mk $startpos (BoolLit false) }
  | SELF { mk $startpos Self }
  | name=ID LPAREN args=separated_list(COMMA, expr) RPAREN
      { mk $startpos
          (match Builtin.of_name name with
          | Some b -> Builtin (b, args)
          | None -> App (name, args)) }
  | name=ID { mk $startpos (Var name) }
  (* Mirrors the map type's notation ({int -> t}). The arrow separator
     leaves `{ x in S : p }` free for a future set filter form. *)
  | LBRACE binder=ID IN lo=expr DOTDOT hi=expr ARROW value=expr RBRACE
      { mk $startpos (MapInit { binder; lo; hi; value }) }
  | LPAREN RPAREN
      { mk $startpos (Tuple []) }
  | LPAREN e=expr COMMA RPAREN
      { mk $startpos (Tuple [ e ]) }
  | LPAREN e1=expr COMMA rest=separated_nonempty_list(COMMA, expr) RPAREN
      { mk $startpos (Tuple (e1 :: rest)) }
  | LBRACKET RBRACKET
      { mk $startpos (Sequence []) }
  | LBRACKET e=expr COMMA RBRACKET
      { mk $startpos (Sequence [ e ]) }
  | LBRACKET e1=expr COMMA rest=separated_nonempty_list(COMMA, expr) RBRACKET
      { mk $startpos (Sequence (e1 :: rest)) }
  | LBRACKET e=expr RBRACKET
      { mk $startpos (Sequence [ e ]) }
  | LPAREN e=expr RPAREN
      { e }
