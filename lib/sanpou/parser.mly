%{
open Cst

let empty_comma_list : 'a comma_list = { items = []; commas = [] }
let singleton_comma_list (x : 'a) : 'a comma_list = { items = [x]; commas = [] }
let cons_comma_list t (x : 'a) (cl : 'a comma_list) : 'a comma_list =
  { items = x :: cl.items; commas = t :: cl.commas }

let loc_of_pos (pos : Lexing.position) : loc =
  { line = pos.pos_lnum; col = pos.pos_cnum - pos.pos_bol + 1 }
%}

%token <Cst.trivia * int> INTV
%token <Cst.trivia * string> ID
%token <Cst.trivia> TRUE FALSE
%token <Cst.trivia> DEF VAR FN MOD FAIR PROCESS IN SELF
%token <Cst.trivia> WHILE IF ELSE RETURN BREAK CONTINUE AWAIT
%token <Cst.trivia> PLUS MINUS MULT LT LTEQ GTEQ EQ EQEQ NEQ ANDAND OROR
%token <Cst.trivia> LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token <Cst.trivia> SEMI COMMA COLON DOTDOT
%token <Cst.trivia> EOF

%left OROR
%left ANDAND
%left EQEQ NEQ
%left LT LTEQ GTEQ
%left PLUS MINUS
%left MULT

%start <Cst.program> program
%%

program:
  | modules=list(module_def) eof_t=EOF { { modules; eof_t } }

module_def:
  | mod_t=MOD id=ID lb=LBRACE items=list(item) rb=RBRACE
      { let (name_t, mod_name) = id in { mod_t; name_t; mod_name; lb; items; rb } }

item:
  | def_t=DEF id=ID eq_t=EQ value=expr semi_t=SEMI
      { let (name_t, name) = id in ConstDef { def_t; name_t; name; eq_t; value; semi_t } }
  | def_t=DEF id=ID lp=LPAREN params=param_list rp=RPAREN eq_t=EQ body_expr=expr semi_t=SEMI
      { let (name_t, name) = id in FunDef { def_t; name_t; name; lp; params; rp; eq_t; body_expr; semi_t } }
    | var_t=VAR id=ID eq_t=EQ value=expr semi_t=SEMI
      { let (name_t, name) = id in VarDecl { var_t; name_t; name; eq_t; value; semi_t } }
  | fn_t=FN id=ID lp=LPAREN params=param_list rp=RPAREN lb=LBRACE body=body rb=RBRACE
      { let (name_t, name) = id in ProcDef { fn_t; name_t; name; lp; params; rp; lb; body; rb } }
    | fair_t=option(FAIR) process_t=PROCESS id=ID eq_t=EQ proc_id=ID in_t=IN lo=expr dotdot_t=DOTDOT hi=expr semi_t=SEMI
      { let (name_t, name) = id in
        let (proc_t, proc) = proc_id in
      Process { fair_t; process_t; name_t; name; eq_t; proc_t; proc; in_t; lo; dotdot_t; hi; semi_t } }

(* comma-separated parameter list: (trivia * id) comma_list *)
param_list:
  | (* empty *) { empty_comma_list }
  | id=ID { let (t, name) = id in singleton_comma_list (t, name) }
  | id=ID c=COMMA rest=param_list_nonempty
      { let (t, name) = id in cons_comma_list c (t, name) rest }

param_list_nonempty:
  | id=ID { let (t, name) = id in singleton_comma_list (t, name) }
  | id=ID c=COMMA rest=param_list_nonempty
      { let (t, name) = id in cons_comma_list c (t, name) rest }

body:
  | steps=list(step) { steps }

step:
  | stmts=atomic_stmts_nonempty semi_t=SEMI { SimpleStep { loc = loc_of_pos $startpos; stmts; semi_t } }
  | semi_t=SEMI { EmptyStep { loc = loc_of_pos $startpos; semi_t } }
  | stmt=block_stmt { BlockStep { loc = loc_of_pos $startpos; stmt } }
    | var_t=VAR id=ID eq_t=EQ value=expr semi_t=SEMI
      { let (name_t, name) = id in VarStep { loc = loc_of_pos $startpos; var_t; name_t; name; eq_t; value; semi_t } }

(* comma-separated nonempty simple_stmt list *)
atomic_stmts_nonempty:
  | s=simple_stmt { singleton_comma_list s }
  | s=simple_stmt c=COMMA rest=atomic_stmts_nonempty
      { cons_comma_list c s rest }

simple_stmt:
  | target=assign_target eq_t=EQ value=expr
    { Assign { target; eq_t; value } }
  | id=ID lp=LPAREN args=expr_list rp=RPAREN
      { let (name_t, name) = id in Call { name_t; name; lp; args; rp } }
  | t=RETURN value=expr
      { Return { t; value } }
  | t=BREAK
      { Break { t } }
  | t=CONTINUE
    { Continue { t } }
  | t=AWAIT cond=expr
      { Await { t; cond } }

assign_target:
  | id=ID
    { let (name_t, name) = id in VarTarget { name_t; name } }
  | id=ID lb_t=LBRACKET index=expr rb_t=RBRACKET
    { let (name_t, name) = id in
    SubscriptTarget { name_t; name; lb_t; index; rb_t } }

block_stmt:
  | while_t=WHILE lp=LPAREN cond=expr rp=RPAREN lb=LBRACE body=body rb=RBRACE
      { While { while_t; lp; cond; rp; lb; body; rb } }
  | if_t=IF lp=LPAREN cond=expr rp=RPAREN lb=LBRACE body=body rb=RBRACE else_branch=option(else_clause)
      { If { if_t; lp; cond; rp; lb; body; rb; else_branch } }

else_clause:
  | else_t=ELSE lb=LBRACE body=body rb=RBRACE { (else_t, lb, body, rb) }

(* comma-separated expr list (possibly empty) *)
expr_list:
  | (* empty *) { empty_comma_list }
  | e=expr { singleton_comma_list e }
  | e=expr c=COMMA rest=expr_list_nonempty
      { cons_comma_list c e rest }

expr_list_nonempty:
  | e=expr { singleton_comma_list e }
  | e=expr c=COMMA rest=expr_list_nonempty
      { cons_comma_list c e rest }

expr:
  | e=or_expr { e }

or_expr:
  | e=and_expr { e }
  | e1=or_expr op_t=OROR e2=and_expr
      { BinOp { lhs = e1; op_t; op = Or; rhs = e2 } }

and_expr:
  | e=comparison_expr { e }
  | e1=and_expr op_t=ANDAND e2=comparison_expr
      { BinOp { lhs = e1; op_t; op = And; rhs = e2 } }

comparison_expr:
  | e=add_expr { e }
  | e1=comparison_expr op_t=LT e2=add_expr
      { BinOp { lhs = e1; op_t; op = Lt; rhs = e2 } }
  | e1=comparison_expr op_t=EQEQ e2=add_expr
      { BinOp { lhs = e1; op_t; op = Eq; rhs = e2 } }
  | e1=comparison_expr op_t=NEQ e2=add_expr
      { BinOp { lhs = e1; op_t; op = Neq; rhs = e2 } }
  | e1=comparison_expr op_t=LTEQ e2=add_expr
      { BinOp { lhs = e1; op_t; op = LtEq; rhs = e2 } }
  | e1=comparison_expr op_t=GTEQ e2=add_expr
      { BinOp { lhs = e1; op_t; op = GtEq; rhs = e2 } }

add_expr:
  | e=mult_expr { e }
  | e1=add_expr op_t=PLUS e2=mult_expr
      { BinOp { lhs = e1; op_t; op = Plus; rhs = e2 } }
  | e1=add_expr op_t=MINUS e2=mult_expr
      { BinOp { lhs = e1; op_t; op = Minus; rhs = e2 } }

mult_expr:
  | e=postfix_expr { e }
  | e1=mult_expr op_t=MULT e2=postfix_expr
      { BinOp { lhs = e1; op_t; op = Mult; rhs = e2 } }

postfix_expr:
  | e=primary_expr { e }
  | lhs=postfix_expr lb_t=LBRACKET index=expr rb_t=RBRACKET
      { Subscript { lhs; lb_t; index; rb_t } }

primary_expr:
  | op_t=MINUS rhs=primary_expr { UnOp { op_t; op = Neg; rhs } }
  | i=INTV { let (t, value) = i in IntLit { t; value } }
  | t=TRUE { BoolLit { t; value = true } }
  | t=FALSE { BoolLit { t; value = false } }
  | t=SELF { Self { t } }
  | id=ID lp=LPAREN args=expr_list rp=RPAREN
      { let (name_t, name) = id in App { name_t; name; lp; args; rp } }
  | id=ID { let (t, name) = id in Var { t; name } }
  | lb=LBRACE id=ID in_t=IN lo=expr dotdot_t=DOTDOT hi=expr colon_t=COLON value=expr trailing_semi_t=option(SEMI) rb=RBRACE
      { let (binder_t, binder) = id in
        MapInit { lb; binder_t; binder; in_t; lo; dotdot_t; hi; colon_t; value; trailing_semi_t; rb } }
  | lp=LPAREN rp=RPAREN
      { Tuple { lp; elems = empty_comma_list; trailing_comma = None; rp } }
  | lp=LPAREN e=expr c=COMMA rp=RPAREN
      { Tuple { lp; elems = singleton_comma_list e; trailing_comma = Some c; rp } }
  | lp=LPAREN e1=expr c=COMMA rest=expr_list_nonempty rp=RPAREN
      { Tuple { lp; elems = cons_comma_list c e1 rest; trailing_comma = None; rp } }
  | lb=LBRACKET rb=RBRACKET
      { Sequence { lb; elems = empty_comma_list; trailing_comma = None; rb } }
  | lb=LBRACKET e=expr c=COMMA rb=RBRACKET
      { Sequence { lb; elems = singleton_comma_list e; trailing_comma = Some c; rb } }
  | lb=LBRACKET e1=expr c=COMMA rest=expr_list_nonempty rb=RBRACKET
      { Sequence { lb; elems = cons_comma_list c e1 rest; trailing_comma = None; rb } }
  | lb=LBRACKET e=expr rb=RBRACKET
      { Sequence { lb; elems = singleton_comma_list e; trailing_comma = None; rb } }
  | lp=LPAREN inner=expr rp=RPAREN { Paren { lp; inner; rp } }
