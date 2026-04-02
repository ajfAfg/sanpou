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
%token <Cst.trivia> DEF LET FN MOD PROCESS IN
%token <Cst.trivia> WHILE IF RETURN BREAK AWAIT
%token <Cst.trivia> PLUS MINUS MULT LT EQ EQEQ
%token <Cst.trivia> LPAREN RPAREN LBRACE RBRACE
%token <Cst.trivia> SEMI COMMA DOTDOT
%token <Cst.trivia> EOF

%left EQEQ
%left LT
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
  | let_t=LET id=ID eq_t=EQ value=expr semi_t=SEMI
      { let (name_t, name) = id in VarDecl { let_t; name_t; name; eq_t; value; semi_t } }
  | fn_t=FN id=ID lp=LPAREN params=param_list rp=RPAREN lb=LBRACE body=body rb=RBRACE
      { let (name_t, name) = id in ProcDef { fn_t; name_t; name; lp; params; rp; lb; body; rb } }
  | process_t=PROCESS id=ID eq_t=EQ proc_id=ID in_t=IN lo=expr dotdot_t=DOTDOT hi=expr semi_t=SEMI
      { let (name_t, name) = id in
        let (proc_t, proc) = proc_id in
        Process { process_t; name_t; name; eq_t; proc_t; proc; in_t; lo; dotdot_t; hi; semi_t } }

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
  | while_t=WHILE lp=LPAREN cond=expr rp=RPAREN semi_t=SEMI { WhileWait { loc = loc_of_pos $startpos; while_t; lp; cond; rp; semi_t } }
  | let_t=LET id=ID eq_t=EQ value=expr semi_t=SEMI
      { let (name_t, name) = id in LetStep { loc = loc_of_pos $startpos; let_t; name_t; name; eq_t; value; semi_t } }

(* comma-separated nonempty simple_stmt list *)
atomic_stmts_nonempty:
  | s=simple_stmt { singleton_comma_list s }
  | s=simple_stmt c=COMMA rest=atomic_stmts_nonempty
      { cons_comma_list c s rest }

simple_stmt:
  | id=ID eq_t=EQ value=expr
      { let (name_t, name) = id in Assign { name_t; name; eq_t; value } }
  | id=ID lp=LPAREN args=expr_list rp=RPAREN
      { let (name_t, name) = id in Call { name_t; name; lp; args; rp } }
  | t=RETURN value=expr
      { Return { t; value } }
  | t=BREAK
      { Break { t } }
  | t=AWAIT cond=expr
      { Await { t; cond } }

block_stmt:
  | while_t=WHILE lp=LPAREN cond=expr rp=RPAREN lb=LBRACE body=body rb=RBRACE
      { While { while_t; lp; cond; rp; lb; body; rb } }
  | if_t=IF lp=LPAREN cond=expr rp=RPAREN lb=LBRACE body=body rb=RBRACE
      { If { if_t; lp; cond; rp; lb; body; rb } }

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
  | e=expr_ { e }
  | e1=expr op_t=PLUS e2=expr { BinOp { lhs = e1; op_t; op = Plus; rhs = e2 } }
  | e1=expr op_t=MINUS e2=expr { BinOp { lhs = e1; op_t; op = Minus; rhs = e2 } }
  | e1=expr op_t=MULT e2=expr { BinOp { lhs = e1; op_t; op = Mult; rhs = e2 } }
  | e1=expr op_t=LT e2=expr { BinOp { lhs = e1; op_t; op = Lt; rhs = e2 } }
  | e1=expr op_t=EQEQ e2=expr { BinOp { lhs = e1; op_t; op = Eq; rhs = e2 } }

expr_:
  | i=INTV { let (t, value) = i in IntLit { t; value } }
  | t=TRUE { BoolLit { t; value = true } }
  | t=FALSE { BoolLit { t; value = false } }
  | id=ID lp=LPAREN args=expr_list rp=RPAREN
      { let (name_t, name) = id in App { name_t; name; lp; args; rp } }
  | id=ID { let (t, name) = id in Var { t; name } }
  | lp=LPAREN rp=RPAREN
      { Tuple { lp; elems = empty_comma_list; trailing_comma = None; rp } }
  | lp=LPAREN e=expr c=COMMA rp=RPAREN
      { Tuple { lp; elems = singleton_comma_list e; trailing_comma = Some c; rp } }
  | lp=LPAREN e1=expr c=COMMA rest=expr_list_nonempty rp=RPAREN
      { Tuple { lp; elems = cons_comma_list c e1 rest; trailing_comma = None; rp } }
  | lp=LPAREN inner=expr rp=RPAREN { Paren { lp; inner; rp } }
