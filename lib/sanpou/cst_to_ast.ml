open Cst

let rec conv_expr = function
  | IntLit { value; _ } -> Ast.IntLit value
  | BoolLit { value; _ } -> Ast.BoolLit value
  | Var { name; _ } -> Ast.Var name
  | BinOp { lhs; op; rhs; _ } ->
      Ast.BinOp (conv_binop op, conv_expr lhs, conv_expr rhs)
  | App { name; args; _ } -> Ast.App (name, List.map conv_expr args.items)
  | Tuple { elems; _ } -> Ast.Tuple (List.map conv_expr elems.items)
  | Paren { inner; _ } -> conv_expr inner

and conv_binop = function
  | Plus -> Ast.Plus
  | Minus -> Ast.Minus
  | Mult -> Ast.Mult
  | Lt -> Ast.Lt
  | Eq -> Ast.Eq

let conv_simple_stmt = function
  | Assign { name; value; _ } -> Ast.Assign (name, conv_expr value)
  | Call { name; args; _ } -> Ast.Call (name, List.map conv_expr args.items)
  | Return { value; _ } -> Ast.Return (conv_expr value)
  | Break _ -> Ast.Break
  | Await { cond; _ } -> Ast.Await (conv_expr cond)

let rec conv_block_stmt = function
  | While { cond; body; _ } -> Ast.While (conv_expr cond, conv_body body)
  | If { cond; body; _ } -> Ast.If (conv_expr cond, conv_body body)

and conv_step = function
  | SimpleStep { stmts; _ } -> List.map conv_simple_stmt stmts.items
  | EmptyStep _ -> []
  | BlockStep { stmt; _ } -> [ conv_block_stmt stmt ]
  | WhileWait { cond; _ } -> [ Ast.While (conv_expr cond, []) ]
  | LetStep { name; value; _ } -> [ Ast.Let (name, conv_expr value) ]

and conv_body steps = List.map conv_step steps

let conv_item = function
  | ConstDef { name; value; _ } -> Ast.ConstDef (name, conv_expr value)
  | FunDef { name; params; body_expr; _ } ->
      Ast.FunDef (name, List.map snd params.items, conv_expr body_expr)
  | VarDecl { name; value; _ } -> Ast.VarDecl (name, conv_expr value)
  | ProcDef { name; params; body; _ } ->
      Ast.ProcDef (name, List.map snd params.items, conv_body body)
  | Process { name; proc; lo; hi; _ } ->
      Ast.Process (name, proc, conv_expr lo, conv_expr hi)

let conv_module_def (m : module_def) : Ast.module_def =
  { mod_name = m.mod_name; items = List.map conv_item m.items }

let convert (prog : program) : Ast.program =
  List.map conv_module_def prog.modules
