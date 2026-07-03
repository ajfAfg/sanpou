(* ===== The surface syntax tree =====

   [Ast] instantiated for the tree exactly as parsed: variable names are
   plain source names and applied callees are undistinguished — the parser
   cannot know whether a name refers to a function or a procedure.
   [Alpha_convert] turns this tree into [Resolved_ast]. *)

type expr = (Ast.id, Ast.id) Ast.expr
type assign_target = (Ast.id, Ast.id) Ast.assign_target
type simple_stmt = (Ast.id, Ast.id) Ast.simple_stmt
type step = (Ast.id, Ast.id) Ast.step
type block_stmt = (Ast.id, Ast.id) Ast.block_stmt
type body = (Ast.id, Ast.id) Ast.body
type item = (Ast.id, Ast.id) Ast.item
type module_def = (Ast.id, Ast.id) Ast.module_def
type program = (Ast.id, Ast.id) Ast.program
