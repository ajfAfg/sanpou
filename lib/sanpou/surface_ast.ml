(* ===== The surface syntax tree =====

   [Generic_ast] instantiated for the tree exactly as parsed: variable names are
   plain source names and applied callees are undistinguished — the parser
   cannot know whether a name refers to a function or a procedure.
   [Alpha_convert] turns this tree into [Resolved_ast]. *)

type expr = (Generic_ast.id, Generic_ast.id) Generic_ast.expr
type accessor = (Generic_ast.id, Generic_ast.id) Generic_ast.accessor
type assign_target = (Generic_ast.id, Generic_ast.id) Generic_ast.assign_target
type simple_stmt = (Generic_ast.id, Generic_ast.id) Generic_ast.simple_stmt
type step = (Generic_ast.id, Generic_ast.id) Generic_ast.step
type block_stmt = (Generic_ast.id, Generic_ast.id) Generic_ast.block_stmt
type body = (Generic_ast.id, Generic_ast.id) Generic_ast.body
type item = (Generic_ast.id, Generic_ast.id) Generic_ast.item
type module_def = (Generic_ast.id, Generic_ast.id) Generic_ast.module_def
type program = (Generic_ast.id, Generic_ast.id) Generic_ast.program
