(* ===== The alpha-converted syntax tree =====

   [Ast] itself knows nothing about renaming; this module instantiates its
   name parameter for the tree that [Alpha_convert] produces and everything
   downstream (linearize, emit) consumes. Variable positions carry an
   [ident]: the unique TLA-safe [name] used for code generation, paired with
   the source [original] used for display. *)

type ident = { name : Ast.id; original : Ast.id } [@@deriving show, eq]

(* An ident whose display name is its unique name: names alpha conversion
   leaves untouched (module-level bindings) and compiler-synthesized
   temporaries. *)
let ident name = { name; original = name }

let display i = i.original

type expr = ident Ast.expr
type assign_target = ident Ast.assign_target
type simple_stmt = ident Ast.simple_stmt
type step = ident Ast.step
type body = ident Ast.body
type item = ident Ast.item
type module_def = ident Ast.module_def
type program = ident Ast.program

let equal_expr = Ast.equal_expr equal_ident
