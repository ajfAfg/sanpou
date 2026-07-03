(* ===== The alpha-converted syntax tree =====

   [Generic_ast] itself knows nothing about renaming or callee resolution; this
   module instantiates its name and callee parameters for the tree that
   [Alpha_convert] produces and everything downstream (linearize, emit)
   consumes. Variable positions carry an [ident]: the unique TLA-safe [name]
   used for code generation, paired with the source [original] used for
   display. Application positions carry a [callee] recording whether the
   applied name is a pure function or a procedure. *)

type ident = { name : Generic_ast.id; original : Generic_ast.id }
[@@deriving show, eq]

(* An ident whose display name is its unique name: names alpha conversion
   leaves untouched (module-level bindings) and compiler-synthesized
   temporaries. *)
let ident name = { name; original = name }
let display i = i.original

(* Which kind of definition an applied name refers to. Functions are pure
   and stay inside expressions; procedure calls transfer control and must
   eventually be hoisted out of expressions. *)
type callee = Fun of Generic_ast.id | Proc of Generic_ast.id
[@@deriving show, eq]

let callee_name = function Fun name | Proc name -> name

type expr = (ident, callee) Generic_ast.expr
type assign_target = (ident, callee) Generic_ast.assign_target
type simple_stmt = (ident, callee) Generic_ast.simple_stmt
type step = (ident, callee) Generic_ast.step
type body = (ident, callee) Generic_ast.body
type item = (ident, callee) Generic_ast.item
type module_def = (ident, callee) Generic_ast.module_def
type program = (ident, callee) Generic_ast.program

let equal_expr = Generic_ast.equal_expr equal_ident equal_callee
