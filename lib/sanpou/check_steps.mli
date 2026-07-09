(** Step-structure checks on the surface AST, run before [Typing]: within one
    atomic step (a SimpleStep or WithStep statement list), a whole-variable
    assignment cannot be combined with another assignment to the same variable,
    and a control transfer (call, return, break, continue) must be the step's
    final statement. *)

exception Error of string * Generic_ast.loc

val check : Surface_ast.program -> unit
