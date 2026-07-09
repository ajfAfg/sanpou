(** Scope and context checks on the surface AST, run before [Typing]: unbound
    names, callable kinds (a function/procedure used as a value, a non-callable
    applied, a statement call to a non-procedure), [break]/[continue] outside a
    loop, [self] outside a procedure, assignment to a non-mutable name,
    process-root shape (a declared, nullary procedure), reserved emitted names,
    and duplicate modules. Name resolution is sequential and lexical; builtins
    are shadowed by definitions of the same name. *)

exception Error of string * Generic_ast.loc

val check : Surface_ast.program -> unit
