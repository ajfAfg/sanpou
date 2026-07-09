(** Process-domain constancy check on the surface AST, run after [Typing]: a
    process ID set must be a constant expression (the emitted spec fixes ProcSet
    at Init), so a domain that reads a [var] — directly or through a def — is
    rejected at the offending reference. *)

exception Error of string * Generic_ast.loc

val check : Surface_ast.program -> unit
