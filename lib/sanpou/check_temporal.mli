(** Restricts the temporal operators (globally/finally) to [property] bodies and
    rejects references to properties from anywhere but another property. Runs on
    the resolved AST so shadowed names need no special handling. *)

exception Error of string * Generic_ast.loc

val check : Resolved_ast.program -> unit
