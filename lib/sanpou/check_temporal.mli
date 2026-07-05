(** Restricts the temporal operators (globally/finally) to module-level def
    bodies — the property position — and rejects references to such
    temporal defs from runtime contexts. Runs on the resolved AST so
    shadowed names need no special handling. *)

exception Error of string * Generic_ast.loc

val check : Resolved_ast.program -> unit
