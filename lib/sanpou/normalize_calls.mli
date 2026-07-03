(** Call normalization: hoist every procedure call out of expression position
    into a [Normalized_ast.CallBindStep] binding a fresh temporary, in
    evaluation order, so that all expressions downstream are pure. A procedure
    call where no procedure frame exists — module-level expressions,
    map-initializer bodies — raises [Error]. *)

exception Error of string * Generic_ast.loc

val normalize : Resolved_ast.program -> Normalized_ast.program
