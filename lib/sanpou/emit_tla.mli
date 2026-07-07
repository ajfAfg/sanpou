(** TLA+ emission: render a linearized module ([Ir.module_ir]) as a TLA+ module
    AST. [config] controls optional checks (e.g. termination). *)

val generate_module : ?config:Config.t -> Ir.module_ir -> Tla.Tla_ast.tla_module

val reserved_module_names : string list
(** Top-level TLA+ names the emitter generates or pulls in via EXTENDS; user
    module-level declarations of these are rejected by [Typing]. *)

val is_generated_action_label : string -> bool
(** Whether a name has the shape of a generated action label ([L1], [L2], ...),
    which are emitted as top-level operators. *)
