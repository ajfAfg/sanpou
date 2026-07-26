(** TLA+ emission: render a linearized module ([Ir.module_ir]) as a TLA+ module
    AST. [config] controls optional checks (e.g. termination). The names the
    emitter reserves for itself live in [Reserved_names]. *)

val generate_module : ?config:Config.t -> Ir.module_ir -> Tla.Tla_ast.tla_module
