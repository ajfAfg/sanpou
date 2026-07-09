(** Names reserved by the emitted TLA+ module: the emitter's generated top-level
    names, the operators of the standard modules it EXTENDS, and the generated
    action-label shape. Shared by the front end (which rejects collisions) and
    [Emit_tla] (which produces the names). *)

val default_init_value : string
(** The null frame sentinel CONSTANT the emitter declares. *)

val module_names : string list
(** Top-level TLA+ names the emitter generates or pulls in via EXTENDS. *)

val is_generated_action_label : string -> bool
(** Whether a name has the shape of a generated action label ([L1], [L2], ...),
    which are emitted as top-level operators. *)

val is_reserved : string -> bool
(** [module_names] membership or generated-action-label shape. *)
