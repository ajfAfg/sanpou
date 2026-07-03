(** Linearization: flatten alpha-converted procedure bodies into flat lists of
    labeled actions ([Ir.module_ir]), making control flow explicit as [pc]
    label transitions. *)

val linearize : Resolved_ast.program -> Ir.module_ir list
