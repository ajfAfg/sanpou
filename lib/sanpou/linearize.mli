(** Linearization: flatten alpha-converted procedure bodies into flat lists of
    labeled actions ([Ir.module_ir]), making control flow explicit as [pc]
    label transitions. Procedure calls are resolved to entry labels here. *)

val linearize : Alpha_convert.alpha_module list -> Ir.module_ir list
