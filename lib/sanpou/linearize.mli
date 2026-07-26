(** Linearization: flatten call-normalized procedure bodies into flat lists of
    labeled actions ([Ir.module_ir]), making control flow explicit as [pc] label
    transitions. *)

exception Error of string * Generic_ast.loc
(** A located linearization diagnostic (e.g. a procedure body that can fall off
    its end without a return). *)

val linearize : Normalized_ast.program -> Ir.module_ir list
