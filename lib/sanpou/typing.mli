(** Hindley-Milner type inference over the AST. [check] returns unit on
    success and raises [Type_error] otherwise; no type information is
    attached to the tree. *)

type ty
(** An inferred type; render with [string_of_ty]. *)

type type_error =
  | Type_clash of ty * ty
  | Unbound_variable of Ast.id
  | Arity_mismatch of Ast.id * int * int
  | Not_a_function of Ast.id
  | Break_outside_loop
  | Continue_outside_loop
  | Return_type_mismatch
  | Assign_to_non_variable of Ast.id
  | Recursive_type

exception Type_error of type_error * Ast.loc

val check : Ast.program -> unit
val string_of_ty : ty -> string
val string_of_type_error : type_error -> string
