(** Hindley-Milner type inference over the AST. [check] returns unit on success
    and raises [Type_error] otherwise; no type information is attached to the
    tree. *)

type ty
(** An inferred type; render with [string_of_ty]. *)

type type_error =
  | Type_clash of ty * ty
  | Unbound_variable of Generic_ast.id
  | Arity_mismatch of Generic_ast.id * int * int
  | Not_a_function of Generic_ast.id
  | Break_outside_loop
  | Continue_outside_loop
  | Return_type_mismatch
  | Assign_to_non_variable of Generic_ast.id
  | Recursive_type
  | Callable_as_value of Generic_ast.id
  | Not_a_procedure of Generic_ast.id
  | Not_a_record of ty
  | Unknown_field of Generic_ast.id * ty
  | Self_outside_procedure
  | Reserved_module_name of Generic_ast.id

exception Type_error of type_error * Generic_ast.loc

val check : Surface_ast.program -> unit
val string_of_ty : ty -> string
val string_of_type_error : type_error -> string
