(** Hindley-Milner type inference over the AST. [check] returns unit on success
    and raises [Type_error] otherwise; no type information is attached to the
    tree. Assumes a well-scoped program: name resolution, callable kinds, and
    statement context are [Check_scope]'s job, and inference here treats a
    violation of that precondition as an internal error. *)

type ty
(** An inferred type; render with [string_of_ty]. *)

type type_error =
  | Type_clash of ty * ty
  | Arity_mismatch of Generic_ast.id * int * int
  | Recursive_type
  | Not_a_record of ty
  | Unknown_field of Generic_ast.id * ty

exception Type_error of type_error * Generic_ast.loc

val check : Surface_ast.program -> unit
val string_of_ty : ty -> string
val string_of_type_error : type_error -> string
