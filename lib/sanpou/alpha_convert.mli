(** Alpha conversion: AST -> AST in which every procedure-local binding
    (parameter, [var], MapInit binder) has a unique TLA-safe name. The renames
    are recorded so later passes can map TLA names back to source names. *)

type rename_kind =
  | ParamVar
  | LocalVar
  | BinderVar
      (** MapInit binder: never becomes a TLA state variable, but is still
          needed to demangle descriptions *)

type rename = {
  tla_name : string;
  original : string;
  proc : string;  (** procedure the binding belongs to *)
  kind : rename_kind;
}

type alpha_module = { ast : Ast.module_def; renames : rename list }

val transform : Ast.program -> alpha_module list
