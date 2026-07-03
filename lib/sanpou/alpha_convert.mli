(** Alpha conversion: surface AST -> resolved AST in which every
    procedure-local binding (parameter, [var], MapInit binder) has a unique
    TLA-safe name. Each [Resolved_ast.ident] keeps its source name in
    [original], so no rename table needs to travel with the tree. *)

val transform : Ast.id Ast.program -> Resolved_ast.program
