open Generic_ast

(* ===== Temporal-operator placement: resolved AST =====

   [globally] and [finally] parse as builtin applications but are TLA+
   temporal operators, meaningful only in the property position — the body
   of a module-level [def] referenced from the TLC config. Their builtin
   type (bool -> bool) says nothing about that, so this pass enforces it
   syntactically:

   - the operators may appear only in module-level [def name = ...] bodies;
   - a def containing one (directly or by referencing another temporal def)
     is a *temporal property*, and referencing it from any runtime context
     (procedure bodies, function bodies, variable initializers, process
     ranges) is an error.

   Running after [Alpha_convert] makes shadowing a non-issue: procedure
   locals and binders are renamed apart, while references to module-level
   defs keep their source name. *)

exception Error of string * Generic_ast.loc

let error message loc = raise (Error (message, loc))

(* The first temporal operator or temporal-def reference in [e], with the
   message describing it. *)
let rec temporal_occurrence (temporal_defs : id list) (e : Resolved_ast.expr) :
    (string * Generic_ast.loc) option =
  let find = temporal_occurrence temporal_defs in
  let first exprs = List.find_map find exprs in
  match e.desc with
  | Builtin (((Builtin.Globally | Builtin.Finally) as b), _) ->
      Some
        ( Builtin.name b
          ^ " is a temporal operator and is only allowed in a module-level \
             def",
          e.loc )
  | Builtin (_, args) -> first args
  | Var i ->
      if List.mem i.name temporal_defs then
        Some
          ( i.original
            ^ " is a temporal property and can only be referenced from \
               another module-level def",
            e.loc )
      else None
  | IntLit _ | BoolLit _ | Self -> None
  | UnOp (_, rhs) -> find rhs
  | BinOp (_, lhs, rhs) -> first [ lhs; rhs ]
  | App (_, args) -> first args
  | Subscript (lhs, index) -> first [ lhs; index ]
  | MapInit { lo; hi; value; _ } -> first [ lo; hi; value ]
  | Tuple elems | Sequence elems -> first elems
  | IfExpr (cond, then_e, else_e) -> first [ cond; then_e; else_e ]
  | Quant { lo; hi; body; _ } -> first [ lo; hi; body ]

let check_expr temporal_defs (e : Resolved_ast.expr) : unit =
  match temporal_occurrence temporal_defs e with
  | Some (message, loc) -> error message loc
  | None -> ()

let check_stmt temporal_defs (stmt : Resolved_ast.simple_stmt) : unit =
  let check = check_expr temporal_defs in
  match stmt.desc with
  | Assign (target, value) ->
      (match target with
      | VarTarget _ -> ()
      | SubscriptTarget (_, indices) -> List.iter check indices);
      check value
  | Call (_, args) -> List.iter check args
  | Return value | Await value | Assert value -> check value
  | Break | Continue -> ()

let rec check_body temporal_defs (steps : Resolved_ast.body) : unit =
  List.iter (check_step temporal_defs) steps

and check_step temporal_defs (step : Resolved_ast.step) : unit =
  let check = check_expr temporal_defs in
  match step.desc with
  | SimpleStep stmts -> List.iter (check_stmt temporal_defs) stmts
  | EmptyStep -> ()
  | VarStep (_, value) -> check value
  | WithStep { lo; hi; stmts; _ } ->
      check lo;
      check hi;
      List.iter (check_stmt temporal_defs) stmts
  | BlockStep (While { cond; body }) ->
      check cond;
      check_body temporal_defs body
  | BlockStep (If { cond; body; else_body }) ->
      check cond;
      check_body temporal_defs body;
      Option.iter (check_body temporal_defs) else_body
  | BlockStep (Either arms) -> List.iter (check_body temporal_defs) arms

let check_module (m : Resolved_ast.module_def) : unit =
  let (_ : id list) =
    List.fold_left
      (fun temporal_defs (item : Resolved_ast.item) ->
        match item.desc with
        | ConstDef { name; value } ->
            (* The one place temporal formulas belong; a def containing one
               becomes temporal itself and taints its referrers. *)
            if temporal_occurrence temporal_defs value <> None then
              name :: temporal_defs
            else temporal_defs
        | FunDef { body_expr; _ } ->
            check_expr temporal_defs body_expr;
            temporal_defs
        | VarDecl { init; _ } ->
            (match init with
            | InitValue value -> check_expr temporal_defs value
            | InitRange (lo, hi) ->
                check_expr temporal_defs lo;
                check_expr temporal_defs hi);
            temporal_defs
        | ProcDef { body; _ } ->
            check_body temporal_defs body;
            temporal_defs
        | Process { lo; hi; _ } ->
            check_expr temporal_defs lo;
            check_expr temporal_defs hi;
            temporal_defs)
      [] m.items
  in
  ()

let check (prog : Resolved_ast.program) : unit = List.iter check_module prog
