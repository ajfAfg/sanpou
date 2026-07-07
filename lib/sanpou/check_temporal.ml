open Generic_ast

(* ===== Temporal-operator placement: resolved AST =====

   [globally] and [finally] parse as builtin applications but are TLA+
   temporal operators, meaningful only inside a [property] item — the
   formulas the TLC config's PROPERTIES section references. Temporal-ness
   is declared by the item kind, so this pass is a plain placement check:

   - the operators may appear only in [property] bodies;
   - a property may be referenced only from another property (a runtime
     value cannot depend on a temporal formula).

   Running after [Alpha_convert] makes shadowing a non-issue: procedure
   locals and binders are renamed apart, module-level references keep
   their source name, and a definition shadowing the [globally]/[finally]
   builtin has already turned the application into a plain [App]. *)

exception Error of string * Generic_ast.loc

let error message loc = raise (Error (message, loc))

(* The first temporal operator or property reference in [e], with the
   message describing it. *)
let rec temporal_occurrence (props : id list) (e : Resolved_ast.expr) :
    (string * Generic_ast.loc) option =
  let find = temporal_occurrence props in
  let first exprs = List.find_map find exprs in
  match e.desc with
  | Builtin (((Builtin.Globally | Builtin.Finally) as b), _) ->
      Some
        ( Builtin.name b
          ^ " is a temporal operator and is only allowed in a property",
          e.loc )
  | Builtin (_, args) -> first args
  | Var i ->
      if List.mem i.name props then
        Some
          ( i.original
            ^ " is a property and can only be referenced from another \
               property",
            e.loc )
      else None
  | IntLit _ | BoolLit _ | StrLit _ | AtomLit _ | Self -> None
  | UnOp (_, rhs) -> find rhs
  | BinOp (_, lhs, rhs) -> first [ lhs; rhs ]
  | App (_, args) -> first args
  | Subscript (lhs, index) -> first [ lhs; index ]
  | Field (record, _) -> find record
  | Record fields -> first (List.map snd fields)
  | Range (lo, hi) -> first [ lo; hi ]
  | MapInit { domain; value; _ } -> first [ domain; value ]
  | Tuple elems | Sequence elems | SetLit elems -> first elems
  | SetComp { domain; pred; _ } -> first [ domain; pred ]
  | IfExpr (cond, then_e, else_e) -> first [ cond; then_e; else_e ]
  | Quant { domain; body; _ } -> first [ domain; body ]

let check_expr props (e : Resolved_ast.expr) : unit =
  match temporal_occurrence props e with
  | Some (message, loc) -> error message loc
  | None -> ()

let check_stmt props (stmt : Resolved_ast.simple_stmt) : unit =
  let check = check_expr props in
  match stmt.desc with
  | Assign (target, value) ->
      (match target with
      | VarTarget _ -> ()
      | PathTarget (_, path) ->
          List.iter
            (function AccIndex e -> check e | AccField _ -> ())
            path);
      check value
  | Call (_, args) -> List.iter check args
  | Return value | Await value | Assert value -> check value
  | Break | Continue -> ()

let rec check_body props (steps : Resolved_ast.body) : unit =
  List.iter (check_step props) steps

and check_step props (step : Resolved_ast.step) : unit =
  let check = check_expr props in
  match step.desc with
  | SimpleStep stmts -> List.iter (check_stmt props) stmts
  | EmptyStep -> ()
  | VarStep (_, value) -> check value
  | WithStep { domain; stmts; _ } ->
      check domain;
      List.iter (check_stmt props) stmts
  | BlockStep (While { cond; body }) ->
      check cond;
      check_body props body
  | BlockStep (If { cond; body; else_body }) ->
      check cond;
      check_body props body;
      Option.iter (check_body props) else_body
  | BlockStep (Either arms) -> List.iter (check_body props) arms

let check_module (m : Resolved_ast.module_def) : unit =
  let (_ : id list) =
    List.fold_left
      (fun props (item : Resolved_ast.item) ->
        match item.desc with
        | PropDef { name; _ } ->
            (* Temporal operators and references to preceding properties
               are exactly what a property body is for. *)
            name :: props
        | ConstDef { value; _ } | FunDef { body_expr = value; _ } ->
            check_expr props value;
            props
        | VarDecl { init; _ } ->
            (match init with
            | InitValue value -> check_expr props value
            | InitIn domain -> check_expr props domain);
            props
        | ProcDef { body; _ } ->
            check_body props body;
            props
        | Process { domain; _ } ->
            check_expr props domain;
            props)
      [] m.items
  in
  ()

let check (prog : Resolved_ast.program) : unit = List.iter check_module prog
