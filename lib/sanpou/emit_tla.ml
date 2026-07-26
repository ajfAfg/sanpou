open Tla.Tla_ast
open Ir
open Config

(* Null sentinel for not-yet-assigned frame fields; see [Reserved_names]
   for why it is a model-value CONSTANT. The full set of top-level names
   this emitter claims (its generated names plus the operators of the
   standard modules it EXTENDS) lives in [Reserved_names.module_names];
   [Check_scope] rejects user declarations that collide with them. *)
let default_init_value = Reserved_names.default_init_value

(* ===== AST expression → TLA+ expression ===== *)

let binop_to_tla = function
  | Generic_ast.Plus -> "+"
  | Generic_ast.Minus -> "-"
  | Generic_ast.Mult -> "*"
  | Generic_ast.Div -> "\\div"
  | Generic_ast.Mod -> "%"
  | Generic_ast.Lt -> "<"
  | Generic_ast.Gt -> ">"
  | Generic_ast.LtEq -> "<="
  | Generic_ast.GtEq -> ">="
  | Generic_ast.Eq -> "="
  | Generic_ast.Neq -> "/="
  | Generic_ast.And -> "/\\"
  | Generic_ast.Or -> "\\/"
  | Generic_ast.In -> "\\in"

let rec expr_to_tla local_vars (e : Normalized_ast.expr) =
  match e.desc with
  | Generic_ast.IntLit value -> TInt value
  | Generic_ast.BoolLit value -> TBool value
  | Generic_ast.StrLit s -> TStr s
  | Generic_ast.AtomLit a ->
      (* the atom's text is its CONSTANT's name *)
      TId a
  | Generic_ast.Var i ->
      (* Locals live in the top stack frame; globals are plain variables *)
      if List.mem i.name local_vars then
        TDot (THead (TSubscript (TId "stack", TId "self")), i.name)
      else TId i.name
  | Generic_ast.Self -> TId "self"
  | Generic_ast.UnOp (Generic_ast.Neg, rhs) -> (
      match rhs.desc with
      | Generic_ast.IntLit value -> TInt (-value)
      | _ -> TParens (TBinOp ("-", TInt 0, expr_to_tla local_vars rhs)))
  | Generic_ast.UnOp (Generic_ast.Not, rhs) -> TNot (expr_to_tla local_vars rhs)
  | Generic_ast.BinOp (op, lhs, rhs) ->
      TParens
        (TBinOp
           ( binop_to_tla op,
             expr_to_tla local_vars lhs,
             expr_to_tla local_vars rhs ))
  | Generic_ast.Builtin (b, args) -> (
      match (b, args) with
      | Builtin.Globally, [ e ] -> TGlobally (expr_to_tla local_vars e)
      | Builtin.Finally, [ e ] -> TFinally (expr_to_tla local_vars e)
      | Builtin.Head, [ e ] -> THead (expr_to_tla local_vars e)
      | Builtin.Tail, [ e ] -> TTail (expr_to_tla local_vars e)
      | Builtin.Append, [ seq; value ] ->
          TApp
            ( "Append",
              [ expr_to_tla local_vars seq; expr_to_tla local_vars value ] )
      | Builtin.Concat, [ lhs; rhs ] ->
          (* Parenthesized like every other infix: a bare \o binds looser
             than a following subscript, so concat(a, b)[1] would otherwise
             emit a \o b[1], which TLA+ reads as a \o (b[1]). *)
          TParens
            (TConcat (expr_to_tla local_vars lhs, expr_to_tla local_vars rhs))
      | Builtin.Len, [ e ] -> TApp ("Len", [ expr_to_tla local_vars e ])
      | Builtin.Union, [ lhs; rhs ] ->
          TParens
            (TBinOp
               ("\\cup", expr_to_tla local_vars lhs, expr_to_tla local_vars rhs))
      | Builtin.Intersection, [ lhs; rhs ] ->
          TParens
            (TBinOp
               ("\\cap", expr_to_tla local_vars lhs, expr_to_tla local_vars rhs))
      | Builtin.Difference, [ lhs; rhs ] ->
          TParens
            (TBinOp
               ("\\", expr_to_tla local_vars lhs, expr_to_tla local_vars rhs))
      | Builtin.Subseteq, [ lhs; rhs ] ->
          TParens
            (TBinOp
               ( "\\subseteq",
                 expr_to_tla local_vars lhs,
                 expr_to_tla local_vars rhs ))
      | Builtin.Cardinality, [ e ] ->
          TApp ("Cardinality", [ expr_to_tla local_vars e ])
      | _ -> assert false (* arity enforced by Typing *))
  | Generic_ast.App (Normalized_ast.Fun name, args) ->
      TApp (name, List.map (expr_to_tla local_vars) args)
  | Generic_ast.Subscript (lhs, index) ->
      TSubscript (expr_to_tla local_vars lhs, expr_to_tla local_vars index)
  | Generic_ast.Field (record, label) ->
      TDot (expr_to_tla local_vars record, label)
  | Generic_ast.Record fields ->
      (* The AST keeps fields in source order (evaluation order for hoisted
         calls); a TLA+ record is a function, so emitting label-sorted is
         semantics-preserving and keeps the output canonical. *)
      TRecord
        (fields
        |> List.sort (fun (a, _) (b, _) -> compare a b)
        |> List.map (fun (l, e) -> (l, expr_to_tla local_vars e)))
  | Generic_ast.Range (lo, hi) ->
      TRange (expr_to_tla local_vars lo, expr_to_tla local_vars hi)
  | Generic_ast.MapInit { binder; domain; value } ->
      TFuncMap
        ( binder.name,
          expr_to_tla local_vars domain,
          expr_to_tla local_vars value )
  | Generic_ast.Tuple elems -> TSeqLit (List.map (expr_to_tla local_vars) elems)
  | Generic_ast.Sequence elems ->
      TSeqLit (List.map (expr_to_tla local_vars) elems)
  | Generic_ast.SetLit elems -> TSet (List.map (expr_to_tla local_vars) elems)
  | Generic_ast.SetComp { binder; domain; pred } ->
      TSetFilter
        (binder.name, expr_to_tla local_vars domain, expr_to_tla local_vars pred)
  | Generic_ast.IfExpr (cond, then_e, else_e) ->
      TParens
        (TIf
           ( expr_to_tla local_vars cond,
             expr_to_tla local_vars then_e,
             expr_to_tla local_vars else_e ))
  | Generic_ast.Quant { quant; binder; domain; body } ->
      (* Parenthesized because TLA+'s \A and \E extend to the right: an
         unparenthesized quantifier as a binop operand would swallow the
         rest of the enclosing expression. *)
      let range = expr_to_tla local_vars domain in
      let body = expr_to_tla local_vars body in
      TParens
        (match quant with
        | Generic_ast.Forall -> TForall (binder.name, range, body)
        | Generic_ast.Exists -> TExists (binder.name, range, body))

(* Module-level expressions have no local vars *)
let expr_to_tla_global = expr_to_tla []

(* ===== FiniteSets usage =====
   Set literals, comprehension, and the set operators (\cup, \cap, \,
   \subseteq, \in) are all in TLA+'s core; only [cardinality] pulls in the
   FiniteSets module, so it is extended only when actually used. *)

let rec expr_uses_cardinality (e : Normalized_ast.expr) : bool =
  match e.desc with
  | Generic_ast.Builtin (Builtin.Cardinality, _) -> true
  | Generic_ast.IntLit _ | Generic_ast.BoolLit _ | Generic_ast.StrLit _
  | Generic_ast.AtomLit _ | Generic_ast.Var _ | Generic_ast.Self ->
      false
  | Generic_ast.UnOp (_, e) -> expr_uses_cardinality e
  | Generic_ast.BinOp (_, a, b)
  | Generic_ast.Range (a, b)
  | Generic_ast.Subscript (a, b) ->
      expr_uses_cardinality a || expr_uses_cardinality b
  | Generic_ast.Field (record, _) -> expr_uses_cardinality record
  | Generic_ast.Record fields ->
      List.exists (fun (_, e) -> expr_uses_cardinality e) fields
  | Generic_ast.App (_, args)
  | Generic_ast.Builtin (_, args)
  | Generic_ast.Tuple args
  | Generic_ast.Sequence args
  | Generic_ast.SetLit args ->
      List.exists expr_uses_cardinality args
  | Generic_ast.MapInit { domain; value; _ } ->
      expr_uses_cardinality domain || expr_uses_cardinality value
  | Generic_ast.SetComp { domain; pred; _ } ->
      expr_uses_cardinality domain || expr_uses_cardinality pred
  | Generic_ast.IfExpr (a, b, c) ->
      expr_uses_cardinality a || expr_uses_cardinality b
      || expr_uses_cardinality c
  | Generic_ast.Quant { domain; body; _ } ->
      expr_uses_cardinality domain || expr_uses_cardinality body

let action_uses_cardinality (a : action) : bool =
  let accessor_uses = function
    | Generic_ast.AccIndex i -> expr_uses_cardinality i
    | Generic_ast.AccField _ -> false
  in
  let assignment_uses = function
    | AssignVar (_, e) -> expr_uses_cardinality e
    | AssignPath (_, path, e) ->
        List.exists accessor_uses path || expr_uses_cardinality e
  in
  let stack_op_uses = function
    | StackPush (_, _, args) -> List.exists expr_uses_cardinality args
    | StackReturn e -> expr_uses_cardinality e
    | StackNone | StackDiscard | StackPopAssign _ -> false
  in
  let pc_dest_uses = function
    | PcBranch (cond, _, _) -> expr_uses_cardinality cond
    | PcNext _ | PcCall _ | PcReturn -> false
  in
  Option.fold ~none:false ~some:expr_uses_cardinality a.guard
  || List.exists (fun (c, _) -> expr_uses_cardinality c) a.asserts
  || List.exists assignment_uses a.assignments
  || List.exists (fun (_, d) -> expr_uses_cardinality d) a.binders
  || pc_dest_uses a.pc_dest || stack_op_uses a.stack_op

let node_uses_cardinality = function
  | Action a -> action_uses_cardinality a
  | Choice c -> List.exists action_uses_cardinality c.arms

let module_uses_cardinality (ir : module_ir) all_runtime_procs : bool =
  let var_init_uses = function
    | Generic_ast.InitValue e | Generic_ast.InitIn e -> expr_uses_cardinality e
  in
  List.exists
    (function
      | Normalized_ast.DefConst (_, e) | Normalized_ast.DefFun (_, _, e) ->
          expr_uses_cardinality e)
    ir.defs
  || List.exists (fun (_, e) -> expr_uses_cardinality e) ir.prop_defs
  || List.exists (fun (_, init) -> var_init_uses init) ir.var_decls
  || List.exists
       (fun (p : process_ir) -> expr_uses_cardinality p.domain)
       ir.processes
  || List.exists
       (fun (p : proc_ir) -> List.exists node_uses_cardinality p.actions)
       all_runtime_procs

(* ===== UNCHANGED computation ===== *)

let compute_unchanged (ir : module_ir) (action : action) : string list =
  let global_vars = List.map fst ir.var_decls in
  let is_local name = List.mem name ir.local_var_decls in
  let assigned =
    (match action.stack_op with
      | StackPopAssign name when not (is_local name) -> [ name ]
      | _ -> [])
    @ List.filter_map
        (function
          | AssignVar (name, _) | AssignPath (name, _, _) ->
              if is_local name then None else Some name)
        action.assignments
  in
  let unchanged =
    List.filter (fun v -> not (List.mem v assigned)) global_vars
  in
  let stack_touched =
    action.stack_op <> StackNone
    || List.exists
         (function
           | AssignVar (name, _) | AssignPath (name, _, _) -> is_local name)
         action.assignments
  in
  if stack_touched then unchanged else "stack" :: unchanged

(* ===== Action IR → TLA+ AST ===== *)

let pc_test_conjunct label =
  TBinOp ("=", TSubscript (TId "pc", TId "self"), TStr label)

(* Everything an action says except its pc test: guard, assignments, stack
   operation, pc', and UNCHANGED. Ordinary actions prepend their own pc test;
   a Choice node disjoins its arms' conjunct lists under a single pc test.

   Locals live in the top frame of stack[self]. Every write to a local is an
   update of that frame, so each action builds a single stack' expression:
   frame updates first, then the stack operation. This relies on an invariant
   guaranteed by Linearize: an action assigns locals of procedure P only while
   a P-frame is on top (pop actions carry no guards/assignments, so the
   transient [value |-> v] record is never read or written except by them). *)
let action_conjuncts proc_entry_labels frame_fields local_vars (ir : module_ir)
    (action : action) : tla_expr list =
  let to_tla = expr_to_tla local_vars in
  let self = TId "self" in
  let stack_self = TSubscript (TId "stack", self) in
  let stack_head = THead stack_self in
  let guard_conjuncts =
    match action.guard with Some g -> [ to_tla g ] | None -> []
  in
  (* After the guard: conjunctions evaluate left to right, so a disabled
     action never reaches its asserts; an enabled one halts TLC with the
     message when the condition is false. *)
  let assert_conjuncts =
    List.map
      (fun (cond, message) -> TApp ("Assert", [ to_tla cond; TStr message ]))
      action.asserts
  in
  (* An access path becomes an EXCEPT selector path: subscripts index, fields
     select. *)
  let selectors_of path =
    List.map
      (function
        | Generic_ast.AccIndex index -> SubSel (to_tla index)
        | Generic_ast.AccField field -> FieldSel field)
      path
  in
  (* Split assignments: globals become their own conjuncts, locals become
     EXCEPT updates of the top frame folded into stack'. *)
  let local_updates =
    List.filter_map
      (function
        | AssignVar (var, expr) when List.mem var local_vars ->
            Some ([ SubSel (TInt 1); FieldSel var ], to_tla expr)
        | AssignPath (var, path, expr) when List.mem var local_vars ->
            Some
              ( [ SubSel (TInt 1); FieldSel var ] @ selectors_of path,
                to_tla expr )
        | _ -> None)
      action.assignments
  in
  let global_assign_conjuncts =
    (* Merge every path update to the same global into one EXCEPT: several
       field/subscript writes in a single step (e.g. [r.a = x, r.b = y]) must
       produce one [r' = [r EXCEPT !.a = x, !.b = y]] conjunct, not two
       conflicting [r' = ...] ones. Variables keep first-occurrence order. *)
    let is_global var = not (List.mem var local_vars) in
    let add acc var entry =
      let rec go = function
        | [] -> [ (var, [ entry ]) ]
        | (v, es) :: rest when v = var -> (v, es @ [ entry ]) :: rest
        | pair :: rest -> pair :: go rest
      in
      go acc
    in
    let grouped =
      List.fold_left
        (fun acc assignment ->
          match assignment with
          | AssignVar (var, expr) when is_global var ->
              add acc var (`Whole (to_tla expr))
          | AssignPath (var, path, expr) when is_global var ->
              add acc var (`Update (selectors_of path, to_tla expr))
          | _ -> acc)
        [] action.assignments
    in
    List.map
      (fun (var, entries) ->
        let assigned =
          (* A whole-variable assignment takes the variable; otherwise the
             per-field/subscript updates combine into a single EXCEPT. *)
          match
            List.find_opt (function `Whole _ -> true | _ -> false) entries
          with
          | Some (`Whole v) -> v
          | _ ->
              let updates =
                List.filter_map
                  (function `Update u -> Some u | `Whole _ -> None)
                  entries
              in
              TExcept (TId var, updates)
        in
        TBinOp ("=", TPrimed (TId var), assigned))
      grouped
  in
  let base =
    if local_updates = [] then stack_self
    else TExcept (stack_self, local_updates)
  in
  (* The new value of stack[self] (if the action touches the stack at all),
     plus any extra conjunct the stack operation contributes. *)
  let new_stack_self, stack_op_conjuncts =
    match action.stack_op with
    | StackNone -> ((if local_updates = [] then None else Some base), [])
    | StackPush (proc_name, return_label, args) ->
        let params, other_fields = List.assoc proc_name frame_fields in
        (* Bind params to args (evaluated in the caller's frame, unprimed).
           Not-yet-assigned fields (the callee's locals, and params of a
           process root proc pushed by a wrapper) start as the null sentinel,
           which keeps "unassigned" distinguishable from a computed 0. Every
           field must exist at push time: TLC silently ignores EXCEPT updates
           to missing record fields. *)
        let null_value = TId default_init_value in
        let rec bind params args =
          match (params, args) with
          | [], _ -> []
          | p :: ps, [] -> (p, null_value) :: bind ps []
          | p :: ps, a :: rest -> (p, to_tla a) :: bind ps rest
        in
        let frame =
          TRecord
            ([ ("procedure", TStr proc_name); ("return_pc", TStr return_label) ]
            @ bind params args
            @ List.map (fun v -> (v, null_value)) other_fields)
        in
        (Some (TConcat (TSeqLit [ frame ], base)), [])
    | StackReturn retval ->
        (* Replace the departing frame with a record carrying only the return
           value; the caller's pop action consumes it. *)
        ( Some
            (TConcat
               (TSeqLit [ TRecord [ ("value", to_tla retval) ] ], TTail base)),
          [] )
    | StackDiscard -> (Some (TTail base), [])
    | StackPopAssign var ->
        if List.mem var local_vars then
          (* After Tail the caller's frame is element 1 *)
          ( Some
              (TExcept
                 ( TTail base,
                   [
                     ( [ SubSel (TInt 1); FieldSel var ],
                       TDot (stack_head, "value") );
                   ] )),
            [] )
        else
          ( Some (TTail base),
            [ TBinOp ("=", TPrimed (TId var), TDot (stack_head, "value")) ] )
  in
  let stack_conjuncts =
    match new_stack_self with
    | Some v ->
        [
          TBinOp
            ( "=",
              TPrimed (TId "stack"),
              TExcept (TId "stack", [ ([ SubSel self ], v) ]) );
        ]
    | None -> []
  in
  let set_pc v =
    TBinOp
      ("=", TPrimed (TId "pc"), TExcept (TId "pc", [ ([ SubSel self ], v) ]))
  in
  let pc_conjunct =
    set_pc
      (match action.pc_dest with
      | PcNext l -> TStr l
      | PcBranch (cond, t, f) -> TIf (to_tla cond, TStr t, TStr f)
      | PcCall callee -> TStr (List.assoc callee proc_entry_labels)
      | PcReturn -> TDot (stack_head, "return_pc"))
  in
  let unchanged_conjuncts =
    match compute_unchanged ir action with
    | [] -> []
    | unchanged -> [ TUnchanged (List.map (fun v -> TId v) unchanged) ]
  in
  let base =
    guard_conjuncts @ assert_conjuncts @ global_assign_conjuncts
    @ stack_op_conjuncts @ stack_conjuncts
    @ (pc_conjunct :: unchanged_conjuncts)
  in
  (* An action with binders fires for any binder value satisfying its
     guard: the whole effect is wrapped in \E over the range. *)
  match action.binders with
  | [] -> base
  | binders ->
      [
        List.fold_right
          (fun (name, domain) body -> TExists (name, to_tla domain, body))
          binders
          (TConj (Block, base));
      ]

let node_to_decl proc_entry_labels frame_fields local_vars (ir : module_ir)
    (node : action_node) : tla_decl =
  let conjuncts_of =
    action_conjuncts proc_entry_labels frame_fields local_vars ir
  in
  match node with
  | Action action ->
      DOpDef
        ( action.label,
          [ "self" ],
          TConj (Block, pc_test_conjunct action.label :: conjuncts_of action) )
  | Choice { label; arms; _ } ->
      (* One pc value; the transition disjoins the arms, each carrying its
         own guard, effect, pc', and UNCHANGED. TLC picks any enabled arm. *)
      DOpDef
        ( label,
          [ "self" ],
          TConj
            ( Block,
              [
                pc_test_conjunct label;
                TDisj
                  ( Block,
                    List.map (fun arm -> TConj (Block, conjuncts_of arm)) arms
                  );
              ] ) )

let proc_to_decls proc_entry_labels frame_fields local_vars (ir : module_ir)
    (proc : proc_ir) : tla_decl list =
  let action_decls =
    List.concat_map
      (fun node ->
        [
          node_to_decl proc_entry_labels frame_fields local_vars ir node;
          DSeparator;
        ])
      proc.actions
  in
  let action_labels = List.map node_label proc.actions in
  let disj =
    DOpDef
      ( proc.proc_name,
        [ "self" ],
        TDisj
          (Inline, List.map (fun l -> TApp (l, [ TId "self" ])) action_labels)
      )
  in
  action_decls @ [ disj ]

(* ===== Module sections =====
   Each section function returns the tla_decls it contributes (including its
   trailing separators); generate_module concatenates them in order. *)

let header_decls ~uses_default_init_value ~uses_finite_sets (ir : module_ir) :
    tla_decl list =
  let global_vars = List.map fst ir.var_decls in
  let all_tla_vars = ("pc" :: global_vars) @ [ "stack" ] in
  let extends =
    [ "TLC"; "Sequences"; "Integers" ]
    @ if uses_finite_sets then [ "FiniteSets" ] else []
  in
  (* User model values and the null sentinel are both CONSTANTs; the .cfg
     assigns each to its own name (see Config.to_cfg_string). *)
  let constants =
    ir.atoms @ if uses_default_init_value then [ default_init_value ] else []
  in
  [ DExtends extends; DSeparator ]
  @ (match constants with
    | [] -> []
    | _ -> [ DConstants constants; DSeparator ])
  @ [
      DVariables all_tla_vars;
      DSeparator;
      DOpDef ("vars", [], TSeqLit (List.map (fun v -> TId v) all_tla_vars));
      DSeparator;
    ]

let const_and_fun_decls (ir : module_ir) : tla_decl list =
  let with_trailing_sep = function
    | [] -> []
    | decls -> decls @ [ DSeparator ]
  in
  (* Source order: sequential name resolution accepts a constant defined via
     an earlier function (and vice versa), so the TLA+ module must keep the
     interleaving to define before use. *)
  with_trailing_sep
    (List.map
       (function
         | Normalized_ast.DefConst (name, expr) ->
             DOpDef (name, [], expr_to_tla_global expr)
         | Normalized_ast.DefFun (name, params, expr) ->
             DOpDef (name, params, expr_to_tla_global expr))
       ir.defs)
  @ with_trailing_sep
      (List.map
         (fun (name, expr) -> DOpDef (name, [], expr_to_tla_global expr))
         ir.prop_defs)

let proc_set_decls (ir : module_ir) : tla_decl list =
  let parts =
    List.map
      (fun (p : process_ir) -> TParens (expr_to_tla_global p.domain))
      ir.processes
  in
  (* Init's pc is a CASE over the process domains, so an ID belonging to two
     processes would silently get the first one's entry label and the other
     instance would never run. Domains are constant (enforced by Typing), so
     pairwise-disjointness ASSUMEs make TLC fail fast at startup instead. *)
  let disjoint_assumes =
    let rec pairs = function
      | [] -> []
      | d :: rest -> List.map (fun d' -> (d, d')) rest @ pairs rest
    in
    List.map
      (fun (d1, d2) ->
        DAssume (TBinOp ("=", TParens (TBinOp ("\\cap", d1, d2)), TSet [])))
      (pairs parts)
  in
  [ DOpDef ("ProcSet", [], TCup parts); DSeparator ]
  @ match disjoint_assumes with [] -> [] | ds -> ds @ [ DSeparator ]

let init_decls (ir : module_ir) : tla_decl list =
  let pc_init =
    match ir.processes with
    | [ single ] ->
        TFuncMap ("self", TId "ProcSet", TStr single.wrapper.entry_label)
    | _ ->
        let cases =
          List.map
            (fun (p : process_ir) ->
              ( TIn (TId "self", expr_to_tla_global p.domain),
                TStr p.wrapper.entry_label ))
            ir.processes
        in
        TFuncMap ("self", TId "ProcSet", TCase cases)
  in
  let conjuncts =
    List.map
      (fun (name, init) ->
        match init with
        | Generic_ast.InitValue expr ->
            TBinOp ("=", TId name, expr_to_tla_global expr)
        | Generic_ast.InitIn domain -> TIn (TId name, expr_to_tla_global domain))
      ir.var_decls
    @ [
        TBinOp ("=", TId "stack", TFuncMap ("self", TId "ProcSet", TSeqLit []));
        TBinOp ("=", TId "pc", pc_init);
      ]
  in
  [ DOpDef ("Init", [], TConj (Block, conjuncts)); DSeparator ]

let runtime_proc_decls proc_entry_labels frame_fields (ir : module_ir)
    all_runtime_procs : tla_decl list =
  List.concat_map
    (fun proc ->
      proc_to_decls proc_entry_labels frame_fields ir.local_var_decls ir proc
      @ [ DSeparator ])
    all_runtime_procs

(* [Terminating] plays two roles: as a stuttering disjunct of Next it keeps
   TLC's deadlock check from reporting normal completion (all processes at
   Done) as a deadlock — PlusCal emits it unconditionally for the same
   reason — and it is the base of the [Termination] liveness property,
   which is only defined when the termination check asks for it. *)
let all_done =
  TForall
    ( "self",
      TId "ProcSet",
      TBinOp ("=", TSubscript (TId "pc", TId "self"), TStr Ir.done_label) )

let terminating_decls : tla_decl list =
  [
    DOpDef
      ( "Terminating",
        [],
        TConj (Block, [ all_done; TUnchangedExpr (TId "vars") ]) );
    DSeparator;
  ]

let termination_property_decls : tla_decl list =
  [ DOpDef ("Termination", [], TFinally all_done); DSeparator ]

let next_decls config (ir : module_ir) : tla_decl list =
  let procedure_disjuncts =
    if ir.procs <> [] then
      let proc_disj =
        TDisj
          ( Inline,
            List.map
              (fun (p : proc_ir) -> TApp (p.proc_name, [ TId "self" ]))
              ir.procs )
      in
      [ TParens (TExists ("self", TId "ProcSet", proc_disj)) ]
    else []
  in
  let process_disjuncts =
    List.map
      (fun (p : process_ir) ->
        TParens
          (TExists
             ( "self",
               expr_to_tla_global p.domain,
               TApp (p.wrapper.proc_name, [ TId "self" ]) )))
      ir.processes
  in
  (* An explicit "Termination" in the sidecar's properties implies the
     Termination property definition even when the check flag is off; the
     Terminating stuttering disjunct is unconditional either way. *)
  let wants_termination =
    config.checks.termination || List.mem "Termination" config.properties
  in
  terminating_decls
  @ (if wants_termination then termination_property_decls else [])
  @ [
      DOpDef
        ( "Next",
          [],
          TDisj
            ( Block,
              procedure_disjuncts @ process_disjuncts @ [ TId "Terminating" ] )
        );
      DSeparator;
    ]

let spec_decls (ir : module_ir) : tla_decl list =
  let fairness_conjuncts =
    List.concat_map
      (fun (p : process_ir) ->
        match p.fairness with
        | Generic_ast.Unfair -> []
        | Generic_ast.WeakFair | Generic_ast.StrongFair ->
            let fairness_op =
              match p.fairness with
              | Generic_ast.StrongFair -> "SF_vars"
              | _ -> "WF_vars"
            in
            let process_range = expr_to_tla_global p.domain in
            let fairness_for proc_name =
              TParens
                (TForall
                   ( "self",
                     process_range,
                     TApp (fairness_op, [ TApp (proc_name, [ TId "self" ]) ]) ))
            in
            fairness_for p.wrapper.proc_name
            :: List.map
                 (fun (proc : proc_ir) -> fairness_for proc.proc_name)
                 ir.procs)
      ir.processes
  in
  [
    DOpDef
      ( "Spec",
        [],
        TConj
          ( Inline,
            [ TId "Init"; TBoxAction (TId "Next", TId "vars") ]
            @ fairness_conjuncts ) );
    DSeparator;
  ]

let generate_module ?(config = Config.default) (ir : module_ir) : tla_module =
  let wrapper_procs =
    List.map (fun (p : process_ir) -> p.wrapper) ir.processes
  in
  let all_runtime_procs = ir.procs @ wrapper_procs in
  let proc_entry_labels =
    List.map
      (fun (p : proc_ir) -> (p.proc_name, p.entry_label))
      all_runtime_procs
  in
  (* Frame layout per procedure: params (in declaration order) plus the
     procedure's other locals (user vars and callRet temps). Only procs in
     ir.procs can be pushed (wrappers are never callees). *)
  let frame_fields =
    List.map
      (fun (p : proc_ir) ->
        let other_fields =
          List.filter_map
            (fun (v : var_info) ->
              match v.proc with
              | Some owner
                when owner = p.proc_name && not (List.mem v.tla_name p.params)
                ->
                  Some v.tla_name
              | _ -> None)
            ir.var_infos
        in
        (p.proc_name, (p.params, other_fields)))
      ir.procs
  in
  (* The null sentinel appears when a push leaves frame fields unbound —
     locals and callRet temps always, params only when the pushing action
     supplies no arguments (a wrapper starting a root proc) — and when
     Canonicalize_locals pinned a dead local to it (an [AssignVar] whose
     right-hand side is the bare sentinel). *)
  let uses_default_init_value =
    let node_actions = function Action a -> [ a ] | Choice c -> c.arms in
    let assigns_sentinel (a : action) =
      List.exists
        (function
          | AssignVar (_, { Generic_ast.desc = Generic_ast.Var i; _ }) ->
              i.name = default_init_value
          | _ -> false)
        a.assignments
    in
    List.exists
      (fun (p : proc_ir) ->
        List.exists
          (fun (a : action) ->
            assigns_sentinel a
            ||
            match a.stack_op with
            | StackPush (callee, _, args) ->
                let params, other_fields = List.assoc callee frame_fields in
                List.length args < List.length params || other_fields <> []
            | _ -> false)
          (List.concat_map node_actions p.actions))
      all_runtime_procs
  in
  let uses_finite_sets = module_uses_cardinality ir all_runtime_procs in
  let body =
    List.concat
      [
        header_decls ~uses_default_init_value ~uses_finite_sets ir;
        const_and_fun_decls ir;
        proc_set_decls ir;
        init_decls ir;
        runtime_proc_decls proc_entry_labels frame_fields ir all_runtime_procs;
        next_decls config ir;
        spec_decls ir;
      ]
  in
  { name = ir.name; body }
