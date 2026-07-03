open Tla.Tla_ast
open Ir
open Config

(* ===== AST expression → TLA+ expression ===== *)

let binop_to_tla = function
  | Generic_ast.Plus -> "+"
  | Generic_ast.Minus -> "-"
  | Generic_ast.Mult -> "*"
  | Generic_ast.Lt -> "<"
  | Generic_ast.LtEq -> "<="
  | Generic_ast.GtEq -> ">="
  | Generic_ast.Eq -> "="
  | Generic_ast.Neq -> "/="
  | Generic_ast.And -> "/\\"
  | Generic_ast.Or -> "\\/"

let rec expr_to_tla local_vars (e : Normalized_ast.expr) =
  match e.desc with
  | Generic_ast.IntLit value -> TInt value
  | Generic_ast.BoolLit value -> TBool value
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
          TConcat (expr_to_tla local_vars lhs, expr_to_tla local_vars rhs)
      | _ -> assert false (* arity enforced by Typing *))
  | Generic_ast.App (Normalized_ast.Fun name, args) ->
      TApp (name, List.map (expr_to_tla local_vars) args)
  | Generic_ast.Subscript (lhs, index) ->
      TSubscript (expr_to_tla local_vars lhs, expr_to_tla local_vars index)
  | Generic_ast.MapInit { binder; lo; hi; value } ->
      TFuncMap
        ( binder.name,
          TRange (expr_to_tla local_vars lo, expr_to_tla local_vars hi),
          expr_to_tla local_vars value )
  | Generic_ast.Tuple elems -> TSeqLit (List.map (expr_to_tla local_vars) elems)
  | Generic_ast.Sequence elems ->
      TSeqLit (List.map (expr_to_tla local_vars) elems)

(* Module-level expressions have no local vars *)
let expr_to_tla_global = expr_to_tla []

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
          | AssignVar (name, _) | AssignIndex (name, _, _) ->
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
           | AssignVar (name, _) | AssignIndex (name, _, _) -> is_local name)
         action.assignments
  in
  if stack_touched then unchanged else "stack" :: unchanged

(* ===== Action IR → TLA+ AST ===== *)

(* Locals live in the top frame of stack[self]. Every write to a local is an
   update of that frame, so each action builds a single stack' expression:
   frame updates first, then the stack operation. This relies on an invariant
   guaranteed by Linearize: an action assigns locals of procedure P only while
   a P-frame is on top (pop actions carry no guards/assignments, so the
   transient [value |-> v] record is never read or written except by them). *)
let action_to_decl proc_entry_labels frame_fields local_vars (ir : module_ir)
    (action : action) : tla_decl =
  let to_tla = expr_to_tla local_vars in
  let self = TId "self" in
  let stack_self = TSubscript (TId "stack", self) in
  let stack_head = THead stack_self in
  let pc_test_conjunct =
    TBinOp ("=", TSubscript (TId "pc", self), TStr action.label)
  in
  let guard_conjuncts =
    match action.guard with Some g -> [ to_tla g ] | None -> []
  in
  (* Split assignments: globals become their own conjuncts, locals become
     EXCEPT updates of the top frame folded into stack'. *)
  let local_updates =
    List.filter_map
      (function
        | AssignVar (var, expr) when List.mem var local_vars ->
            Some ([ SubSel (TInt 1); FieldSel var ], to_tla expr)
        | AssignIndex (var, index, expr) when List.mem var local_vars ->
            Some
              ( [ SubSel (TInt 1); FieldSel var; SubSel (to_tla index) ],
                to_tla expr )
        | _ -> None)
      action.assignments
  in
  let global_assign_conjuncts =
    List.filter_map
      (function
        | AssignVar (var, expr) when not (List.mem var local_vars) ->
            Some (TBinOp ("=", TPrimed (TId var), to_tla expr))
        | AssignIndex (var, index, expr) when not (List.mem var local_vars) ->
            Some
              (TBinOp
                 ( "=",
                   TPrimed (TId var),
                   TExcept
                     (TId var, [ ([ SubSel (to_tla index) ], to_tla expr) ]) ))
        | _ -> None)
      action.assignments
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
           process root proc pushed by a wrapper) start as the null sentinel:
           sanpou values are never strings, so "__null__" cannot collide with
           a user value and keeps "unassigned" distinguishable from a computed
           0. Every field must exist at push time: TLC silently ignores EXCEPT
           updates to missing record fields. *)
        let null_value = TStr "__null__" in
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
  let conjuncts =
    (pc_test_conjunct :: guard_conjuncts)
    @ global_assign_conjuncts @ stack_op_conjuncts @ stack_conjuncts
    @ (pc_conjunct :: unchanged_conjuncts)
  in
  DOpDef (action.label, [ "self" ], TConj (Block, conjuncts))

let proc_to_decls proc_entry_labels frame_fields local_vars (ir : module_ir)
    (proc : proc_ir) : tla_decl list =
  let action_decls =
    List.concat_map
      (fun a ->
        [
          action_to_decl proc_entry_labels frame_fields local_vars ir a;
          DSeparator;
        ])
      proc.actions
  in
  let action_labels = List.map (fun (a : action) -> a.label) proc.actions in
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

let header_decls (ir : module_ir) : tla_decl list =
  let global_vars = List.map fst ir.var_decls in
  let all_tla_vars = ("pc" :: global_vars) @ [ "stack" ] in
  [
    DExtends [ "TLC"; "Sequences"; "Integers" ];
    DSeparator;
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
  with_trailing_sep
    (List.map
       (fun (name, expr) -> DOpDef (name, [], expr_to_tla_global expr))
       ir.const_defs)
  @ with_trailing_sep
      (List.map
         (fun (name, params, expr) ->
           DOpDef (name, params, expr_to_tla_global expr))
         ir.fun_defs)

let proc_set_decls (ir : module_ir) : tla_decl list =
  let parts =
    List.map
      (fun (p : process_ir) ->
        TParens (TRange (expr_to_tla_global p.lo, expr_to_tla_global p.hi)))
      ir.processes
  in
  [ DOpDef ("ProcSet", [], TCup parts); DSeparator ]

let init_decls (ir : module_ir) : tla_decl list =
  let pc_init =
    match ir.processes with
    | [ single ] ->
        TFuncMap ("self", TId "ProcSet", TStr single.wrapper.entry_label)
    | _ ->
        let cases =
          List.map
            (fun (p : process_ir) ->
              ( TIn
                  ( TId "self",
                    TRange (expr_to_tla_global p.lo, expr_to_tla_global p.hi) ),
                TStr p.wrapper.entry_label ))
            ir.processes
        in
        TFuncMap ("self", TId "ProcSet", TCase cases)
  in
  let conjuncts =
    List.map
      (fun (name, expr) -> TBinOp ("=", TId name, expr_to_tla_global expr))
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

let termination_decls : tla_decl list =
  let all_done =
    TForall
      ( "self",
        TId "ProcSet",
        TBinOp ("=", TSubscript (TId "pc", TId "self"), TStr Ir.done_label) )
  in
  [
    DOpDef
      ( "Terminating",
        [],
        TConj (Block, [ all_done; TUnchangedExpr (TId "vars") ]) );
    DSeparator;
    DOpDef ("Termination", [], TFinally all_done);
    DSeparator;
  ]

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
               TRange (expr_to_tla_global p.lo, expr_to_tla_global p.hi),
               TApp (p.wrapper.proc_name, [ TId "self" ]) )))
      ir.processes
  in
  let termination_disjuncts =
    if config.checks.termination then [ TId "Terminating" ] else []
  in
  (if config.checks.termination then termination_decls else [])
  @ [
      DOpDef
        ( "Next",
          [],
          TDisj
            ( Block,
              procedure_disjuncts @ process_disjuncts @ termination_disjuncts )
        );
      DSeparator;
    ]

let spec_decls (ir : module_ir) : tla_decl list =
  let fairness_conjuncts =
    List.concat_map
      (fun (p : process_ir) ->
        if p.fair then
          let process_range =
            TRange (expr_to_tla_global p.lo, expr_to_tla_global p.hi)
          in
          let fairness_for proc_name =
            TParens
              (TForall
                 ( "self",
                   process_range,
                   TApp ("WF_vars", [ TApp (proc_name, [ TId "self" ]) ]) ))
          in
          fairness_for p.wrapper.proc_name
          :: List.map
               (fun (proc : proc_ir) -> fairness_for proc.proc_name)
               ir.procs
        else [])
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
  let body =
    List.concat
      [
        header_decls ir;
        const_and_fun_decls ir;
        proc_set_decls ir;
        init_decls ir;
        runtime_proc_decls proc_entry_labels frame_fields ir all_runtime_procs;
        next_decls config ir;
        spec_decls ir;
      ]
  in
  { name = ir.name; body }
