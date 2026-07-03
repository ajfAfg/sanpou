open Tla.Tla_ast
open Ir
open Config

(* ===== CST expression → TLA+ expression ===== *)

let binop_to_tla = function
  | Cst.Plus -> "+"
  | Cst.Minus -> "-"
  | Cst.Mult -> "*"
  | Cst.Lt -> "<"
  | Cst.LtEq -> "<="
  | Cst.GtEq -> ">="
  | Cst.Eq -> "="
  | Cst.Neq -> "/="
  | Cst.And -> "/\\"
  | Cst.Or -> "\\/"

let rec cst_to_tla local_vars = function
  | Cst.IntLit { value; _ } -> TInt value
  | Cst.BoolLit { value; _ } -> TBool value
  | Cst.Var { name; _ } ->
      (* Locals live in the top stack frame; globals are plain variables *)
      if List.mem name local_vars then
        TDot (THead (TSubscript (TId "stack", TId "self")), name)
      else TId name
  | Cst.Self _ -> TId "self"
  | Cst.UnOp { op = Neg; rhs; _ } -> (
      match rhs with
      | Cst.IntLit { value; _ } -> TInt (-value)
      | _ -> TParens (TBinOp ("-", TInt 0, cst_to_tla local_vars rhs)))
  | Cst.BinOp { op; lhs; rhs; _ } ->
      TParens
        (TBinOp
           ( binop_to_tla op,
             cst_to_tla local_vars lhs,
             cst_to_tla local_vars rhs ))
  | Cst.App { name = "globally"; args; _ } -> (
      match args.items with
      | [ e ] -> TGlobally (cst_to_tla local_vars e)
      | _ -> failwith "globally takes exactly one argument")
  | Cst.App { name = "finally"; args; _ } -> (
      match args.items with
      | [ e ] -> TFinally (cst_to_tla local_vars e)
      | _ -> failwith "finally takes exactly one argument")
  | Cst.App { name = "head"; args; _ } -> (
      match args.items with
      | [ e ] -> THead (cst_to_tla local_vars e)
      | _ -> failwith "head takes exactly one argument")
  | Cst.App { name = "tail"; args; _ } -> (
      match args.items with
      | [ e ] -> TTail (cst_to_tla local_vars e)
      | _ -> failwith "tail takes exactly one argument")
  | Cst.App { name = "append"; args; _ } -> (
      match args.items with
      | [ seq; value ] ->
          TApp
            ( "Append",
              [ cst_to_tla local_vars seq; cst_to_tla local_vars value ] )
      | _ -> failwith "append takes exactly two arguments")
  | Cst.App { name = "concat"; args; _ } -> (
      match args.items with
      | [ lhs; rhs ] ->
          TConcat (cst_to_tla local_vars lhs, cst_to_tla local_vars rhs)
      | _ -> failwith "concat takes exactly two arguments")
  | Cst.App { name; args; _ } ->
      TApp (name, List.map (cst_to_tla local_vars) args.items)
  | Cst.Subscript { lhs; index; _ } ->
      TSubscript (cst_to_tla local_vars lhs, cst_to_tla local_vars index)
  | Cst.MapInit { binder; lo; hi; value; _ } ->
      TFuncMap
        ( binder,
          TRange (cst_to_tla local_vars lo, cst_to_tla local_vars hi),
          cst_to_tla local_vars value )
  | Cst.Tuple { elems; _ } ->
      TSeqLit (List.map (cst_to_tla local_vars) elems.items)
  | Cst.Sequence { elems; _ } ->
      TSeqLit (List.map (cst_to_tla local_vars) elems.items)
  | Cst.Paren { inner; _ } -> cst_to_tla local_vars inner

(* Module-level expressions have no local vars *)
let cst_to_tla_global = cst_to_tla []

type process_wrapper = { process : process_ir; proc : proc_ir }

let wrapper_proc_name (process : process_ir) =
  "__process_" ^ process.name ^ "_wrapper__"

let make_wrapper_source proc_name description (loc : Cst.loc) =
  { proc_name; description; line = loc.line; col = loc.col }

let wrapper_of_process (process : process_ir) : process_wrapper =
  let proc_name = wrapper_proc_name process in
  let entry_label = Ir.wrapper_entry_label process.name in
  let discard_label = Ir.wrapper_discard_label process.name in
  let entry_action =
    make_action ~label:entry_label ~pc_dest:(PcNext "__call__")
      ~stack_op:(StackPush (process.proc, discard_label, []))
      ~source:
        (make_wrapper_source proc_name
           ("[wrapper call " ^ process.proc ^ " for process " ^ process.name
          ^ "]")
           process.loc)
      ()
  in
  let discard_action =
    make_action ~label:discard_label ~pc_dest:(PcNext "Done")
      ~stack_op:StackDiscard
      ~source:
        (make_wrapper_source proc_name
           ("[wrapper discard return for process " ^ process.name ^ "]")
           process.loc)
      ()
  in
  {
    process;
    proc =
      {
        proc_name;
        params = [];
        actions = [ entry_action; discard_action ];
        entry_label;
      };
  }

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
  let to_tla = cst_to_tla local_vars in
  let self = TId "self" in
  let conjuncts = ref [] in
  let add c = conjuncts := !conjuncts @ [ c ] in
  let stack_self = TSubscript (TId "stack", self) in
  let stack_head = THead stack_self in
  let pc_dest_val () =
    match action.pc_dest with
    | PcNext l -> TStr l
    | PcBranch (cond, t, f) -> TIf (to_tla cond, TStr t, TStr f)
  in
  let set_pc v = add (TBinOp ("=", TPrimed (TId "pc"), TExcept (TId "pc", [ ([ SubSel self ], v) ]))) in
  add (TBinOp ("=", TSubscript (TId "pc", self), TStr action.label));
  (match action.guard with Some g -> add (to_tla g) | None -> ());
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
  List.iter
    (function
      | AssignVar (var, expr) when not (List.mem var local_vars) ->
          add (TBinOp ("=", TPrimed (TId var), to_tla expr))
      | AssignIndex (var, index, expr) when not (List.mem var local_vars) ->
          add
            (TBinOp
               ( "=",
                 TPrimed (TId var),
                 TExcept (TId var, [ ([ SubSel (to_tla index) ], to_tla expr) ])
               ))
      | _ -> ())
    action.assignments;
  let base =
    if local_updates = [] then stack_self
    else TExcept (stack_self, local_updates)
  in
  (* The new value of stack[self], if the action touches the stack at all *)
  let new_stack_self =
    match action.stack_op with
    | StackNone -> if local_updates = [] then None else Some base
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
            ([
               ("procedure", TStr proc_name); ("return_pc", TStr return_label);
             ]
            @ bind params args
            @ List.map (fun v -> (v, null_value)) other_fields)
        in
        Some (TConcat (TSeqLit [ frame ], base))
    | StackReturn retval ->
        (* Replace the departing frame with a record carrying only the return
           value; the caller's pop action consumes it. *)
        Some
          (TConcat
             (TSeqLit [ TRecord [ ("value", to_tla retval) ] ], TTail base))
    | StackDiscard -> Some (TTail base)
    | StackPopAssign var ->
        if List.mem var local_vars then
          (* After Tail the caller's frame is element 1 *)
          Some
            (TExcept
               ( TTail base,
                 [
                   ( [ SubSel (TInt 1); FieldSel var ],
                     TDot (stack_head, "value") );
                 ] ))
        else (
          add (TBinOp ("=", TPrimed (TId var), TDot (stack_head, "value")));
          Some (TTail base))
  in
  (match new_stack_self with
  | Some v ->
      add
        (TBinOp
           ( "=",
             TPrimed (TId "stack"),
             TExcept (TId "stack", [ ([ SubSel self ], v) ]) ))
  | None -> ());
  (match action.stack_op with
  | StackPush (proc_name, _, _) ->
      set_pc (TStr (List.assoc proc_name proc_entry_labels))
  | StackReturn _ -> set_pc (TDot (stack_head, "return_pc"))
  | StackDiscard | StackPopAssign _ | StackNone -> set_pc (pc_dest_val ()));
  let unchanged = compute_unchanged ir action in
  if unchanged <> [] then add (TUnchanged (List.map (fun v -> TId v) unchanged));
  DOpDef (action.label, [ "self" ], TConj (Block, !conjuncts))

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

let add_process_next add_next process proc_name =
  add_next
    (TParens
       (TExists
          ( "self",
            TRange (cst_to_tla_global process.lo, cst_to_tla_global process.hi),
            TApp (proc_name, [ TId "self" ]) )))

let generate_module ?(config = Config.default) (ir : module_ir) : tla_module =
  let process_wrappers = List.map wrapper_of_process ir.processes in
  let wrapper_procs = List.map (fun wrapper -> wrapper.proc) process_wrappers in
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
  let decls = ref [] in
  let add d = decls := !decls @ [ d ] in
  let add_sep () = add DSeparator in
  add (DExtends [ "TLC"; "Sequences"; "Integers" ]);
  add_sep ();
  let global_vars = List.map fst ir.var_decls in
  let all_tla_vars = ("pc" :: global_vars) @ [ "stack" ] in
  add (DVariables all_tla_vars);
  add_sep ();
  add (DOpDef ("vars", [], TSeqLit (List.map (fun v -> TId v) all_tla_vars)));
  add_sep ();
  List.iter
    (fun (name, expr) -> add (DOpDef (name, [], cst_to_tla_global expr)))
    ir.const_defs;
  if ir.const_defs <> [] then add_sep ();
  List.iter
    (fun (name, params, expr) ->
      add (DOpDef (name, params, cst_to_tla_global expr)))
    ir.fun_defs;
  if ir.fun_defs <> [] then add_sep ();
  let proc_set_parts =
    List.map
      (fun (p : process_ir) ->
        TParens (TRange (cst_to_tla_global p.lo, cst_to_tla_global p.hi)))
      ir.processes
  in
  add (DOpDef ("ProcSet", [], TCup proc_set_parts));
  add_sep ();
  let init_conjuncts = ref [] in
  let add_init c = init_conjuncts := !init_conjuncts @ [ c ] in
  List.iter
    (fun (name, expr) ->
      add_init (TBinOp ("=", TId name, cst_to_tla_global expr)))
    ir.var_decls;
  add_init
    (TBinOp ("=", TId "stack", TFuncMap ("self", TId "ProcSet", TSeqLit [])));
  (match ir.processes with
  | [ single ] ->
      let wrapper =
        List.find
          (fun (wrapper : process_wrapper) ->
            wrapper.process.name = single.name)
          process_wrappers
      in
      add_init
        (TBinOp
           ( "=",
             TId "pc",
             TFuncMap ("self", TId "ProcSet", TStr wrapper.proc.entry_label) ))
  | _ ->
      let cases =
        List.map
          (fun (p : process_ir) ->
            let wrapper =
              List.find
                (fun (candidate : process_wrapper) ->
                  candidate.process.name = p.name)
                process_wrappers
            in
            ( TIn
                ( TId "self",
                  TRange (cst_to_tla_global p.lo, cst_to_tla_global p.hi) ),
              TStr wrapper.proc.entry_label ))
          ir.processes
      in
      add_init
        (TBinOp ("=", TId "pc", TFuncMap ("self", TId "ProcSet", TCase cases))));
  add (DOpDef ("Init", [], TConj (Block, !init_conjuncts)));
  add_sep ();
  List.iter
    (fun proc ->
      List.iter add
        (proc_to_decls proc_entry_labels frame_fields ir.local_var_decls ir
           proc);
      add_sep ())
    all_runtime_procs;
  let procedure_procs = ir.procs in
  let next_disj = ref [] in
  let add_next d = next_disj := !next_disj @ [ d ] in
  (if procedure_procs <> [] then
     let proc_disj =
       TDisj
         ( Inline,
           List.map
             (fun (p : proc_ir) -> TApp (p.proc_name, [ TId "self" ]))
             procedure_procs )
     in
     add_next (TParens (TExists ("self", TId "ProcSet", proc_disj))));
  List.iter
    (fun (wrapper : process_wrapper) ->
      add_process_next add_next wrapper.process wrapper.proc.proc_name)
    process_wrappers;
  if config.checks.termination then add_next (TId "Terminating");
  if config.checks.termination then (
    add
      (DOpDef
         ( "Terminating",
           [],
           TConj
             ( Block,
               [
                 TForall
                   ( "self",
                     TId "ProcSet",
                     TBinOp ("=", TSubscript (TId "pc", TId "self"), TStr "Done")
                   );
                 TUnchangedExpr (TId "vars");
               ] ) ));
    add_sep ();
    add
      (DOpDef
         ( "Termination",
           [],
           TFinally
             (TForall
                ( "self",
                  TId "ProcSet",
                  TBinOp ("=", TSubscript (TId "pc", TId "self"), TStr "Done")
                )) ));
    add_sep ());
  add (DOpDef ("Next", [], TDisj (Block, !next_disj)));
  add_sep ();
  let fairness_conjuncts =
    List.concat_map
      (fun (wrapper : process_wrapper) ->
        if wrapper.process.fair then
          let process_range =
            TRange
              ( cst_to_tla_global wrapper.process.lo,
                cst_to_tla_global wrapper.process.hi )
          in
          let fairness_for proc_name =
            TParens
              (TForall
                 ( "self",
                   process_range,
                   TApp ("WF_vars", [ TApp (proc_name, [ TId "self" ]) ]) ))
          in
          fairness_for wrapper.proc.proc_name
          :: List.map
               (fun (proc : proc_ir) -> fairness_for proc.proc_name)
               ir.procs
        else [])
      process_wrappers
  in
  add
    (DOpDef
       ( "Spec",
         [],
         TConj
           ( Inline,
             [ TId "Init"; TBoxAction (TId "Next", TId "vars") ]
             @ fairness_conjuncts ) ));
  add_sep ();
  { name = ir.name; body = !decls }
