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
      if List.mem name local_vars then TSubscript (TId name, TId "self")
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

let wrapper_entry_label (process : process_ir) =
  "__w_" ^ process.name ^ "_entry__"

let wrapper_discard_label (process : process_ir) =
  "__w_" ^ process.name ^ "_discard__"

let make_wrapper_source proc_name description =
  { proc_name; description; line = 0; col = 0 }

let wrapper_of_process (process : process_ir) : process_wrapper =
  let proc_name = wrapper_proc_name process in
  let entry_label = wrapper_entry_label process in
  let discard_label = wrapper_discard_label process in
  let entry_action =
    {
      label = entry_label;
      guard = None;
      assignments = [];
      pc_dest = PcNext "__call__";
      stack_op = StackPush (process.proc, discard_label, []);
      source =
        make_wrapper_source proc_name
          ("[wrapper call " ^ process.proc ^ " for process " ^ process.name
         ^ "]");
    }
  in
  let discard_action =
    {
      label = discard_label;
      guard = None;
      assignments = [];
      pc_dest = PcNext "Done";
      stack_op = StackDiscard;
      source =
        make_wrapper_source proc_name
          ("[wrapper discard return for process " ^ process.name ^ "]");
    }
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
  let all_vars = global_vars @ ir.local_var_decls in
  let assigned =
    List.map
      (function
        | AssignVar (name, _) -> name | AssignIndex (name, _, _) -> name)
      action.assignments
  in
  let unchanged = List.filter (fun v -> not (List.mem v assigned)) all_vars in
  match action.stack_op with
  | StackNone -> "stack" :: unchanged
  | _ -> unchanged

(* ===== Action IR → TLA+ AST ===== *)

let action_to_decl proc_entry_labels local_vars (ir : module_ir)
    (action : action) : tla_decl =
  let to_tla = cst_to_tla local_vars in
  let self = TId "self" in
  let conjuncts = ref [] in
  let add c = conjuncts := !conjuncts @ [ c ] in
  add (TBinOp ("=", TSubscript (TId "pc", self), TStr action.label));
  (match action.guard with Some g -> add (to_tla g) | None -> ());
  List.iter
    (function
      | AssignVar (var, expr) ->
          if List.mem var local_vars then
            add
              (TBinOp
                 ("=", TPrimed (TId var), TExcept (TId var, self, to_tla expr)))
          else add (TBinOp ("=", TPrimed (TId var), to_tla expr))
      | AssignIndex (var, index, expr) ->
          if List.mem var local_vars then
            let local_collection = TSubscript (TId var, self) in
            let updated_local =
              TExcept (local_collection, to_tla index, to_tla expr)
            in
            add
              (TBinOp
                 ("=", TPrimed (TId var), TExcept (TId var, self, updated_local)))
          else
            add
              (TBinOp
                 ( "=",
                   TPrimed (TId var),
                   TExcept (TId var, to_tla index, to_tla expr) )))
    action.assignments;
  (match action.stack_op with
  | StackPush (proc_name, return_label, _) ->
      let entry = List.assoc proc_name proc_entry_labels in
      add
        (TBinOp
           ( "=",
             TPrimed (TId "stack"),
             TExcept
               ( TId "stack",
                 self,
                 TConcat
                   ( TSeqLit
                       [
                         TRecord
                           [
                             ("procedure", TStr proc_name);
                             ("return_pc", TStr return_label);
                           ];
                       ],
                     TSubscript (TId "stack", self) ) ) ));
      add
        (TBinOp ("=", TPrimed (TId "pc"), TExcept (TId "pc", self, TStr entry)))
  | StackReturn retval ->
      add
        (TBinOp
           ( "=",
             TPrimed (TId "pc"),
             TExcept
               ( TId "pc",
                 self,
                 TDot (THead (TSubscript (TId "stack", self)), "return_pc") ) ));
      add
        (TBinOp
           ( "=",
             TPrimed (TId "stack"),
             TExcept
               ( TId "stack",
                 self,
                 TConcat
                   ( TSeqLit [ to_tla retval ],
                     TTail (TSubscript (TId "stack", self)) ) ) ))
  | StackDiscard ->
      let pc_val =
        match action.pc_dest with
        | PcNext l -> TStr l
        | PcBranch (cond, t, f) -> TIf (to_tla cond, TStr t, TStr f)
      in
      add (TBinOp ("=", TPrimed (TId "pc"), TExcept (TId "pc", self, pc_val)));
      add
        (TBinOp
           ( "=",
             TPrimed (TId "stack"),
             TExcept (TId "stack", self, TTail (TSubscript (TId "stack", self)))
           ))
  | StackNone ->
      let pc_val =
        match action.pc_dest with
        | PcNext l -> TStr l
        | PcBranch (cond, t, f) -> TIf (to_tla cond, TStr t, TStr f)
      in
      add (TBinOp ("=", TPrimed (TId "pc"), TExcept (TId "pc", self, pc_val))));
  let unchanged = compute_unchanged ir action in
  if unchanged <> [] then add (TUnchanged (List.map (fun v -> TId v) unchanged));
  DOpDef (action.label, [ "self" ], TConj (Block, !conjuncts))

let proc_to_decls proc_entry_labels local_vars (ir : module_ir) (proc : proc_ir)
    : tla_decl list =
  let action_decls =
    List.concat_map
      (fun a ->
        [ action_to_decl proc_entry_labels local_vars ir a; DSeparator ])
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
  let decls = ref [] in
  let add d = decls := !decls @ [ d ] in
  let add_sep () = add DSeparator in
  add (DExtends [ "TLC"; "Sequences"; "Integers" ]);
  add_sep ();
  let global_vars = List.map fst ir.var_decls in
  let all_tla_vars = ("pc" :: global_vars) @ ir.local_var_decls @ [ "stack" ] in
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
  List.iter
    (fun name ->
      add_init
        (TBinOp ("=", TId name, TFuncMap ("self", TId "ProcSet", TInt 0))))
    ir.local_var_decls;
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
      List.iter add (proc_to_decls proc_entry_labels ir.local_var_decls ir proc);
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
