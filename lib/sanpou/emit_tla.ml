open Tla.Tla_ast
open Ir

(* ===== CST expression → TLA+ expression ===== *)

let binop_to_tla = function
  | Cst.Plus -> "+"
  | Cst.Minus -> "-"
  | Cst.Mult -> "*"
  | Cst.Lt -> "<"
  | Cst.LtEq -> "<="
  | Cst.GtEq -> ">="
  | Cst.Eq -> "="
  | Cst.And -> "/\\"

let rec cst_to_tla local_vars = function
  | Cst.IntLit { value; _ } -> TInt value
  | Cst.BoolLit { value; _ } -> TBool value
  | Cst.Var { name; _ } ->
      if List.mem name local_vars then TSubscript (TId name, TId "self")
      else TId name
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
  | Cst.App { name; args; _ } ->
      TApp (name, List.map (cst_to_tla local_vars) args.items)
  | Cst.Tuple { elems; _ } ->
      TSeqLit (List.map (cst_to_tla local_vars) elems.items)
  | Cst.Paren { inner; _ } -> cst_to_tla local_vars inner

(* Module-level expressions have no local vars *)
let cst_to_tla_global = cst_to_tla []

(* ===== UNCHANGED computation ===== *)

let compute_unchanged (ir : module_ir) (action : action) : string list =
  let global_vars = List.map fst ir.var_decls in
  let all_vars = global_vars @ ir.local_var_decls in
  let assigned = List.map fst action.assignments in
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
    (fun (var, expr) ->
      if List.mem var local_vars then
        add
          (TBinOp ("=", TPrimed (TId var), TExcept (TId var, self, to_tla expr)))
      else add (TBinOp ("=", TPrimed (TId var), to_tla expr)))
    action.assignments;
  (match action.stack_op with
  | StackPush (proc_name, return_label) ->
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
                             ("pc", TStr return_label);
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
                 TDot (THead (TSubscript (TId "stack", self)), "pc") ) ));
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

let generate_module (ir : module_ir) : tla_module =
  let proc_entry_labels =
    List.map (fun (p : proc_ir) -> (p.proc_name, p.entry_label)) ir.procs
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
      let proc =
        List.find (fun (p : proc_ir) -> p.proc_name = single.proc) ir.procs
      in
      add_init
        (TBinOp
           ( "=",
             TId "pc",
             TFuncMap ("self", TId "ProcSet", TStr proc.entry_label) ))
  | _ ->
      let cases =
        List.map
          (fun (p : process_ir) ->
            let proc =
              List.find (fun (pr : proc_ir) -> pr.proc_name = p.proc) ir.procs
            in
            ( TIn
                ( TId "self",
                  TRange (cst_to_tla_global p.lo, cst_to_tla_global p.hi) ),
              TStr proc.entry_label ))
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
    ir.procs;
  let procedure_procs =
    List.filter
      (fun (p : proc_ir) ->
        not
          (List.exists
             (fun (proc : process_ir) -> proc.proc = p.proc_name)
             ir.processes))
      ir.procs
  in
  let process_procs =
    List.filter
      (fun (p : proc_ir) ->
        List.exists
          (fun (proc : process_ir) -> proc.proc = p.proc_name)
          ir.processes)
      ir.procs
  in
  let next_disj = ref [] in
  let add_next d = next_disj := !next_disj @ [ d ] in
  if procedure_procs <> [] then (
    let proc_disj =
      TDisj
        ( Inline,
          List.map
            (fun (p : proc_ir) -> TApp (p.proc_name, [ TId "self" ]))
            procedure_procs )
    in
    add_next (TParens (TExists ("self", TId "ProcSet", proc_disj)));
    List.iter
      (fun (p : proc_ir) ->
        let range =
          List.find
            (fun (pr : process_ir) -> pr.proc = p.proc_name)
            ir.processes
        in
        add_next
          (TParens
             (TExists
                ( "self",
                  TRange (cst_to_tla_global range.lo, cst_to_tla_global range.hi),
                  TApp (p.proc_name, [ TId "self" ]) ))))
      process_procs)
  else
    List.iter
      (fun (p : proc_ir) ->
        let range =
          List.find
            (fun (pr : process_ir) -> pr.proc = p.proc_name)
            ir.processes
        in
        add_next
          (TParens
             (TExists
                ( "self",
                  TRange (cst_to_tla_global range.lo, cst_to_tla_global range.hi),
                  TApp (p.proc_name, [ TId "self" ]) ))))
      process_procs;
  add (DOpDef ("Next", [], TDisj (Block, !next_disj)));
  add_sep ();
  let fairness_conjuncts =
    List.filter_map
      (fun (p : process_ir) ->
        if p.fair then
          Some
            (TForall
               ( "self",
                 TRange (cst_to_tla_global p.lo, cst_to_tla_global p.hi),
                 TApp ("WF_vars", [ TApp (p.proc, [ TId "self" ]) ]) ))
        else None)
      ir.processes
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
