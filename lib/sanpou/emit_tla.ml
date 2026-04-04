open Tla.Tla_ast
open Ir

(* ===== Action IR → TLA+ AST ===== *)

let action_to_decl proc_entry_labels local_vars (action : action) : tla_decl =
  let self = TId "self" in
  let conjuncts = ref [] in
  let add c = conjuncts := !conjuncts @ [ c ] in
  add (TBinOp ("=", TSubscript (TId "pc", self), TStr action.label));
  (match action.guard with Some g -> add g | None -> ());
  List.iter
    (fun (var, expr) ->
      if List.mem var local_vars then
        add (TBinOp ("=", TPrimed (TId var), TExcept (TId var, self, expr)))
      else add (TBinOp ("=", TPrimed (TId var), expr)))
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
                   (TSeqLit [ retval ], TTail (TSubscript (TId "stack", self)))
               ) ))
  | StackDiscard ->
      let pc_val =
        match action.pc_dest with
        | PcNext l -> TStr l
        | PcBranch (cond, t, f) -> TIf (cond, TStr t, TStr f)
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
        | PcBranch (cond, t, f) -> TIf (cond, TStr t, TStr f)
      in
      add (TBinOp ("=", TPrimed (TId "pc"), TExcept (TId "pc", self, pc_val))));
  if action.unchanged <> [] then
    add (TUnchanged (List.map (fun v -> TId v) action.unchanged));
  DOpDef (action.label, [ "self" ], TConj (Block, !conjuncts))

let proc_to_decls proc_entry_labels local_vars (proc : proc_ir) : tla_decl list
    =
  let action_decls =
    List.concat_map
      (fun a -> [ action_to_decl proc_entry_labels local_vars a; DSeparator ])
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
  List.iter (fun (name, expr) -> add (DOpDef (name, [], expr))) ir.const_defs;
  if ir.const_defs <> [] then add_sep ();
  List.iter
    (fun (name, params, expr) -> add (DOpDef (name, params, expr)))
    ir.fun_defs;
  if ir.fun_defs <> [] then add_sep ();
  let global_vars = List.map fst ir.var_decls in
  let all_tla_vars = ("pc" :: global_vars) @ ir.local_var_decls @ [ "stack" ] in
  add (DVariables all_tla_vars);
  add_sep ();
  add (DOpDef ("vars", [], TSeqLit (List.map (fun v -> TId v) all_tla_vars)));
  add_sep ();
  let proc_set_parts =
    List.map
      (fun (p : process_ir) -> TParens (TRange (p.lo, p.hi)))
      ir.processes
  in
  add (DOpDef ("ProcSet", [], TCup proc_set_parts));
  add_sep ();
  let init_conjuncts = ref [] in
  let add_init c = init_conjuncts := !init_conjuncts @ [ c ] in
  List.iter
    (fun (name, expr) -> add_init (TBinOp ("=", TId name, expr)))
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
            (TIn (TId "self", TRange (p.lo, p.hi)), TStr proc.entry_label))
          ir.processes
      in
      add_init
        (TBinOp ("=", TId "pc", TFuncMap ("self", TId "ProcSet", TCase cases))));
  add (DOpDef ("Init", [], TConj (Block, !init_conjuncts)));
  add_sep ();
  List.iter
    (fun proc ->
      List.iter add (proc_to_decls proc_entry_labels ir.local_var_decls proc);
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
                  TRange (range.lo, range.hi),
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
                  TRange (range.lo, range.hi),
                  TApp (p.proc_name, [ TId "self" ]) ))))
      process_procs;
  add (DOpDef ("Next", [], TDisj (Block, !next_disj)));
  add_sep ();
  add
    (DOpDef
       ( "Spec",
         [],
         TConj (Inline, [ TId "Init"; TBoxAction (TId "Next", TId "vars") ]) ));
  add_sep ();
  { name = ir.name; body = !decls }
