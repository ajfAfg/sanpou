open Ast
open Tla.Tla_ast

(* --- Label generation --- *)

let label_counter = ref 0

let fresh_label () =
  incr label_counter;
  "L" ^ string_of_int !label_counter

let reset_label_counter () = label_counter := 0

(* --- TLA+ action (one labeled step) --- *)

type stack_op =
  | StackNone
  | StackPush of string * string (* procedure name, return label — for Call *)
  | StackReturn of tla_expr (* return value *)
  | StackDiscard (* pop return value after call *)

type action = {
  label : string;
  guard : tla_expr option; (* await condition *)
  assignments : (string * tla_expr) list; (* var, expr pairs *)
  pc_next : string;
  stack_op : stack_op;
  unchanged : string list; (* variables unchanged *)
}

(* --- Collected state for codegen --- *)

type proc_info = {
  proc_name : string;
  params : id list;
  actions : action list;
  entry_label : string;
}

type process_info = {
  name : string;
  proc : string;
  lo : tla_expr;
  hi : tla_expr;
}

type codegen_state = {
  const_defs : (string * tla_expr) list;
  fun_defs : (string * string list * tla_expr) list;
  var_decls : (string * tla_expr) list;
  procs : proc_info list;
  processes : process_info list;
}

let empty_state =
  { const_defs = []; fun_defs = []; var_decls = []; procs = []; processes = [] }

(* --- Expression to TLA+ AST --- *)

let binop_to_tla = function
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Lt -> "<"
  | Eq -> "="

let rec expr_to_tla = function
  | IntLit i -> TInt i
  | BoolLit b -> TBool b
  | Var id -> TId id
  | BinOp (op, e1, e2) ->
      TParens (TBinOp (binop_to_tla op, expr_to_tla e1, expr_to_tla e2))
  | App (name, args) -> TApp (name, List.map expr_to_tla args)
  | Tuple es -> TSeqLit (List.map expr_to_tla es)

(* --- Compile body to list of actions --- *)

(* Context passed down during body compilation *)
type compile_ctx = {
  all_vars : string list; (* all global mutable variables *)
  break_label : string option; (* where to jump on Break *)
}

(* Result of compiling a body segment *)
type compiled = { actions : action list; entry : string; exit_label : string }

let make_action ~label ~pc_next ~assignments ~guard ~stack_op ~changed ~ctx =
  let unchanged =
    List.filter (fun v -> not (List.mem v changed)) ctx.all_vars
  in
  let unchanged =
    match stack_op with StackNone -> "stack" :: unchanged | _ -> unchanged
  in
  { label; guard; assignments; pc_next; stack_op; unchanged }

(* Compile a single step (list of stmts sharing one atomic label) *)
let rec compile_step ctx (stmts : step) (next_label : string) : compiled =
  let label = fresh_label () in
  match stmts with
  | [] ->
      let a =
        make_action ~label ~pc_next:next_label ~assignments:[] ~guard:None
          ~stack_op:StackNone ~changed:[] ~ctx
      in
      { actions = [ a ]; entry = label; exit_label = next_label }
  | [ Return expr ] ->
      let a =
        make_action ~label ~pc_next:"__return__" ~assignments:[] ~guard:None
          ~stack_op:(StackReturn (expr_to_tla expr))
          ~changed:[] ~ctx
      in
      { actions = [ a ]; entry = label; exit_label = next_label }
  | [ Break ] ->
      let break_target =
        match ctx.break_label with
        | Some l -> l
        | None -> failwith "break outside of loop"
      in
      let a =
        make_action ~label ~pc_next:break_target ~assignments:[] ~guard:None
          ~stack_op:StackNone ~changed:[] ~ctx
      in
      { actions = [ a ]; entry = label; exit_label = next_label }
  | [ Call (name, _args) ] ->
      let pop_label = fresh_label () in
      let call_action =
        make_action ~label ~pc_next:"__call__" ~assignments:[] ~guard:None
          ~stack_op:(StackPush (name, pop_label))
          ~changed:[] ~ctx
      in
      let pop_action =
        make_action ~label:pop_label ~pc_next:next_label ~assignments:[]
          ~guard:None ~stack_op:StackDiscard ~changed:[] ~ctx
      in
      {
        actions = [ call_action; pop_action ];
        entry = label;
        exit_label = next_label;
      }
  | [ While (cond, body) ] -> compile_while ctx label cond body next_label
  | [ If (cond, body) ] -> compile_if ctx label cond body next_label
  | _ ->
      (* General step: mix of assignments, await, calls *)
      compile_general_step ctx label stmts next_label

and compile_general_step ctx label (stmts : step) next_label =
  (* Collect all assignments and guards in this atomic step *)
  let guard = ref None in
  let assignments = ref [] in
  let changed = ref [] in
  let stack_op_ref = ref StackNone in
  let actual_next = ref next_label in
  let extra_actions = ref [] in
  List.iter
    (fun stmt ->
      match stmt with
      | Assign (id, e) ->
          assignments := !assignments @ [ (id, expr_to_tla e) ];
          changed := id :: !changed
      | Await cond -> guard := Some (expr_to_tla cond)
      | Call (name, _args) ->
          let pop_label = fresh_label () in
          stack_op_ref := StackPush (name, pop_label);
          actual_next := "__call__";
          let pop_action =
            make_action ~label:pop_label ~pc_next:next_label ~assignments:[]
              ~guard:None ~stack_op:StackDiscard ~changed:[] ~ctx
          in
          extra_actions := [ pop_action ]
      | Return expr ->
          stack_op_ref := StackReturn (expr_to_tla expr);
          actual_next := "__return__"
      | _ -> failwith "unsupported stmt in atomic step")
    stmts;
  let a =
    make_action ~label ~pc_next:!actual_next ~assignments:!assignments
      ~guard:!guard ~stack_op:!stack_op_ref ~changed:!changed ~ctx
  in
  { actions = [ a ] @ !extra_actions; entry = label; exit_label = next_label }

and compile_while ctx _outer_label cond body after_loop_label =
  (* while(cond) { body } translates to:
     - check label: if cond, go to body entry; else go to after_loop
     - body steps (with break → after_loop)
     - last body step loops back to check label *)
  let check_label = fresh_label () in
  let body_ctx = { ctx with break_label = Some after_loop_label } in
  let body_compiled = compile_body body_ctx body check_label in
  (* Check action: condition branch *)
  let check_true =
    make_action ~label:check_label ~pc_next:body_compiled.entry ~assignments:[]
      ~guard:(Some (expr_to_tla cond))
      ~stack_op:StackNone ~changed:[] ~ctx
  in
  let check_false_label = fresh_label () in
  let check_false =
    make_action ~label:check_false_label ~pc_next:after_loop_label
      ~assignments:[]
      ~guard:(Some (TNot (expr_to_tla cond)))
      ~stack_op:StackNone ~changed:[] ~ctx
  in
  {
    actions =
      [
        { check_true with label = check_label };
        { check_false with label = check_false_label };
      ]
      @ body_compiled.actions;
    entry = check_label;
    exit_label = after_loop_label;
  }

and compile_if ctx _outer_label cond body next_label =
  (* if(cond) { body } translates to:
     - check: if cond, go to body; else skip to next *)
  let check_label = fresh_label () in
  let body_compiled = compile_body ctx body next_label in
  let check_true =
    make_action ~label:check_label ~pc_next:body_compiled.entry ~assignments:[]
      ~guard:(Some (expr_to_tla cond))
      ~stack_op:StackNone ~changed:[] ~ctx
  in
  let check_false_label = fresh_label () in
  let check_false =
    make_action ~label:check_false_label ~pc_next:next_label ~assignments:[]
      ~guard:(Some (TNot (expr_to_tla cond)))
      ~stack_op:StackNone ~changed:[] ~ctx
  in
  {
    actions = [ check_true; check_false ] @ body_compiled.actions;
    entry = check_label;
    exit_label = next_label;
  }

and compile_body ctx (steps : body) (continuation : string) : compiled =
  (* Compile steps in reverse, threading continuation labels *)
  match steps with
  | [] ->
      (* Empty body: just go to continuation *)
      let label = fresh_label () in
      let a =
        make_action ~label ~pc_next:continuation ~assignments:[] ~guard:None
          ~stack_op:StackNone ~changed:[] ~ctx
      in
      { actions = [ a ]; entry = label; exit_label = continuation }
  | _ ->
      let rev_steps = List.rev steps in
      let first_step = List.hd rev_steps in
      let rest_steps = List.tl rev_steps in
      let compiled = compile_step ctx first_step continuation in
      List.fold_left
        (fun acc step ->
          let c = compile_step ctx step acc.entry in
          {
            actions = c.actions @ acc.actions;
            entry = c.entry;
            exit_label = acc.exit_label;
          })
        compiled rest_steps

(* --- Resolve labels: replace __call__ and __return__ with actual targets --- *)

let resolve_call_target procs action =
  match action.stack_op with
  | StackPush (proc_name, _return_label) ->
      let target_proc = List.find (fun p -> p.proc_name = proc_name) procs in
      { action with pc_next = target_proc.entry_label }
  | _ -> action

(* --- Build TLA+ AST --- *)

let action_to_decl proc_entry_labels (action : action) : tla_decl =
  let self = TId "self" in
  let conjuncts = ref [] in
  let add c = conjuncts := !conjuncts @ [ c ] in
  (* pc check *)
  add (TBinOp ("=", TSubscript (TId "pc", self), TStr action.label));
  (* Guard *)
  (match action.guard with
  | Some g -> add g
  | None -> ());
  (* Assignments *)
  List.iter
    (fun (var, expr) -> add (TBinOp ("=", TPrimed (TId var), expr)))
    action.assignments;
  (* Stack operation + pc update *)
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
      add
        (TBinOp
           ( "=",
             TPrimed (TId "pc"),
             TExcept (TId "pc", self, TStr action.pc_next) ));
      add
        (TBinOp
           ( "=",
             TPrimed (TId "stack"),
             TExcept (TId "stack", self, TTail (TSubscript (TId "stack", self)))
           ))
  | StackNone ->
      add
        (TBinOp
           ( "=",
             TPrimed (TId "pc"),
             TExcept (TId "pc", self, TStr action.pc_next) )));
  (* UNCHANGED *)
  if action.unchanged <> [] then
    add (TUnchanged (List.map (fun v -> TId v) action.unchanged));
  DOpDef (action.label, [ "self" ], TConj (Block, !conjuncts))

let proc_to_decls proc_entry_labels (proc : proc_info) : tla_decl list =
  let action_decls =
    List.concat_map
      (fun a -> [ action_to_decl proc_entry_labels a; DSeparator ])
      proc.actions
  in
  let action_labels = List.map (fun a -> a.label) proc.actions in
  let disj =
    DOpDef
      ( proc.proc_name,
        [ "self" ],
        TDisj
          (Inline, List.map (fun l -> TApp (l, [ TId "self" ])) action_labels)
      )
  in
  action_decls @ [ disj ]

let compile_module (m : module_def) : tla_module =
  reset_label_counter ();
  (* First pass: collect all items *)
  let state =
    List.fold_left
      (fun st item ->
        match item with
        | ConstDef (name, expr) ->
            {
              st with
              const_defs = st.const_defs @ [ (name, expr_to_tla expr) ];
            }
        | FunDef (name, params, expr) ->
            {
              st with
              fun_defs = st.fun_defs @ [ (name, params, expr_to_tla expr) ];
            }
        | VarDecl (name, expr) ->
            { st with var_decls = st.var_decls @ [ (name, expr_to_tla expr) ] }
        | ProcDef (name, params, body) ->
            let all_vars = List.map fst st.var_decls in
            let ctx = { all_vars; break_label = None } in
            let done_label = "Done" in
            let compiled = compile_body ctx body done_label in
            let proc_info =
              {
                proc_name = name;
                params;
                actions = compiled.actions;
                entry_label = compiled.entry;
              }
            in
            { st with procs = st.procs @ [ proc_info ] }
        | Process (name, proc, lo, hi) ->
            {
              st with
              processes =
                st.processes
                @ [ { name; proc; lo = expr_to_tla lo; hi = expr_to_tla hi } ];
            })
      empty_state m.items
  in
  (* Resolve call targets *)
  let procs : proc_info list =
    List.map
      (fun (p : proc_info) ->
        {
          p with
          actions = List.map (resolve_call_target state.procs) p.actions;
        })
      state.procs
  in
  let proc_entry_labels =
    List.map (fun p -> (p.proc_name, p.entry_label)) procs
  in
  (* Build declarations *)
  let decls = ref [] in
  let add d = decls := !decls @ [ d ] in
  let add_sep () = add DSeparator in
  (* Extends *)
  add (DExtends [ "TLC"; "Sequences"; "Integers" ]);
  add_sep ();
  (* Constant definitions *)
  List.iter (fun (name, expr) -> add (DOpDef (name, [], expr))) state.const_defs;
  if state.const_defs <> [] then add_sep ();
  (* Function definitions *)
  List.iter
    (fun (name, params, expr) -> add (DOpDef (name, params, expr)))
    state.fun_defs;
  if state.fun_defs <> [] then add_sep ();
  (* Variables *)
  let global_vars = List.map fst state.var_decls in
  let all_tla_vars = ("pc" :: global_vars) @ [ "stack" ] in
  add (DVariables all_tla_vars);
  add_sep ();
  add (DOpDef ("vars", [], TSeqLit (List.map (fun v -> TId v) all_tla_vars)));
  add_sep ();
  (* ProcSet *)
  let proc_set_parts =
    List.map (fun p -> TParens (TRange (p.lo, p.hi))) state.processes
  in
  add (DOpDef ("ProcSet", [], TCup proc_set_parts));
  add_sep ();
  (* Init *)
  let init_conjuncts = ref [] in
  let add_init c = init_conjuncts := !init_conjuncts @ [ c ] in
  List.iter
    (fun (name, expr) -> add_init (TBinOp ("=", TId name, expr)))
    state.var_decls;
  add_init
    (TBinOp ("=", TId "stack", TFuncMap ("self", TId "ProcSet", TSeqLit [])));
  (match state.processes with
  | [ single ] ->
      let proc = List.find (fun p -> p.proc_name = single.proc) procs in
      add_init
        (TBinOp
           ( "=",
             TId "pc",
             TFuncMap ("self", TId "ProcSet", TStr proc.entry_label) ))
  | _ ->
      let cases =
        List.map
          (fun p ->
            let proc = List.find (fun pr -> pr.proc_name = p.proc) procs in
            (TIn (TId "self", TRange (p.lo, p.hi)), TStr proc.entry_label))
          state.processes
      in
      add_init
        (TBinOp ("=", TId "pc", TFuncMap ("self", TId "ProcSet", TCase cases))));
  add (DOpDef ("Init", [], TConj (Block, !init_conjuncts)));
  add_sep ();
  (* Procedure definitions *)
  List.iter
    (fun proc ->
      List.iter add (proc_to_decls proc_entry_labels proc);
      add_sep ())
    procs;
  (* Next *)
  let procedure_procs =
    List.filter
      (fun p ->
        not (List.exists (fun proc -> proc.proc = p.proc_name) state.processes))
      procs
  in
  let process_procs =
    List.filter
      (fun p ->
        List.exists (fun proc -> proc.proc = p.proc_name) state.processes)
      procs
  in
  let next_disj = ref [] in
  let add_next d = next_disj := !next_disj @ [ d ] in
  if procedure_procs <> [] then (
    let proc_disj =
      TDisj
        ( Inline,
          List.map (fun p -> TApp (p.proc_name, [ TId "self" ])) procedure_procs
        )
    in
    add_next (TParens (TExists ("self", TId "ProcSet", proc_disj)));
    List.iter
      (fun p ->
        let range =
          List.find (fun pr -> pr.proc = p.proc_name) state.processes
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
      (fun p ->
        let range =
          List.find (fun pr -> pr.proc = p.proc_name) state.processes
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
  (* Spec *)
  add
    (DOpDef
       ( "Spec",
         [],
         TConj (Inline, [ TId "Init"; TBoxAction (TId "Next", TId "vars") ]) ));
  add_sep ();
  { name = m.mod_name; body = !decls }

let compile (program : program) : tla_module list =
  List.map compile_module program
