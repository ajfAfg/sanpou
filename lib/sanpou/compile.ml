open Cst
open Tla.Tla_ast

(* ===== Source info for source map ===== *)

type source_info = {
  proc_name : string;
  description : string;
  line : int;
  col : int;
}

(* ===== Action IR ===== *)

type stack_op =
  | StackNone
  | StackPush of string * string (* procedure name, return label *)
  | StackReturn of tla_expr (* return value *)
  | StackDiscard (* pop return value after call *)

type pc_dest = PcNext of string | PcBranch of tla_expr * string * string

type action = {
  label : string;
  guard : tla_expr option;
  assignments : (string * tla_expr) list;
  pc_dest : pc_dest;
  stack_op : stack_op;
  unchanged : string list;
  source : source_info;
}

type proc_ir = {
  proc_name : string;
  params : id list;
  actions : action list;
  entry_label : string;
}

type process_ir = { name : string; proc : string; lo : tla_expr; hi : tla_expr }

type module_ir = {
  name : string;
  const_defs : (string * tla_expr) list;
  fun_defs : (string * string list * tla_expr) list;
  var_decls : (string * tla_expr) list;
  local_var_decls : string list;
  procs : proc_ir list;
  processes : process_ir list;
}

(* ===== Label generation ===== *)

let label_counter = ref 0

let fresh_label () =
  incr label_counter;
  "L" ^ string_of_int !label_counter

let reset_label_counter () = label_counter := 0

(* ===== Variable name generation for local vars ===== *)

let var_name_counter = ref 0

let fresh_var_name base =
  incr var_name_counter;
  base ^ "__" ^ string_of_int !var_name_counter

let reset_var_name_counter () = var_name_counter := 0

(* Module-level accumulator for local variable TLA+ names *)
let module_local_vars : string list ref = ref []
let reset_module_local_vars () = module_local_vars := []
let collect_local_var name = module_local_vars := name :: !module_local_vars
let get_module_local_vars () = List.rev !module_local_vars
let is_local_var name = List.mem name !module_local_vars

let resolve_name env name =
  match List.assoc_opt name env with Some tla_name -> tla_name | None -> name

(* ===== CST expression → TLA+ expression ===== *)

let binop_to_tla = function
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Lt -> "<"
  | Eq -> "="

let rec expr_to_tla = function
  | IntLit { value; _ } -> TInt value
  | BoolLit { value; _ } -> TBool value
  | Var { name; _ } -> TId name
  | BinOp { op; lhs; rhs; _ } ->
      TParens (TBinOp (binop_to_tla op, expr_to_tla lhs, expr_to_tla rhs))
  | App { name; args; _ } -> TApp (name, List.map expr_to_tla args.items)
  | Tuple { elems; _ } -> TSeqLit (List.map expr_to_tla elems.items)
  | Paren { inner; _ } -> expr_to_tla inner

(* ===== Compile context ===== *)

type compile_ctx = {
  proc_name : string;
  all_vars : string list;
  name_env : (string * string) list;
  break_label : string option;
}

type compiled = { actions : action list; entry : string; exit_label : string }

let rec expr_to_tla_ctx ctx = function
  | IntLit { value; _ } -> TInt value
  | BoolLit { value; _ } -> TBool value
  | Var { name; _ } ->
      let tla_name = resolve_name ctx.name_env name in
      if is_local_var tla_name then TSubscript (TId tla_name, TId "self")
      else TId tla_name
  | BinOp { op; lhs; rhs; _ } ->
      TParens
        (TBinOp
           (binop_to_tla op, expr_to_tla_ctx ctx lhs, expr_to_tla_ctx ctx rhs))
  | App { name; args; _ } ->
      TApp (name, List.map (expr_to_tla_ctx ctx) args.items)
  | Tuple { elems; _ } -> TSeqLit (List.map (expr_to_tla_ctx ctx) elems.items)
  | Paren { inner; _ } -> expr_to_tla_ctx ctx inner

let make_source ~proc_name ~description ~(loc : loc) =
  { proc_name; description; line = loc.line; col = loc.col }

let make_action ~label ~pc_next ~assignments ~guard ~stack_op ~changed ~ctx
    ~source =
  let unchanged =
    List.filter (fun v -> not (List.mem v changed)) ctx.all_vars
  in
  let unchanged =
    match stack_op with StackNone -> "stack" :: unchanged | _ -> unchanged
  in
  {
    label;
    guard;
    assignments;
    pc_dest = PcNext pc_next;
    stack_op;
    unchanged;
    source;
  }

(* ===== Step description generation (using pretty printer) ===== *)

let describe_simple_stmts (stmts : simple_stmt comma_list) =
  String.concat ", " (List.map Cst_printer.pretty_simple_stmt stmts.items)

let describe_block_check (bs : block_stmt) =
  match bs with
  | While { cond; _ } -> "while (" ^ Cst_printer.pretty_expr cond ^ ") [check]"
  | If { cond; _ } -> "if (" ^ Cst_printer.pretty_expr cond ^ ") [check]"

let describe_while_wait (cond : expr) =
  "while (" ^ Cst_printer.pretty_expr cond ^ ") [wait]"

(* ===== CST → Action IR compilation ===== *)

(* Convert CST simple_stmt to individual assignment/guard/call operations *)
let compile_simple_stmt ctx label (stmt : simple_stmt) ~guard ~assignments
    ~changed ~stack_op_ref ~actual_next ~extra_actions ~next_label ~source =
  match stmt with
  | Assign { name; value; _ } ->
      let tla_name = resolve_name ctx.name_env name in
      assignments := !assignments @ [ (tla_name, expr_to_tla_ctx ctx value) ];
      changed := tla_name :: !changed
  | Await { cond; _ } -> guard := Some (expr_to_tla_ctx ctx cond)
  | Call { name; _ } ->
      let pop_label = fresh_label () in
      stack_op_ref := StackPush (name, pop_label);
      actual_next := "__call__";
      let pop_source =
        { source with description = "[return from " ^ name ^ "]" }
      in
      let pop_action =
        make_action ~label:pop_label ~pc_next:next_label ~assignments:[]
          ~guard:None ~stack_op:StackDiscard ~changed:[] ~ctx ~source:pop_source
      in
      extra_actions := [ pop_action ]
  | Return { value; _ } ->
      stack_op_ref := StackReturn (expr_to_tla_ctx ctx value);
      actual_next := "__return__"
  | Break _ ->
      let break_target =
        match ctx.break_label with
        | Some l -> l
        | None -> failwith "break outside of loop"
      in
      ignore label;
      actual_next := break_target

let rec compile_step ctx (step : Cst.step) (next_label : string) : compiled =
  let label = fresh_label () in
  match step with
  | EmptyStep { loc; _ } ->
      let source = make_source ~proc_name:ctx.proc_name ~description:";" ~loc in
      let a =
        make_action ~label ~pc_next:next_label ~assignments:[] ~guard:None
          ~stack_op:StackNone ~changed:[] ~ctx ~source
      in
      { actions = [ a ]; entry = label; exit_label = next_label }
  | SimpleStep { stmts; loc; _ } -> (
      let source =
        make_source ~proc_name:ctx.proc_name
          ~description:(describe_simple_stmts stmts)
          ~loc
      in
      match stmts.items with
      | [ Return { value; _ } ] ->
          let a =
            make_action ~label ~pc_next:"__return__" ~assignments:[] ~guard:None
              ~stack_op:(StackReturn (expr_to_tla_ctx ctx value))
              ~changed:[] ~ctx ~source
          in
          { actions = [ a ]; entry = label; exit_label = next_label }
      | [ Break _ ] ->
          let break_target =
            match ctx.break_label with
            | Some l -> l
            | None -> failwith "break outside of loop"
          in
          let a =
            make_action ~label ~pc_next:break_target ~assignments:[] ~guard:None
              ~stack_op:StackNone ~changed:[] ~ctx ~source
          in
          { actions = [ a ]; entry = label; exit_label = next_label }
      | [ Call { name; _ } ] ->
          let pop_label = fresh_label () in
          let call_action =
            make_action ~label ~pc_next:"__call__" ~assignments:[] ~guard:None
              ~stack_op:(StackPush (name, pop_label))
              ~changed:[] ~ctx ~source
          in
          let pop_source =
            { source with description = "[return from " ^ name ^ "]" }
          in
          let pop_action =
            make_action ~label:pop_label ~pc_next:next_label ~assignments:[]
              ~guard:None ~stack_op:StackDiscard ~changed:[] ~ctx
              ~source:pop_source
          in
          {
            actions = [ call_action; pop_action ];
            entry = label;
            exit_label = next_label;
          }
      | _ ->
          (* General step: mix of assignments, await, calls *)
          let guard = ref None in
          let assignments = ref [] in
          let changed = ref [] in
          let stack_op_ref = ref StackNone in
          let actual_next = ref next_label in
          let extra_actions = ref [] in
          List.iter
            (fun stmt ->
              compile_simple_stmt ctx label stmt ~guard ~assignments ~changed
                ~stack_op_ref ~actual_next ~extra_actions ~next_label ~source)
            stmts.items;
          let a =
            make_action ~label ~pc_next:!actual_next ~assignments:!assignments
              ~guard:!guard ~stack_op:!stack_op_ref ~changed:!changed ~ctx
              ~source
          in
          {
            actions = [ a ] @ !extra_actions;
            entry = label;
            exit_label = next_label;
          })
  | BlockStep { stmt = While { cond; body; _ }; loc } ->
      compile_while ctx cond body next_label loc
  | BlockStep { stmt = If { cond; body; _ }; loc } ->
      compile_if ctx cond body next_label loc
  | WhileWait { cond; loc; _ } -> compile_while ctx cond [] next_label loc
  | LetStep _ -> failwith "LetStep should be handled in compile_body"

and compile_while ctx cond body after_loop_label loc =
  let check_label = fresh_label () in
  let body_ctx = { ctx with break_label = Some after_loop_label } in
  let body_compiled = compile_body body_ctx body check_label in
  let desc = "while (" ^ Cst_printer.pretty_expr cond ^ ") [check]" in
  let source = make_source ~proc_name:ctx.proc_name ~description:desc ~loc in
  let unchanged = "stack" :: ctx.all_vars in
  let check_action =
    {
      label = check_label;
      guard = None;
      assignments = [];
      pc_dest =
        PcBranch
          (expr_to_tla_ctx ctx cond, body_compiled.entry, after_loop_label);
      stack_op = StackNone;
      unchanged;
      source;
    }
  in
  {
    actions = [ check_action ] @ body_compiled.actions;
    entry = check_label;
    exit_label = after_loop_label;
  }

and compile_if ctx cond body next_label loc =
  let check_label = fresh_label () in
  let body_compiled = compile_body ctx body next_label in
  let desc = "if (" ^ Cst_printer.pretty_expr cond ^ ") [check]" in
  let source = make_source ~proc_name:ctx.proc_name ~description:desc ~loc in
  let unchanged = "stack" :: ctx.all_vars in
  let check_action =
    {
      label = check_label;
      guard = None;
      assignments = [];
      pc_dest =
        PcBranch (expr_to_tla_ctx ctx cond, body_compiled.entry, next_label);
      stack_op = StackNone;
      unchanged;
      source;
    }
  in
  {
    actions = [ check_action ] @ body_compiled.actions;
    entry = check_label;
    exit_label = next_label;
  }

and compile_body ctx (steps : Cst.body) (continuation : string) : compiled =
  match steps with
  | [] ->
      let label = fresh_label () in
      let source =
        make_source ~proc_name:ctx.proc_name ~description:"[empty body]"
          ~loc:{ line = 0; col = 0 }
      in
      let a =
        make_action ~label ~pc_next:continuation ~assignments:[] ~guard:None
          ~stack_op:StackNone ~changed:[] ~ctx ~source
      in
      { actions = [ a ]; entry = label; exit_label = continuation }
  | _ ->
      (* Forward pass: build per-step env annotations *)
      let _, annotated_steps =
        List.fold_left
          (fun (env, acc) step ->
            match step with
            | LetStep { name; _ } ->
                let tla_name = fresh_var_name name in
                collect_local_var tla_name;
                let ann = (step, env, Some tla_name) in
                let new_env = (name, tla_name) :: env in
                (new_env, acc @ [ ann ])
            | _ -> (env, acc @ [ (step, env, None) ]))
          (ctx.name_env, []) steps
      in
      (* Backward pass: compile with per-step env *)
      let compile_annotated (step, env, let_info) next_lbl =
        let step_ctx = { ctx with name_env = env } in
        match let_info with
        | Some tla_name -> compile_let_step step_ctx tla_name step next_lbl
        | None -> compile_step step_ctx step next_lbl
      in
      let rev_annotated = List.rev annotated_steps in
      let first = List.hd rev_annotated in
      let rest = List.tl rev_annotated in
      let compiled = compile_annotated first continuation in
      List.fold_left
        (fun acc ann ->
          let c = compile_annotated ann acc.entry in
          {
            actions = c.actions @ acc.actions;
            entry = c.entry;
            exit_label = acc.exit_label;
          })
        compiled rest

and compile_let_step ctx tla_name step next_label =
  let label = fresh_label () in
  match step with
  | LetStep { name; value; loc; _ } ->
      let init_expr = expr_to_tla_ctx ctx value in
      let source =
        make_source ~proc_name:ctx.proc_name
          ~description:("let " ^ name ^ " = " ^ Cst_printer.pretty_expr value)
          ~loc
      in
      let a =
        make_action ~label ~pc_next:next_label
          ~assignments:[ (tla_name, init_expr) ]
          ~guard:None ~stack_op:StackNone ~changed:[ tla_name ] ~ctx ~source
      in
      { actions = [ a ]; entry = label; exit_label = next_label }
  | _ -> failwith "compile_let_step called on non-LetStep"

(* ===== Resolve call targets ===== *)

let resolve_call_target procs action =
  match action.stack_op with
  | StackPush (proc_name, _) ->
      let target_proc =
        List.find (fun (p : proc_ir) -> p.proc_name = proc_name) procs
      in
      { action with pc_dest = PcNext target_proc.entry_label }
  | _ -> action

(* ===== CST module → module_ir ===== *)

let compile_module_ir (m : Cst.module_def) : module_ir =
  reset_label_counter ();
  reset_var_name_counter ();
  reset_module_local_vars ();
  let const_defs = ref [] in
  let fun_defs = ref [] in
  let var_decls = ref [] in
  let procs = ref [] in
  let processes = ref [] in
  List.iter
    (fun (item : Cst.item) ->
      match item with
      | ConstDef { name; value; _ } ->
          const_defs := !const_defs @ [ (name, expr_to_tla value) ]
      | FunDef { name; params; body_expr; _ } ->
          fun_defs :=
            !fun_defs
            @ [ (name, List.map snd params.items, expr_to_tla body_expr) ]
      | VarDecl { name; value; _ } ->
          var_decls := !var_decls @ [ (name, expr_to_tla value) ]
      | ProcDef { name; params; body; _ } ->
          let all_vars = List.map fst !var_decls in
          let ctx =
            { proc_name = name; all_vars; name_env = []; break_label = None }
          in
          let done_label = "Done" in
          let compiled = compile_body ctx body done_label in
          let proc =
            {
              proc_name = name;
              params = List.map snd params.items;
              actions = compiled.actions;
              entry_label = compiled.entry;
            }
          in
          procs := !procs @ [ proc ]
      | Process { name; proc; lo; hi; _ } ->
          processes :=
            !processes
            @ [ { name; proc; lo = expr_to_tla lo; hi = expr_to_tla hi } ])
    m.items;
  (* Get all local vars collected during compilation *)
  let all_local = get_module_local_vars () in
  (* Fixup UNCHANGED for all actions to include local vars *)
  let fixup_action a =
    let missing =
      List.filter
        (fun v ->
          (not (List.mem v a.unchanged))
          && not (List.exists (fun (var, _) -> var = v) a.assignments))
        all_local
    in
    { a with unchanged = a.unchanged @ missing }
  in
  let fixed_procs =
    List.map
      (fun (p : proc_ir) ->
        { p with actions = List.map fixup_action p.actions })
      !procs
  in
  (* Resolve call targets *)
  let resolved_procs =
    List.map
      (fun (p : proc_ir) ->
        {
          p with
          actions = List.map (resolve_call_target fixed_procs) p.actions;
        })
      fixed_procs
  in
  {
    name = m.mod_name;
    const_defs = !const_defs;
    fun_defs = !fun_defs;
    var_decls = !var_decls;
    local_var_decls = all_local;
    procs = resolved_procs;
    processes = !processes;
  }

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

(* ===== Public API ===== *)

let compile_to_ir (prog : Cst.program) : module_ir list =
  List.map compile_module_ir prog.modules

let compile (prog : Cst.program) : tla_module list =
  let irs = compile_to_ir prog in
  List.map generate_module irs
