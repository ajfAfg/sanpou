open Cst
open Ir

(* ===== Label generation ===== *)

let fresh_label counter =
  incr counter;
  "L" ^ string_of_int !counter

(* ===== Linearization context ===== *)

type ctx = {
  proc_name : string;
  break_label : string option;
  continue_label : string option;
  label_counter : int ref;
}

type compiled = { actions : action list; entry : string; exit_label : string }

(* ===== Source info generation ===== *)

let make_source ~proc_name ~description ~(loc : Cst.loc) =
  { proc_name; description; line = loc.line; col = loc.col }

let describe_simple_stmts (stmts : simple_stmt comma_list) =
  String.concat ", " (List.map Cst_printer.pretty_simple_stmt stmts.items)

(* ===== Simple statement linearization ===== *)

let linearize_simple_stmt ctx (stmt : simple_stmt) ~guard ~assignments
    ~stack_op_ref ~actual_next ~extra_actions ~next_label ~source =
  match stmt with
  | Assign { target; value; _ } ->
      let assignment =
        match target with
        | VarTarget { name; _ } -> AssignVar (name, value)
        | SubscriptTarget { name; index; _ } -> AssignIndex (name, index, value)
      in
      assignments := !assignments @ [ assignment ]
  | Await { cond; _ } -> guard := Some cond
  | Call { name; args; _ } ->
      let pop_label = fresh_label ctx.label_counter in
      stack_op_ref := StackPush (name, pop_label, args.items);
      actual_next := "__call__";
      let pop_source =
        { source with description = "[return from " ^ name ^ "]" }
      in
      let pop_action =
        {
          label = pop_label;
          guard = None;
          assignments = [];
          pc_dest = PcNext next_label;
          stack_op = StackDiscard;
          source = pop_source;
        }
      in
      extra_actions := [ pop_action ]
  | Return { value; _ } ->
      stack_op_ref := StackReturn value;
      actual_next := "__return__"
  | Break _ ->
      let break_target =
        match ctx.break_label with
        | Some l -> l
        | None -> failwith "break outside of loop"
      in
      actual_next := break_target
  | Continue _ ->
      let continue_target =
        match ctx.continue_label with
        | Some l -> l
        | None -> failwith "continue outside of loop"
      in
      actual_next := continue_target

(* ===== Step linearization ===== *)

let rec linearize_step ctx (step : Cst.step) (next_label : string) : compiled =
  let label = fresh_label ctx.label_counter in
  match step with
  | EmptyStep { loc; _ } ->
      let source = make_source ~proc_name:ctx.proc_name ~description:";" ~loc in
      let a =
        {
          label;
          guard = None;
          assignments = [];
          pc_dest = PcNext next_label;
          stack_op = StackNone;
          source;
        }
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
            {
              label;
              guard = None;
              assignments = [];
              pc_dest = PcNext "__return__";
              stack_op = StackReturn value;
              source;
            }
          in
          { actions = [ a ]; entry = label; exit_label = next_label }
      | [ Break _ ] ->
          let break_target =
            match ctx.break_label with
            | Some l -> l
            | None -> failwith "break outside of loop"
          in
          let a =
            {
              label;
              guard = None;
              assignments = [];
              pc_dest = PcNext break_target;
              stack_op = StackNone;
              source;
            }
          in
          { actions = [ a ]; entry = label; exit_label = next_label }
      | [ Continue _ ] ->
          let continue_target =
            match ctx.continue_label with
            | Some l -> l
            | None -> failwith "continue outside of loop"
          in
          let a =
            {
              label;
              guard = None;
              assignments = [];
              pc_dest = PcNext continue_target;
              stack_op = StackNone;
              source;
            }
          in
          { actions = [ a ]; entry = label; exit_label = next_label }
      | [ Call { name; args; _ } ] ->
          let pop_label = fresh_label ctx.label_counter in
          let call_action =
            {
              label;
              guard = None;
              assignments = [];
              pc_dest = PcNext "__call__";
              stack_op = StackPush (name, pop_label, args.items);
              source;
            }
          in
          let pop_source =
            { source with description = "[return from " ^ name ^ "]" }
          in
          let pop_action =
            {
              label = pop_label;
              guard = None;
              assignments = [];
              pc_dest = PcNext next_label;
              stack_op = StackDiscard;
              source = pop_source;
            }
          in
          {
            actions = [ call_action; pop_action ];
            entry = label;
            exit_label = next_label;
          }
      | _ ->
          let guard = ref None in
          let assignments = ref [] in
          let stack_op_ref = ref StackNone in
          let actual_next = ref next_label in
          let extra_actions = ref [] in
          List.iter
            (fun stmt ->
              linearize_simple_stmt ctx stmt ~guard ~assignments ~stack_op_ref
                ~actual_next ~extra_actions ~next_label ~source)
            stmts.items;
          let a =
            {
              label;
              guard = !guard;
              assignments = !assignments;
              pc_dest = PcNext !actual_next;
              stack_op = !stack_op_ref;
              source;
            }
          in
          {
            actions = [ a ] @ !extra_actions;
            entry = label;
            exit_label = next_label;
          })
  | BlockStep { stmt = While { cond; body; _ }; loc } ->
      linearize_while ctx cond body next_label loc
  | BlockStep { stmt = If { cond; body; else_branch; _ }; loc } ->
      linearize_if ctx cond body next_label loc else_branch
  | VarStep _ -> failwith "VarStep should be handled in linearize_body"

and linearize_while ctx cond body after_loop_label loc =
  let check_label = fresh_label ctx.label_counter in
  let body_ctx =
    {
      ctx with
      break_label = Some after_loop_label;
      continue_label = Some check_label;
    }
  in
  let body_compiled = linearize_body body_ctx body check_label in
  let desc = "while (" ^ Cst_printer.pretty_expr cond ^ ") [check]" in
  let source = make_source ~proc_name:ctx.proc_name ~description:desc ~loc in
  let check_action =
    {
      label = check_label;
      guard = None;
      assignments = [];
      pc_dest = PcBranch (cond, body_compiled.entry, after_loop_label);
      stack_op = StackNone;
      source;
    }
  in
  {
    actions = [ check_action ] @ body_compiled.actions;
    entry = check_label;
    exit_label = after_loop_label;
  }

and linearize_if ctx cond body next_label loc else_branch =
  let check_label = fresh_label ctx.label_counter in
  let body_compiled = linearize_body ctx body next_label in
  let else_entry, else_actions =
    match else_branch with
    | Some (_, _, else_body, _) ->
        let compiled = linearize_body ctx else_body next_label in
        (compiled.entry, compiled.actions)
    | None -> (next_label, [])
  in
  let desc = "if (" ^ Cst_printer.pretty_expr cond ^ ") [check]" in
  let source = make_source ~proc_name:ctx.proc_name ~description:desc ~loc in
  let check_action =
    {
      label = check_label;
      guard = None;
      assignments = [];
      pc_dest = PcBranch (cond, body_compiled.entry, else_entry);
      stack_op = StackNone;
      source;
    }
  in
  {
    actions = [ check_action ] @ body_compiled.actions @ else_actions;
    entry = check_label;
    exit_label = next_label;
  }

and linearize_body ctx (steps : Cst.body) (continuation : string) : compiled =
  match steps with
  | [] ->
      let label = fresh_label ctx.label_counter in
      let source =
        make_source ~proc_name:ctx.proc_name ~description:"[empty body]"
          ~loc:{ line = 0; col = 0 }
      in
      let a =
        {
          label;
          guard = None;
          assignments = [];
          pc_dest = PcNext continuation;
          stack_op = StackNone;
          source;
        }
      in
      { actions = [ a ]; entry = label; exit_label = continuation }
  | _ ->
      (* Backward pass only — alpha conversion already handled variable scoping *)
      let linearize_one step next_lbl =
        match step with
        | VarStep _ -> linearize_var_step ctx step next_lbl
        | _ -> linearize_step ctx step next_lbl
      in
      let rev_steps = List.rev steps in
      let first = List.hd rev_steps in
      let rest = List.tl rev_steps in
      let compiled = linearize_one first continuation in
      List.fold_left
        (fun acc step ->
          let c = linearize_one step acc.entry in
          {
            actions = c.actions @ acc.actions;
            entry = c.entry;
            exit_label = acc.exit_label;
          })
        compiled rest

and linearize_var_step ctx step next_label =
  let label = fresh_label ctx.label_counter in
  match step with
  | VarStep { name; value; loc; _ } ->
      let source =
        make_source ~proc_name:ctx.proc_name
          ~description:("var " ^ name ^ " = " ^ Cst_printer.pretty_expr value)
          ~loc
      in
      let a =
        {
          label;
          guard = None;
          assignments = [ AssignVar (name, value) ];
          pc_dest = PcNext next_label;
          stack_op = StackNone;
          source;
        }
      in
      { actions = [ a ]; entry = label; exit_label = next_label }
  | _ -> failwith "linearize_var_step called on non-VarStep"

(* ===== Resolve call targets ===== *)

let resolve_call_target procs action =
  match action.stack_op with
  | StackPush (proc_name, _, args) ->
      let target_proc =
        List.find (fun (p : proc_ir) -> p.proc_name = proc_name) procs
      in
      let params = target_proc.params in
      let param_bindings =
        try List.map2 (fun param arg -> AssignVar (param, arg)) params args
        with Invalid_argument _ ->
          failwith ("arity mismatch when calling procedure " ^ proc_name)
      in
      {
        action with
        assignments = action.assignments @ param_bindings;
        pc_dest = PcNext target_proc.entry_label;
      }
  | _ -> action

(* ===== Module linearization ===== *)

let linearize_module (am : Alpha_convert.alpha_module) : module_ir =
  let m = am.cst in
  let label_counter = ref 0 in
  let const_defs = ref [] in
  let fun_defs = ref [] in
  let var_decls = ref [] in
  let procs = ref [] in
  let processes = ref [] in
  List.iter
    (fun (item : Cst.item) ->
      match item with
      | ConstDef { name; value; _ } ->
          const_defs := !const_defs @ [ (name, value) ]
      | FunDef { name; params; body_expr; _ } ->
          fun_defs :=
            !fun_defs @ [ (name, List.map snd params.items, body_expr) ]
      | VarDecl { name; value; _ } ->
          var_decls := !var_decls @ [ (name, value) ]
      | ProcDef { name; params; body; _ } ->
          let ctx =
            {
              proc_name = name;
              break_label = None;
              continue_label = None;
              label_counter;
            }
          in
          let done_label = "Done" in
          let compiled = linearize_body ctx body done_label in
          let proc =
            {
              proc_name = name;
              params = List.map snd params.items;
              actions = compiled.actions;
              entry_label = compiled.entry;
            }
          in
          procs := !procs @ [ proc ]
      | Process { name; proc; fair_t; lo; hi; _ } ->
          processes :=
            !processes
            @ [ { name; proc; fair = Option.is_some fair_t; lo; hi } ])
    m.items;
  (* Resolve call targets *)
  let resolved_procs =
    List.map
      (fun (p : proc_ir) ->
        { p with actions = List.map (resolve_call_target !procs) p.actions })
      !procs
  in
  {
    name = m.mod_name;
    const_defs = !const_defs;
    fun_defs = !fun_defs;
    var_decls = !var_decls;
    local_var_decls = am.local_vars;
    procs = resolved_procs;
    processes = !processes;
  }

(* ===== Public API ===== *)

let linearize (modules : Alpha_convert.alpha_module list) : module_ir list =
  List.map linearize_module modules
