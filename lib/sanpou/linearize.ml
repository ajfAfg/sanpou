open Cst
open Ir

(* ===== Label generation ===== *)

let fresh_label counter =
  incr counter;
  "L" ^ string_of_int !counter

(* ===== Linearization context ===== *)

type ctx = {
  proc_name : string;
  proc_names : string list;
  break_label : string option;
  continue_label : string option;
  label_counter : int ref;
  temp_counter : int ref;
  temp_local_vars : string list ref;
}

type compiled = { actions : action list; entry : string; exit_label : string }

(* ===== Source info generation ===== *)

let make_source ~proc_name ~description ~(loc : Cst.loc) =
  { proc_name; description; line = loc.line; col = loc.col }

let describe_simple_stmts (stmts : simple_stmt comma_list) =
  String.concat ", " (List.map Cst_printer.pretty_simple_stmt stmts.items)

let is_proc_call ctx name = List.mem name ctx.proc_names

let fresh_temp_var ctx =
  incr ctx.temp_counter;
  let name = "callRet__" ^ string_of_int !(ctx.temp_counter) in
  ctx.temp_local_vars := !(ctx.temp_local_vars) @ [ name ];
  name

let rec lower_expr ctx source continuation expr =
  match expr with
  | IntLit _ | BoolLit _ | Var _ | Self _ -> ([], continuation, expr)
  | UnOp r ->
      let actions, entry, rhs = lower_expr ctx source continuation r.rhs in
      (actions, entry, UnOp { r with rhs })
  | BinOp r ->
      let rhs_actions, rhs_entry, rhs =
        lower_expr ctx source continuation r.rhs
      in
      let lhs_actions, lhs_entry, lhs = lower_expr ctx source rhs_entry r.lhs in
      (lhs_actions @ rhs_actions, lhs_entry, BinOp { r with lhs; rhs })
  | App r when is_proc_call ctx r.name ->
      let call_label = fresh_label ctx.label_counter in
      let pop_label = fresh_label ctx.label_counter in
      let temp = fresh_temp_var ctx in
      let args_actions, args_entry, args =
        lower_expr_list ctx source call_label r.args.items
      in
      let call_action =
        {
          label = call_label;
          guard = None;
          assignments = [];
          pc_dest = PcNext "__call__";
          stack_op = StackPush (r.name, pop_label, args);
          source = { source with description = "[call " ^ r.name ^ "]" };
        }
      in
      let pop_action =
        {
          label = pop_label;
          guard = None;
          assignments = [];
          pc_dest = PcNext continuation;
          stack_op = StackPopAssign temp;
          source = { source with description = "[return from " ^ r.name ^ "]" };
        }
      in
      ( args_actions @ [ call_action; pop_action ],
        args_entry,
        Var { t = r.name_t; name = temp } )
  | App r ->
      let actions, entry, args =
        lower_expr_list ctx source continuation r.args.items
      in
      (actions, entry, App { r with args = { r.args with items = args } })
  | Subscript r ->
      let index_actions, index_entry, index =
        lower_expr ctx source continuation r.index
      in
      let lhs_actions, lhs_entry, lhs =
        lower_expr ctx source index_entry r.lhs
      in
      (lhs_actions @ index_actions, lhs_entry, Subscript { r with lhs; index })
  | MapInit r ->
      let value_actions, value_entry, value =
        lower_expr ctx source continuation r.value
      in
      let hi_actions, hi_entry, hi = lower_expr ctx source value_entry r.hi in
      let lo_actions, lo_entry, lo = lower_expr ctx source hi_entry r.lo in
      ( lo_actions @ hi_actions @ value_actions,
        lo_entry,
        MapInit { r with lo; hi; value } )
  | Tuple r ->
      let actions, entry, elems =
        lower_expr_list ctx source continuation r.elems.items
      in
      (actions, entry, Tuple { r with elems = { r.elems with items = elems } })
  | Sequence r ->
      let actions, entry, elems =
        lower_expr_list ctx source continuation r.elems.items
      in
      ( actions,
        entry,
        Sequence { r with elems = { r.elems with items = elems } } )
  | Paren r ->
      let actions, entry, inner = lower_expr ctx source continuation r.inner in
      (actions, entry, Paren { r with inner })

and lower_expr_list ctx source continuation exprs =
  let actions_rev, entry, exprs_rev =
    List.fold_right
      (fun expr (actions_acc, continuation_acc, exprs_acc) ->
        let actions, entry, expr =
          lower_expr ctx source continuation_acc expr
        in
        (actions @ actions_acc, entry, expr :: exprs_acc))
      exprs ([], continuation, [])
  in
  (actions_rev, entry, exprs_rev)

let lower_assign_target ctx source continuation = function
  | VarTarget _ as target -> ([], continuation, target)
  | SubscriptTarget r ->
      let actions, entry, index = lower_expr ctx source continuation r.index in
      (actions, entry, SubscriptTarget { r with index })

(* ===== Simple statement linearization ===== *)

let linearize_simple_stmt ctx (stmt : simple_stmt) ~guard ~assignments
    ~stack_op_ref ~actual_next ~extra_actions ~pre_actions ~next_label ~source
    ~label =
  match stmt with
  | Assign { target; value; _ } ->
      let value_actions, value_entry, value =
        lower_expr ctx source label value
      in
      let target_actions, entry, target =
        lower_assign_target ctx source value_entry target
      in
      pre_actions := !pre_actions @ target_actions @ value_actions;
      let assignment =
        match target with
        | VarTarget { name; _ } -> AssignVar (name, value)
        | SubscriptTarget { name; index; _ } -> AssignIndex (name, index, value)
      in
      assignments := !assignments @ [ assignment ]
  | Await { cond; _ } ->
      let actions, _, cond = lower_expr ctx source label cond in
      pre_actions := !pre_actions @ actions;
      guard := Some cond
  | Call { name; args; _ } ->
      let pop_label = fresh_label ctx.label_counter in
      let args_actions, _, args = lower_expr_list ctx source label args.items in
      pre_actions := !pre_actions @ args_actions;
      stack_op_ref := StackPush (name, pop_label, args);
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
      let actions, _, value = lower_expr ctx source label value in
      pre_actions := !pre_actions @ actions;
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
          let pre_actions, entry, value = lower_expr ctx source label value in
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
          { actions = pre_actions @ [ a ]; entry; exit_label = next_label }
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
          let args_actions, entry, args =
            lower_expr_list ctx source label args.items
          in
          let call_action =
            {
              label;
              guard = None;
              assignments = [];
              pc_dest = PcNext "__call__";
              stack_op = StackPush (name, pop_label, args);
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
            actions = args_actions @ [ call_action; pop_action ];
            entry;
            exit_label = next_label;
          }
      | _ ->
          let guard = ref None in
          let assignments = ref [] in
          let stack_op_ref = ref StackNone in
          let actual_next = ref next_label in
          let extra_actions = ref [] in
          let pre_actions = ref [] in
          List.iter
            (fun stmt ->
              linearize_simple_stmt ctx stmt ~guard ~assignments ~stack_op_ref
                ~actual_next ~extra_actions ~pre_actions ~next_label ~source
                ~label)
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
            actions = !pre_actions @ [ a ] @ !extra_actions;
            entry =
              (match !pre_actions with
              | first :: _ -> first.label
              | [] -> label);
            exit_label = next_label;
          })
  | BlockStep { stmt = While { cond; body; _ }; loc } ->
      linearize_while ctx cond body next_label loc
  | BlockStep { stmt = If { cond; body; else_branch; _ }; loc } ->
      linearize_if ctx cond body next_label loc else_branch
  | VarStep _ -> failwith "VarStep should be handled in linearize_body"

and linearize_while ctx cond body after_loop_label loc =
  let check_label = fresh_label ctx.label_counter in
  let source =
    make_source ~proc_name:ctx.proc_name
      ~description:("while (" ^ Cst_printer.pretty_expr cond ^ ") [check]")
      ~loc
  in
  let cond_actions, cond_entry, cond = lower_expr ctx source check_label cond in
  let body_ctx =
    {
      ctx with
      break_label = Some after_loop_label;
      continue_label = Some cond_entry;
    }
  in
  let body_compiled = linearize_body body_ctx body cond_entry in
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
    actions = cond_actions @ [ check_action ] @ body_compiled.actions;
    entry = cond_entry;
    exit_label = after_loop_label;
  }

and linearize_if ctx cond body next_label loc else_branch =
  let check_label = fresh_label ctx.label_counter in
  let source =
    make_source ~proc_name:ctx.proc_name
      ~description:("if (" ^ Cst_printer.pretty_expr cond ^ ") [check]")
      ~loc
  in
  let cond_actions, cond_entry, cond = lower_expr ctx source check_label cond in
  let body_compiled = linearize_body ctx body next_label in
  let else_entry, else_actions =
    match else_branch with
    | Some (_, _, else_body, _) ->
        let compiled = linearize_body ctx else_body next_label in
        (compiled.entry, compiled.actions)
    | None -> (next_label, [])
  in
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
    actions =
      cond_actions @ [ check_action ] @ body_compiled.actions @ else_actions;
    entry = cond_entry;
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
      let pre_actions, entry, value = lower_expr ctx source label value in
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
      { actions = pre_actions @ [ a ]; entry; exit_label = next_label }
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
  let temp_counter = ref 0 in
  let temp_local_vars = ref [] in
  let proc_names =
    List.filter_map
      (function ProcDef { name; _ } -> Some name | _ -> None)
      m.items
  in
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
              proc_names;
              break_label = None;
              continue_label = None;
              label_counter;
              temp_counter;
              temp_local_vars;
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
    local_var_decls = am.local_vars @ !temp_local_vars;
    procs = resolved_procs;
    processes = !processes;
  }

(* ===== Public API ===== *)

let linearize (modules : Alpha_convert.alpha_module list) : module_ir list =
  List.map linearize_module modules
