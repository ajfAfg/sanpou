open Ast
open Ir

(* ===== Linearization context ===== *)

(* The name generators are closures created once per module in
   linearize_module; the mutable counters behind them never escape it. *)
type ctx = {
  proc_name : string;
  proc_names : string list;
  break_label : string option;
  continue_label : string option;
  fresh_label : unit -> string;
  fresh_temp_var : proc:string -> callee:string -> string;
      (* returns a fresh temp var name and records its var_info *)
  demangle : string -> string;
      (* maps alpha-converted names back to source names for descriptions *)
}

let fresh_label ctx = ctx.fresh_label ()

type compiled = { actions : action list; entry : string; exit_label : string }

(* ===== Source info generation ===== *)

let make_source ~proc_name ~description ~(loc : Ast.loc) =
  { proc_name; description; line = loc.line; col = loc.col }

let describe_simple_stmts ctx (stmts : simple_stmt list) =
  String.concat ", "
    (List.map (Ast_printer.pretty_simple_stmt ~rename:ctx.demangle) stmts)

let is_proc_call ctx name = List.mem name ctx.proc_names

let fresh_temp_var ctx ~callee = ctx.fresh_temp_var ~proc:ctx.proc_name ~callee

let rec lower_expr ctx source continuation (expr : expr) =
  match expr.desc with
  | IntLit _ | BoolLit _ | Var _ | Self -> ([], continuation, expr)
  | UnOp (op, rhs) ->
      let actions, entry, rhs = lower_expr ctx source continuation rhs in
      (actions, entry, { expr with desc = UnOp (op, rhs) })
  | BinOp (op, lhs, rhs) ->
      let rhs_actions, rhs_entry, rhs = lower_expr ctx source continuation rhs in
      let lhs_actions, lhs_entry, lhs = lower_expr ctx source rhs_entry lhs in
      (lhs_actions @ rhs_actions, lhs_entry, { expr with desc = BinOp (op, lhs, rhs) })
  | App (name, args) when is_proc_call ctx name ->
      let call_label = fresh_label ctx in
      let pop_label = fresh_label ctx in
      let temp = fresh_temp_var ctx ~callee:name in
      let args_actions, args_entry, args =
        lower_expr_list ctx source call_label args
      in
      let call_action =
        make_action ~label:call_label ~pc_dest:(PcNext "__call__")
          ~stack_op:(StackPush (name, pop_label, args))
          ~source:{ source with description = "[call " ^ name ^ "]" }
          ()
      in
      let pop_action =
        make_action ~label:pop_label ~pc_dest:(PcNext continuation)
          ~stack_op:(StackPopAssign temp)
          ~source:{ source with description = "[return from " ^ name ^ "]" }
          ()
      in
      ( args_actions @ [ call_action; pop_action ],
        args_entry,
        { expr with desc = Var temp } )
  | App (name, args) ->
      let actions, entry, args =
        lower_expr_list ctx source continuation args
      in
      (actions, entry, { expr with desc = App (name, args) })
  | Subscript (lhs, index) ->
      let index_actions, index_entry, index =
        lower_expr ctx source continuation index
      in
      let lhs_actions, lhs_entry, lhs = lower_expr ctx source index_entry lhs in
      ( lhs_actions @ index_actions,
        lhs_entry,
        { expr with desc = Subscript (lhs, index) } )
  | MapInit { binder; lo; hi; value } ->
      let value_actions, value_entry, value =
        lower_expr ctx source continuation value
      in
      let hi_actions, hi_entry, hi = lower_expr ctx source value_entry hi in
      let lo_actions, lo_entry, lo = lower_expr ctx source hi_entry lo in
      ( lo_actions @ hi_actions @ value_actions,
        lo_entry,
        { expr with desc = MapInit { binder; lo; hi; value } } )
  | Tuple elems ->
      let actions, entry, elems =
        lower_expr_list ctx source continuation elems
      in
      (actions, entry, { expr with desc = Tuple elems })
  | Sequence elems ->
      let actions, entry, elems =
        lower_expr_list ctx source continuation elems
      in
      (actions, entry, { expr with desc = Sequence elems })

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
  | SubscriptTarget (name, index) ->
      let actions, entry, index = lower_expr ctx source continuation index in
      (actions, entry, SubscriptTarget (name, index))

(* ===== Simple statement linearization ===== *)

(* Cumulative effect of the simple statements merged into one action.
   [assignments] and [pre_actions] accumulate across statements; [guard],
   [stack_op], [next] and [extra_actions] are overwritten (last statement
   wins), matching how at most one control-transferring statement is
   meaningful per step. *)
type stmt_effect = {
  guard : Ast.expr option;
  assignments : assignment list;
  stack_op : stack_op;
  next : string;
  pre_actions : action list;
  extra_actions : action list;
}

let empty_effect ~next =
  {
    guard = None;
    assignments = [];
    stack_op = StackNone;
    next;
    pre_actions = [];
    extra_actions = [];
  }

let apply_simple_stmt ctx ~next_label ~source ~label eff (stmt : simple_stmt) =
  match stmt.desc with
  | Assign (target, value) ->
      let value_actions, value_entry, value =
        lower_expr ctx source label value
      in
      let target_actions, _, target =
        lower_assign_target ctx source value_entry target
      in
      let assignment =
        match target with
        | VarTarget name -> AssignVar (name, value)
        | SubscriptTarget (name, index) -> AssignIndex (name, index, value)
      in
      {
        eff with
        pre_actions = eff.pre_actions @ target_actions @ value_actions;
        assignments = eff.assignments @ [ assignment ];
      }
  | Await cond ->
      let actions, _, cond = lower_expr ctx source label cond in
      { eff with pre_actions = eff.pre_actions @ actions; guard = Some cond }
  | Call (name, args) ->
      let pop_label = fresh_label ctx in
      let args_actions, _, args = lower_expr_list ctx source label args in
      let pop_action =
        make_action ~label:pop_label ~pc_dest:(PcNext next_label)
          ~stack_op:StackDiscard
          ~source:{ source with description = "[return from " ^ name ^ "]" }
          ()
      in
      {
        eff with
        pre_actions = eff.pre_actions @ args_actions;
        stack_op = StackPush (name, pop_label, args);
        next = "__call__";
        extra_actions = [ pop_action ];
      }
  | Return value ->
      let actions, _, value = lower_expr ctx source label value in
      {
        eff with
        pre_actions = eff.pre_actions @ actions;
        stack_op = StackReturn value;
        next = "__return__";
      }
  | Break -> (
      match ctx.break_label with
      | Some l -> { eff with next = l }
      | None -> failwith "break outside of loop")
  | Continue -> (
      match ctx.continue_label with
      | Some l -> { eff with next = l }
      | None -> failwith "continue outside of loop")

(* ===== Step linearization ===== *)

let rec linearize_step ctx (step : Ast.step) (next_label : string) : compiled =
  let label = fresh_label ctx in
  match step.desc with
  | EmptyStep ->
      let source =
        make_source ~proc_name:ctx.proc_name ~description:";" ~loc:step.loc
      in
      let a = make_action ~label ~pc_dest:(PcNext next_label) ~source () in
      { actions = [ a ]; entry = label; exit_label = next_label }
  | SimpleStep stmts ->
      let source =
        make_source ~proc_name:ctx.proc_name
          ~description:(describe_simple_stmts ctx stmts)
          ~loc:step.loc
      in
      let eff =
        List.fold_left
          (apply_simple_stmt ctx ~next_label ~source ~label)
          (empty_effect ~next:next_label)
          stmts
      in
      let a =
        make_action ?guard:eff.guard ~assignments:eff.assignments
          ~stack_op:eff.stack_op ~label ~pc_dest:(PcNext eff.next) ~source ()
      in
      {
        actions = eff.pre_actions @ [ a ] @ eff.extra_actions;
        entry =
          (match eff.pre_actions with
          | first :: _ -> first.label
          | [] -> label);
        exit_label = next_label;
      }
  | BlockStep (While { cond; body }) ->
      linearize_while ctx cond body next_label step.loc
  | BlockStep (If { cond; body; else_body }) ->
      linearize_if ctx cond body next_label step.loc else_body
  | VarStep _ -> failwith "VarStep should be handled in linearize_body"

and linearize_while ctx cond body after_loop_label loc =
  let check_label = fresh_label ctx in
  let source =
    make_source ~proc_name:ctx.proc_name
      ~description:
        ("while ("
        ^ Ast_printer.pretty_expr ~rename:ctx.demangle cond
        ^ ") [check]")
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
  let body_compiled = linearize_body body_ctx body cond_entry ~loc in
  let check_action =
    make_action ~label:check_label
      ~pc_dest:(PcBranch (cond, body_compiled.entry, after_loop_label))
      ~source ()
  in
  {
    actions = cond_actions @ [ check_action ] @ body_compiled.actions;
    entry = cond_entry;
    exit_label = after_loop_label;
  }

and linearize_if ctx cond body next_label loc else_body =
  let check_label = fresh_label ctx in
  let source =
    make_source ~proc_name:ctx.proc_name
      ~description:
        ("if ("
        ^ Ast_printer.pretty_expr ~rename:ctx.demangle cond
        ^ ") [check]")
      ~loc
  in
  let cond_actions, cond_entry, cond = lower_expr ctx source check_label cond in
  let body_compiled = linearize_body ctx body next_label ~loc in
  let else_entry, else_actions =
    match else_body with
    | Some else_body ->
        let compiled = linearize_body ctx else_body next_label ~loc in
        (compiled.entry, compiled.actions)
    | None -> (next_label, [])
  in
  let check_action =
    make_action ~label:check_label
      ~pc_dest:(PcBranch (cond, body_compiled.entry, else_entry))
      ~source ()
  in
  {
    actions =
      cond_actions @ [ check_action ] @ body_compiled.actions @ else_actions;
    entry = cond_entry;
    exit_label = next_label;
  }

and linearize_body ctx (steps : Ast.body) (continuation : string)
    ~(loc : Ast.loc) : compiled =
  (* [loc] is the enclosing construct's location, used when the body is empty *)
  match steps with
  | [] ->
      let label = fresh_label ctx in
      let source =
        make_source ~proc_name:ctx.proc_name ~description:"[empty body]" ~loc
      in
      let a = make_action ~label ~pc_dest:(PcNext continuation) ~source () in
      { actions = [ a ]; entry = label; exit_label = continuation }
  | _ ->
      (* Backward pass only — alpha conversion already handled variable scoping *)
      let linearize_one (step : Ast.step) next_lbl =
        match step.desc with
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

and linearize_var_step ctx (step : Ast.step) next_label =
  let label = fresh_label ctx in
  match step.desc with
  | VarStep (name, value) ->
      let source =
        make_source ~proc_name:ctx.proc_name
          ~description:
            ("var " ^ ctx.demangle name ^ " = "
            ^ Ast_printer.pretty_expr ~rename:ctx.demangle value)
          ~loc:step.loc
      in
      let pre_actions, entry, value = lower_expr ctx source label value in
      let a =
        make_action
          ~assignments:[ AssignVar (name, value) ]
          ~label ~pc_dest:(PcNext next_label) ~source ()
      in
      { actions = pre_actions @ [ a ]; entry; exit_label = next_label }
  | _ -> failwith "linearize_var_step called on non-VarStep"

(* ===== Resolve call targets ===== *)

(* Parameter binding is not lowered to assignments here: the arguments stay
   in StackPush and Emit_tla writes them directly into the pushed frame. *)
let resolve_call_target procs (action : action) =
  match action.stack_op with
  | StackPush (proc_name, _, args) ->
      let target_proc =
        List.find (fun (p : proc_ir) -> p.proc_name = proc_name) procs
      in
      if List.length target_proc.params <> List.length args then
        failwith ("arity mismatch when calling procedure " ^ proc_name);
      { action with pc_dest = PcNext target_proc.entry_label }
  | _ -> action

(* ===== Module linearization ===== *)

let linearize_module (am : Alpha_convert.alpha_module) : module_ir =
  let m = am.ast in
  let label_counter = ref 0 in
  let fresh_label () =
    incr label_counter;
    "L" ^ string_of_int !label_counter
  in
  let temp_counter = ref 0 in
  let temp_var_infos = ref [] in
  let fresh_temp_var ~proc ~callee =
    incr temp_counter;
    let name = "callRet__" ^ string_of_int !temp_counter in
    temp_var_infos :=
      !temp_var_infos
      @ [ { tla_name = name; original = name; proc = Some proc; kind = CallRet callee } ];
    name
  in
  let demangle name =
    match
      List.find_opt
        (fun (r : Alpha_convert.rename) -> r.tla_name = name)
        am.renames
    with
    | Some r -> r.original
    | None -> name
  in
  let proc_names =
    List.filter_map
      (fun (item : item) ->
        match item.desc with ProcDef { name; _ } -> Some name | _ -> None)
      m.items
  in
  let const_defs =
    List.filter_map
      (fun (item : item) ->
        match item.desc with
        | ConstDef { name; value } -> Some (name, value)
        | _ -> None)
      m.items
  in
  let fun_defs =
    List.filter_map
      (fun (item : item) ->
        match item.desc with
        | FunDef { name; params; body_expr } -> Some (name, params, body_expr)
        | _ -> None)
      m.items
  in
  let var_decls =
    List.filter_map
      (fun (item : item) ->
        match item.desc with
        | VarDecl { name; value } -> Some (name, value)
        | _ -> None)
      m.items
  in
  let procs =
    List.filter_map
      (fun (item : item) ->
        match item.desc with
        | ProcDef { name; params; body } ->
            let ctx =
              {
                proc_name = name;
                proc_names;
                break_label = None;
                continue_label = None;
                fresh_label;
                fresh_temp_var;
                demangle;
              }
            in
            let done_label = "Done" in
            let compiled = linearize_body ctx body done_label ~loc:item.loc in
            Some
              {
                proc_name = name;
                params;
                actions = compiled.actions;
                entry_label = compiled.entry;
              }
        | _ -> None)
      m.items
  in
  let processes =
    List.filter_map
      (fun (item : item) ->
        match item.desc with
        | Process { name; proc; fair; lo; hi } ->
            Some { name; proc; fair; lo; hi; loc = item.loc }
        | _ -> None)
      m.items
  in
  (* Resolve call targets *)
  let resolved_procs =
    List.map
      (fun (p : proc_ir) ->
        { p with actions = List.map (resolve_call_target procs) p.actions })
      procs
  in
  let var_infos =
    List.map
      (fun (name, _) -> { tla_name = name; original = name; proc = None; kind = Global })
      var_decls
    @ List.filter_map
        (fun (r : Alpha_convert.rename) ->
          match r.kind with
          | Alpha_convert.BinderVar -> None (* never a TLA state variable *)
          | Alpha_convert.ParamVar | Alpha_convert.LocalVar ->
              Some
                {
                  tla_name = r.tla_name;
                  original = r.original;
                  proc = Some r.proc;
                  kind =
                    (match r.kind with
                    | Alpha_convert.ParamVar -> Param
                    | _ -> Local);
                })
        am.renames
    @ !temp_var_infos
  in
  (* Derived from var_infos so the two cannot drift apart *)
  let local_var_decls =
    List.filter_map
      (fun (v : var_info) ->
        match v.kind with Global -> None | _ -> Some v.tla_name)
      var_infos
  in
  {
    name = m.mod_name;
    const_defs;
    fun_defs;
    var_decls;
    local_var_decls;
    var_infos;
    procs = resolved_procs;
    processes;
  }

(* ===== Public API ===== *)

let linearize (modules : Alpha_convert.alpha_module list) : module_ir list =
  List.map linearize_module modules
