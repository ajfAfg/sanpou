open Ast
open Ir

(* ===== Linearization context ===== *)

(* The name generators are closures created once per module in
   linearize_module; the mutable counters behind them never escape it. *)
type ctx = {
  proc_name : string;
  break_label : string option;
  continue_label : string option;
  fresh_label : unit -> string;
  fresh_temp_var : proc:string -> callee:string -> string;
      (* returns a fresh temp var name and records its var_info *)
}

let fresh_label ctx = ctx.fresh_label ()

type compiled = { actions : action list; entry : string; exit_label : string }

(* ===== Source info generation ===== *)

let make_source ~proc_name ~description ~(loc : Ast.loc) =
  { proc_name; description; line = loc.line; col = loc.col }

let pretty_expr =
  Ast_printer.pretty_expr Resolved_ast.display Resolved_ast.callee_name

let describe_simple_stmts (stmts : Resolved_ast.simple_stmt list) =
  String.concat ", "
    (List.map
       (Ast_printer.pretty_simple_stmt Resolved_ast.display
          Resolved_ast.callee_name)
       stmts)

let fresh_temp_var ctx ~callee = ctx.fresh_temp_var ~proc:ctx.proc_name ~callee

let rec lower_expr ctx source continuation (expr : Resolved_ast.expr) =
  match expr.desc with
  | IntLit _ | BoolLit _ | Var _ | Self -> ([], continuation, expr)
  | UnOp (op, rhs) ->
      let actions, entry, rhs = lower_expr ctx source continuation rhs in
      (actions, entry, { expr with desc = UnOp (op, rhs) })
  | BinOp (op, lhs, rhs) ->
      let rhs_actions, rhs_entry, rhs =
        lower_expr ctx source continuation rhs
      in
      let lhs_actions, lhs_entry, lhs = lower_expr ctx source rhs_entry lhs in
      ( lhs_actions @ rhs_actions,
        lhs_entry,
        { expr with desc = BinOp (op, lhs, rhs) } )
  | App (Resolved_ast.Proc name, args) ->
      let call_label = fresh_label ctx in
      let pop_label = fresh_label ctx in
      let temp = fresh_temp_var ctx ~callee:name in
      let args_actions, args_entry, args =
        lower_expr_list ctx source call_label args
      in
      let call_action =
        make_action ~label:call_label ~pc_dest:(PcCall name)
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
        { expr with desc = Var (Resolved_ast.ident temp) } )
  | App ((Resolved_ast.Fun _ as callee), args) ->
      let actions, entry, args = lower_expr_list ctx source continuation args in
      (actions, entry, { expr with desc = App (callee, args) })
  | Builtin (b, args) ->
      let actions, entry, args = lower_expr_list ctx source continuation args in
      (actions, entry, { expr with desc = Builtin (b, args) })
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
  guard : Resolved_ast.expr option;
  assignments : assignment list;
  stack_op : stack_op;
  next : pc_dest;
  pre_actions : action list;
  extra_actions : action list;
}

let empty_effect ~next =
  {
    guard = None;
    assignments = [];
    stack_op = StackNone;
    next = PcNext next;
    pre_actions = [];
    extra_actions = [];
  }

let apply_simple_stmt ctx ~next_label ~source ~label eff
    (stmt : Resolved_ast.simple_stmt) =
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
        | VarTarget i -> AssignVar (i.name, value)
        | SubscriptTarget (i, index) -> AssignIndex (i.name, index, value)
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
        next = PcCall name;
        extra_actions = [ pop_action ];
      }
  | Return value ->
      let actions, _, value = lower_expr ctx source label value in
      {
        eff with
        pre_actions = eff.pre_actions @ actions;
        stack_op = StackReturn value;
        next = PcReturn;
      }
  | Break -> (
      match ctx.break_label with
      | Some l -> { eff with next = PcNext l }
      | None -> failwith "break outside of loop")
  | Continue -> (
      match ctx.continue_label with
      | Some l -> { eff with next = PcNext l }
      | None -> failwith "continue outside of loop")

(* ===== Step linearization ===== *)

let rec linearize_step ctx (step : Resolved_ast.step) (next_label : string) :
    compiled =
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
          ~description:(describe_simple_stmts stmts)
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
          ~stack_op:eff.stack_op ~label ~pc_dest:eff.next ~source ()
      in
      {
        actions = eff.pre_actions @ [ a ] @ eff.extra_actions;
        entry =
          (match eff.pre_actions with first :: _ -> first.label | [] -> label);
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
      ~description:("while (" ^ pretty_expr cond ^ ") [check]")
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
      ~description:("if (" ^ pretty_expr cond ^ ") [check]")
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

and linearize_body ctx (steps : Resolved_ast.body) (continuation : string)
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
      let linearize_one (step : Resolved_ast.step) next_lbl =
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

and linearize_var_step ctx (step : Resolved_ast.step) next_label =
  let label = fresh_label ctx in
  match step.desc with
  | VarStep (i, value) ->
      let source =
        make_source ~proc_name:ctx.proc_name
          ~description:("var " ^ i.original ^ " = " ^ pretty_expr value)
          ~loc:step.loc
      in
      let pre_actions, entry, value = lower_expr ctx source label value in
      let a =
        make_action
          ~assignments:[ AssignVar (i.name, value) ]
          ~label ~pc_dest:(PcNext next_label) ~source ()
      in
      { actions = pre_actions @ [ a ]; entry; exit_label = next_label }
  | _ -> failwith "linearize_var_step called on non-VarStep"

(* ===== Module linearization ===== *)

(* Local [var] bindings of a body, in pre-order; MapInit binders never become
   TLA state variables and are not collected. *)
let rec local_idents (steps : Resolved_ast.body) : Resolved_ast.ident list =
  List.concat_map
    (fun (step : Resolved_ast.step) ->
      match step.desc with
      | VarStep (i, _) -> [ i ]
      | BlockStep (While { body; _ }) -> local_idents body
      | BlockStep (If { body; else_body; _ }) -> (
          local_idents body
          @ match else_body with Some b -> local_idents b | None -> [])
      | SimpleStep _ | EmptyStep -> [])
    steps

(* Each process runs its root procedure through a synthetic wrapper proc: the
   wrapper's entry action pushes the initial frame, and its discard action
   drops the unused return value and parks the process at Done. Synthesizing
   it here keeps Emit_tla a pure IR -> TLA+ pass and lets Source_map read
   wrapper actions like any others. *)
let wrapper_of_process ~process_name ~root_proc ~(loc : Ast.loc) : proc_ir =
  let entry_label = "__w_" ^ process_name ^ "_entry__" in
  let discard_label = "__w_" ^ process_name ^ "_discard__" in
  let entry_action =
    make_action ~label:entry_label ~pc_dest:(PcCall root_proc)
      ~stack_op:(StackPush (root_proc, discard_label, []))
      ~source:
        (make_source ~proc_name:process_name
           ~description:
             ("[process " ^ process_name ^ " starts " ^ root_proc ^ "]")
           ~loc)
      ()
  in
  let discard_action =
    make_action ~label:discard_label ~pc_dest:(PcNext Ir.done_label)
      ~stack_op:StackDiscard
      ~source:
        (make_source ~proc_name:process_name
           ~description:("[process " ^ process_name ^ " finished]")
           ~loc)
      ()
  in
  {
    proc_name = "__process_" ^ process_name ^ "_wrapper__";
    params = [];
    actions = [ entry_action; discard_action ];
    entry_label;
  }

let linearize_module (m : Resolved_ast.module_def) : module_ir =
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
      @ [
          {
            tla_name = name;
            original = name;
            proc = Some proc;
            kind = CallRet callee;
          };
        ];
    name
  in
  let const_defs =
    List.filter_map
      (fun (item : Resolved_ast.item) ->
        match item.desc with
        | ConstDef { name; value } -> Some (name, value)
        | _ -> None)
      m.items
  in
  let fun_defs =
    List.filter_map
      (fun (item : Resolved_ast.item) ->
        match item.desc with
        | FunDef { name; params; body_expr } -> Some (name, params, body_expr)
        | _ -> None)
      m.items
  in
  let var_decls =
    List.filter_map
      (fun (item : Resolved_ast.item) ->
        match item.desc with
        | VarDecl { name; value } -> Some (name, value)
        | _ -> None)
      m.items
  in
  let procs =
    List.filter_map
      (fun (item : Resolved_ast.item) ->
        match item.desc with
        | ProcDef { name; params; body } ->
            let ctx =
              {
                proc_name = name;
                break_label = None;
                continue_label = None;
                fresh_label;
                fresh_temp_var;
              }
            in
            let compiled =
              linearize_body ctx body Ir.done_label ~loc:item.loc
            in
            Some
              {
                proc_name = name;
                params =
                  List.map (fun (p : Resolved_ast.ident) -> p.name) params;
                actions = compiled.actions;
                entry_label = compiled.entry;
              }
        | _ -> None)
      m.items
  in
  let processes =
    List.filter_map
      (fun (item : Resolved_ast.item) ->
        match item.desc with
        | Process { name; proc; fair; lo; hi } ->
            Some
              {
                name;
                proc;
                fair;
                lo;
                hi;
                loc = item.loc;
                wrapper =
                  wrapper_of_process ~process_name:name ~root_proc:proc
                    ~loc:item.loc;
              }
        | _ -> None)
      m.items
  in
  let proc_var_info proc kind (i : Resolved_ast.ident) =
    { tla_name = i.name; original = i.original; proc = Some proc; kind }
  in
  let var_infos =
    List.map
      (fun (name, _) ->
        { tla_name = name; original = name; proc = None; kind = Global })
      var_decls
    @ List.concat_map
        (fun (item : Resolved_ast.item) ->
          match item.desc with
          | ProcDef { name; params; body } ->
              List.map (proc_var_info name Param) params
              @ List.map (proc_var_info name Local) (local_idents body)
          | _ -> [])
        m.items
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
    procs;
    processes;
  }

(* ===== Public API ===== *)

let linearize (prog : Resolved_ast.program) : module_ir list =
  List.map linearize_module prog
