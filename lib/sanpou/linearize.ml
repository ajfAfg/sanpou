open Ir

exception Error of string * Generic_ast.loc

let error message loc = raise (Error (message, loc))

(* ===== Linearization context ===== *)

(* [fresh_label] is a closure created once per module in linearize_module;
   the mutable counter behind it never escapes it. *)
type ctx = {
  proc_name : string;
  break_label : string option;
  continue_label : string option;
  fresh_label : unit -> string;
}

let fresh_label ctx = ctx.fresh_label ()

type compiled = {
  actions : action_node list;
  entry : string;
  exit_label : string;
}

(* ===== Source info generation ===== *)

let make_source ~proc_name ~description ~(loc : Generic_ast.loc) =
  { proc_name; description; line = loc.line; col = loc.col }

let pretty_expr =
  Ast_printer.pretty_expr Resolved_ast.display Normalized_ast.callee_name

let describe_simple_stmts (stmts : Normalized_ast.simple_stmt list) =
  String.concat ", "
    (List.map
       (Ast_printer.pretty_simple_stmt Resolved_ast.display
          Normalized_ast.callee_name)
       stmts)

(* ===== Simple statement linearization ===== *)

(* Cumulative effect of the simple statements merged into one action.
   [assignments], [asserts], and [guard] accumulate across statements
   (guards conjoin, like PlusCal's `when`); [stack_op], [next] and
   [extra_actions] are overwritten (last statement wins), matching how
   at most one control-transferring statement is meaningful per step. *)
type stmt_effect = {
  guard : Normalized_ast.expr option;
  asserts : (Normalized_ast.expr * string) list;
  assignments : assignment list;
  stack_op : stack_op;
  next : pc_dest;
  extra_actions : action list;
}

let empty_effect ~next =
  {
    guard = None;
    asserts = [];
    assignments = [];
    stack_op = StackNone;
    next = PcNext next;
    extra_actions = [];
  }

let apply_simple_stmt ctx ~next_label ~source eff
    (stmt : Normalized_ast.simple_stmt) =
  match stmt.desc with
  | Generic_ast.Assign (target, value) ->
      let assignment =
        match target with
        | Generic_ast.VarTarget i -> AssignVar (i.name, value)
        | Generic_ast.PathTarget (i, path) -> AssignPath (i.name, path, value)
      in
      { eff with assignments = eff.assignments @ [ assignment ] }
  | Generic_ast.Await cond ->
      (* Multiple awaits in one step conjoin: with pre-state reads every
         guard constrains the same state, so order is immaterial. *)
      let guard =
        match eff.guard with
        | None -> cond
        | Some g ->
            {
              Generic_ast.desc = Generic_ast.BinOp (Generic_ast.And, g, cond);
              loc = g.loc;
            }
      in
      { eff with guard = Some guard }
  | Generic_ast.Assert cond ->
      let message =
        Printf.sprintf "assertion failed at line %d, col %d: %s" stmt.loc.line
          stmt.loc.col (pretty_expr cond)
      in
      { eff with asserts = eff.asserts @ [ (cond, message) ] }
  | Generic_ast.Call (name, args) ->
      let pop_label = fresh_label ctx in
      let pop_action =
        make_action ~label:pop_label ~pc_dest:(PcNext next_label)
          ~stack_op:StackDiscard
          ~source:{ source with description = "[return from " ^ name ^ "]" }
          ()
      in
      {
        eff with
        stack_op = StackPush (name, pop_label, args);
        next = PcCall name;
        extra_actions = [ pop_action ];
      }
  | Generic_ast.Return value ->
      { eff with stack_op = StackReturn value; next = PcReturn }
  | Generic_ast.Break -> (
      match ctx.break_label with
      | Some l -> { eff with next = PcNext l }
      | None -> failwith "break outside of loop")
  | Generic_ast.Continue -> (
      match ctx.continue_label with
      | Some l -> { eff with next = PcNext l }
      | None -> failwith "continue outside of loop")

(* ===== Step linearization ===== *)

let rec linearize_step ctx (step : Normalized_ast.step) (next_label : string) :
    compiled =
  match step.desc with
  | Normalized_ast.EmptyStep ->
      let label = fresh_label ctx in
      let source =
        make_source ~proc_name:ctx.proc_name ~description:";" ~loc:step.loc
      in
      let a = make_action ~label ~pc_dest:(PcNext next_label) ~source () in
      { actions = [ Action a ]; entry = label; exit_label = next_label }
  | Normalized_ast.SimpleStep stmts ->
      let label = fresh_label ctx in
      let source =
        make_source ~proc_name:ctx.proc_name
          ~description:(describe_simple_stmts stmts)
          ~loc:step.loc
      in
      let eff =
        List.fold_left
          (apply_simple_stmt ctx ~next_label ~source)
          (empty_effect ~next:next_label)
          stmts
      in
      let a =
        make_action ?guard:eff.guard ~asserts:eff.asserts
          ~assignments:eff.assignments ~stack_op:eff.stack_op ~label
          ~pc_dest:eff.next ~source ()
      in
      {
        actions = List.map (fun x -> Action x) (a :: eff.extra_actions);
        entry = label;
        exit_label = next_label;
      }
  | Normalized_ast.VarStep (i, value) ->
      let label = fresh_label ctx in
      let source =
        make_source ~proc_name:ctx.proc_name
          ~description:("var " ^ i.original ^ " = " ^ pretty_expr value)
          ~loc:step.loc
      in
      let a =
        make_action
          ~assignments:[ AssignVar (i.name, value) ]
          ~label ~pc_dest:(PcNext next_label) ~source ()
      in
      { actions = [ Action a ]; entry = label; exit_label = next_label }
  | Normalized_ast.CallBindStep { bind; callee; args } ->
      let call_label = fresh_label ctx in
      let pop_label = fresh_label ctx in
      let call_action =
        make_action ~label:call_label ~pc_dest:(PcCall callee)
          ~stack_op:(StackPush (callee, pop_label, args))
          ~source:
            (make_source ~proc_name:ctx.proc_name
               ~description:("[call " ^ callee ^ "]")
               ~loc:step.loc)
          ()
      in
      let pop_action =
        make_action ~label:pop_label ~pc_dest:(PcNext next_label)
          ~stack_op:(StackPopAssign bind.name)
          ~source:
            (make_source ~proc_name:ctx.proc_name
               ~description:("[return from " ^ callee ^ "]")
               ~loc:step.loc)
          ()
      in
      {
        actions = [ Action call_action; Action pop_action ];
        entry = call_label;
        exit_label = next_label;
      }
  | Normalized_ast.WithStep { binder; domain; stmts } ->
      let label = fresh_label ctx in
      let source =
        make_source ~proc_name:ctx.proc_name
          ~description:
            ("with (" ^ binder.original ^ " in " ^ pretty_expr domain ^ ") { "
           ^ describe_simple_stmts stmts ^ " }")
          ~loc:step.loc
      in
      let eff =
        List.fold_left
          (apply_simple_stmt ctx ~next_label ~source)
          (empty_effect ~next:next_label)
          stmts
      in
      let a =
        make_action
          ~binders:[ (binder.name, domain) ]
          ?guard:eff.guard ~asserts:eff.asserts ~assignments:eff.assignments
          ~stack_op:eff.stack_op ~label ~pc_dest:eff.next ~source ()
      in
      {
        actions = List.map (fun x -> Action x) (a :: eff.extra_actions);
        entry = label;
        exit_label = next_label;
      }
  | Normalized_ast.BlockStep (While { pre; cond; body }) ->
      linearize_while ctx pre cond body next_label step.loc
  | Normalized_ast.BlockStep (If { cond; body; else_body }) ->
      linearize_if ctx cond body next_label step.loc else_body
  | Normalized_ast.BlockStep (Either arms) ->
      linearize_either ctx arms next_label step.loc

and linearize_while ctx pre cond body after_loop_label loc =
  let check_label = fresh_label ctx in
  let source =
    make_source ~proc_name:ctx.proc_name
      ~description:("while (" ^ pretty_expr cond ^ ") [check]")
      ~loc
  in
  (* [pre] re-runs before every check, so the back edges below target its
     entry rather than the check itself. *)
  let pre_compiled =
    match pre with
    | [] -> None
    | _ -> Some (linearize_body ctx pre check_label ~loc)
  in
  let cond_entry =
    match pre_compiled with Some c -> c.entry | None -> check_label
  in
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
    actions =
      (match pre_compiled with Some c -> c.actions | None -> [])
      @ [ Action check_action ] @ body_compiled.actions;
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
    actions = (Action check_action :: body_compiled.actions) @ else_actions;
    entry = check_label;
    exit_label = next_label;
  }

and linearize_either ctx arms next_label loc =
  let label = fresh_label ctx in
  let source =
    make_source ~proc_name:ctx.proc_name
      ~description:("either [" ^ string_of_int (List.length arms) ^ " arms]")
      ~loc
  in
  let compiled_arms =
    List.map (fun arm -> linearize_body ctx arm next_label ~loc) arms
  in
  (* The choice embeds each arm's entry action as an alternative; an arm
     that itself starts with an either contributes its alternatives
     directly (disjunction is associative). The embedded original is kept
     only when a jump inside the arm (a loop back edge) still targets its
     label; otherwise its pc value is unreachable and the standalone
     operator would be dead weight. Jumps from outside the arm cannot
     target it: labels are fresh per construct, and break/continue only
     jump outward. *)
  let referenced_labels nodes =
    let of_action (a : action) =
      (match a.pc_dest with
      | PcNext l -> [ l ]
      | PcBranch (_, t, f) -> [ t; f ]
      | PcCall _ | PcReturn -> [])
      @ match a.stack_op with StackPush (_, ret, _) -> [ ret ] | _ -> []
    in
    List.concat_map
      (function
        | Action a -> of_action a
        | Choice inner -> List.concat_map of_action inner.arms)
      nodes
  in
  let entry_alternatives (c : compiled) =
    match List.find (fun n -> node_label n = c.entry) c.actions with
    | Action a -> [ a ]
    | Choice inner -> inner.arms
  in
  let arm_actions (c : compiled) =
    if List.mem c.entry (referenced_labels c.actions) then c.actions
    else List.filter (fun n -> node_label n <> c.entry) c.actions
  in
  let choice =
    Choice
      {
        label;
        arms = List.concat_map entry_alternatives compiled_arms;
        source;
      }
  in
  {
    actions = choice :: List.concat_map arm_actions compiled_arms;
    entry = label;
    exit_label = next_label;
  }

and linearize_body ctx (steps : Normalized_ast.body) (continuation : string)
    ~(loc : Generic_ast.loc) : compiled =
  (* [loc] is the enclosing construct's location, used when the body is empty *)
  match steps with
  | [] ->
      let label = fresh_label ctx in
      let source =
        make_source ~proc_name:ctx.proc_name ~description:"[empty body]" ~loc
      in
      let a = make_action ~label ~pc_dest:(PcNext continuation) ~source () in
      { actions = [ Action a ]; entry = label; exit_label = continuation }
  | _ ->
      let rev_steps = List.rev steps in
      let first = List.hd rev_steps in
      let rest = List.tl rev_steps in
      let compiled = linearize_step ctx first continuation in
      List.fold_left
        (fun acc step ->
          let c = linearize_step ctx step acc.entry in
          {
            actions = c.actions @ acc.actions;
            entry = c.entry;
            exit_label = acc.exit_label;
          })
        compiled rest

(* ===== Module linearization ===== *)

(* Local bindings of a body, in pre-order: [var] steps and the temporaries
   bound by hoisted calls. MapInit binders never become TLA state variables
   and are not collected. *)
let rec local_var_infos proc (steps : Normalized_ast.body) : var_info list =
  List.concat_map
    (fun (step : Normalized_ast.step) ->
      match step.desc with
      | Normalized_ast.VarStep (i, _) ->
          [
            {
              tla_name = i.name;
              original = i.original;
              proc = Some proc;
              kind = Local;
            };
          ]
      | Normalized_ast.CallBindStep { bind; callee; _ } ->
          [
            {
              tla_name = bind.name;
              original = bind.original;
              proc = Some proc;
              kind = CallRet callee;
            };
          ]
      | Normalized_ast.BlockStep (While { pre; body; _ }) ->
          local_var_infos proc pre @ local_var_infos proc body
      | Normalized_ast.BlockStep (If { body; else_body; _ }) -> (
          local_var_infos proc body
          @ match else_body with Some b -> local_var_infos proc b | None -> [])
      | Normalized_ast.BlockStep (Either arms) ->
          List.concat_map (local_var_infos proc) arms
      (* with binders never become TLA state variables, like MapInit's *)
      | Normalized_ast.WithStep _ | Normalized_ast.SimpleStep _
      | Normalized_ast.EmptyStep ->
          [])
    steps

(* Each process runs its root procedure through a synthetic wrapper proc: the
   wrapper's entry action pushes the initial frame, and its discard action
   drops the unused return value and parks the process at Done. Synthesizing
   it here keeps Emit_tla a pure IR -> TLA+ pass and lets Source_map read
   wrapper actions like any others. *)
let wrapper_of_process ~process_name ~root_proc ~(loc : Generic_ast.loc) :
    proc_ir =
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
    actions = [ Action entry_action; Action discard_action ];
    entry_label;
  }

let linearize_module (m : Normalized_ast.module_def) : module_ir =
  let label_counter = ref 0 in
  let fresh_label () =
    incr label_counter;
    "L" ^ string_of_int !label_counter
  in
  let procs =
    List.map
      (fun (p : Normalized_ast.proc_def) ->
        let ctx =
          {
            proc_name = p.name;
            break_label = None;
            continue_label = None;
            fresh_label;
          }
        in
        (* A body must not fall off its end: the continuation label below
           is a sentinel, and any reachable jump to it means some path ends
           without a return. (It used to fall through to Done directly,
           which parked the process mid-call with every frame still stacked
           — silently skipping the caller's continuation and letting
           Termination pass.) Reachability comes from the compiled jump
           graph, so a procedure that never exits — [while (true)] with no
           break — needs no return: its loop's exit edge is dead and a
           literal-condition branch only references the arm it can take. *)
        let fallthrough_label = "__" ^ p.name ^ "_fallthrough__" in
        let compiled =
          linearize_body ctx p.body fallthrough_label ~loc:p.loc
        in
        let jump_targets =
          let of_dest = function
            | PcNext l -> [ l ]
            | PcBranch ({ desc = Generic_ast.BoolLit true; _ }, t, _) -> [ t ]
            | PcBranch ({ desc = Generic_ast.BoolLit false; _ }, _, f) -> [ f ]
            | PcBranch (_, t, f) -> [ t; f ]
            | PcCall _ | PcReturn -> []
          in
          let of_action (a : action) =
            of_dest a.pc_dest
            @
            match a.stack_op with StackPush (_, ret, _) -> [ ret ] | _ -> []
          in
          List.concat_map
            (function
              | Action a -> of_action a
              | Choice c -> List.concat_map of_action c.arms)
            compiled.actions
        in
        if
          compiled.entry = fallthrough_label
          || List.mem fallthrough_label jump_targets
        then
          error
            ("procedure " ^ p.name
           ^ " can fall off its end without a return: every finishing path \
              must end in `return`")
            p.loc;
        {
          proc_name = p.name;
          params = List.map (fun (i : Resolved_ast.ident) -> i.name) p.params;
          actions = compiled.actions;
          entry_label = compiled.entry;
        })
      m.procs
  in
  let processes =
    List.map
      (fun (p : Normalized_ast.process_def) ->
        {
          name = p.name;
          proc = p.proc;
          fairness = p.fairness;
          domain = p.domain;
          loc = p.loc;
          wrapper =
            wrapper_of_process ~process_name:p.name ~root_proc:p.proc ~loc:p.loc;
        })
      m.processes
  in
  let var_infos =
    List.map
      (fun (name, _) ->
        { tla_name = name; original = name; proc = None; kind = Global })
      m.var_decls
    @ List.concat_map
        (fun (p : Normalized_ast.proc_def) ->
          List.map
            (fun (i : Resolved_ast.ident) ->
              {
                tla_name = i.name;
                original = i.original;
                proc = Some p.name;
                kind = Param;
              })
            p.params
          @ local_var_infos p.name p.body)
        m.procs
  in
  (* Derived from var_infos so the two cannot drift apart *)
  let local_var_decls =
    List.filter_map
      (fun (v : var_info) ->
        match v.kind with Global -> None | _ -> Some v.tla_name)
      var_infos
  in
  {
    name = m.name;
    atoms = m.atoms;
    defs = m.defs;
    prop_defs = m.prop_defs;
    var_decls = m.var_decls;
    local_var_decls;
    var_infos;
    procs;
    processes;
  }

(* ===== Public API ===== *)

let linearize (prog : Normalized_ast.program) : module_ir list =
  List.map linearize_module prog
