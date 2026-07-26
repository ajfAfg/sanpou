(* ===== Action fusion (static partial-order reduction) =====

   An action that touches only process-local state — pc, the stack, frame
   locals — commutes with every action of every other process (a both-mover),
   so composing it with its successor does not change the shared-state
   projection TLC checks: invariants and temporal properties can only
   reference module-level names, and a local action is always enabled so
   deadlock existence is preserved too. Fusing such actions removes
   interleaving points that only enumerate semantically identical
   schedules, which shrinks the reachable state count multiplicatively in
   the process count.

   The one rule that keeps fusion sound is: a fused action performs AT MOST
   ONE shared access (an original action that reads or writes any module
   variable). Merging two shared accesses would make two sequential
   observations simultaneous — a strictly coarser model that can mask real
   interleavings.

   Sequential composition is expressed in the IR's simultaneous-assignment
   form by substitution: the follower's expressions see the leader's local
   writes by replacing each read of a written local with the written
   expression (all reads in one action see the pre-state, so the leader's
   right-hand sides are already in pre-state terms). Alpha conversion makes
   every binder and local name unique, so naive substitution cannot
   capture.

   Known, deliberate semantic shifts (documented in the issue tracker):
   - A follower's guard becomes the fused action's guard, so a blocked
     process is reported one action earlier than before (same deadlock,
     earlier trace position).
   - A fused action maps to the leader's source location; its description
     concatenates the constituents'. *)

open Ir
module StringSet = Set.Make (String)

(* ===== Global-read analysis ===== *)

(* Module-level defs may read state variables (e.g. an invariant def
   [def mutexOk = cs <= 1]), so an expression mentioning such a def reads
   shared state transitively. Sequential name resolution means a def only
   references earlier defs: one left-to-right pass suffices. *)
let global_reading_defs ~(is_global : string -> bool)
    (defs : Normalized_ast.def list) : StringSet.t =
  let rec expr_reads gdefs (e : Normalized_ast.expr) : bool =
    let reads = expr_reads gdefs in
    match e.desc with
    | Generic_ast.Var i -> is_global i.name || StringSet.mem i.name gdefs
    | Generic_ast.App (Normalized_ast.Fun f, args) ->
        StringSet.mem f gdefs || List.exists reads args
    | Generic_ast.IntLit _ | Generic_ast.BoolLit _ | Generic_ast.StrLit _
    | Generic_ast.AtomLit _ | Generic_ast.Self ->
        false
    | Generic_ast.UnOp (_, e) | Generic_ast.Field (e, _) -> reads e
    | Generic_ast.BinOp (_, a, b)
    | Generic_ast.Range (a, b)
    | Generic_ast.Subscript (a, b) ->
        reads a || reads b
    | Generic_ast.Record fields -> List.exists (fun (_, e) -> reads e) fields
    | Generic_ast.Builtin (_, args)
    | Generic_ast.Tuple args
    | Generic_ast.Sequence args
    | Generic_ast.SetLit args ->
        List.exists reads args
    | Generic_ast.MapInit { domain; value; _ } -> reads domain || reads value
    | Generic_ast.SetComp { domain; pred; _ } -> reads domain || reads pred
    | Generic_ast.IfExpr (a, b, c) -> reads a || reads b || reads c
    | Generic_ast.Quant { domain; body; _ } -> reads domain || reads body
  in
  List.fold_left
    (fun gdefs def ->
      let name, body =
        match def with
        | Normalized_ast.DefConst (name, e) -> (name, e)
        | Normalized_ast.DefFun (name, _, e) -> (name, e)
      in
      if expr_reads gdefs body then StringSet.add name gdefs else gdefs)
    StringSet.empty defs

type env = {
  is_global : string -> bool;
  is_local : string -> bool;
  gdefs : StringSet.t;
}

let rec expr_reads_global env (e : Normalized_ast.expr) : bool =
  let reads = expr_reads_global env in
  match e.desc with
  | Generic_ast.Var i -> env.is_global i.name || StringSet.mem i.name env.gdefs
  | Generic_ast.App (Normalized_ast.Fun f, args) ->
      StringSet.mem f env.gdefs || List.exists reads args
  | Generic_ast.IntLit _ | Generic_ast.BoolLit _ | Generic_ast.StrLit _
  | Generic_ast.AtomLit _ | Generic_ast.Self ->
      false
  | Generic_ast.UnOp (_, e) | Generic_ast.Field (e, _) -> reads e
  | Generic_ast.BinOp (_, a, b)
  | Generic_ast.Range (a, b)
  | Generic_ast.Subscript (a, b) ->
      reads a || reads b
  | Generic_ast.Record fields -> List.exists (fun (_, e) -> reads e) fields
  | Generic_ast.Builtin (_, args)
  | Generic_ast.Tuple args
  | Generic_ast.Sequence args
  | Generic_ast.SetLit args ->
      List.exists reads args
  | Generic_ast.MapInit { domain; value; _ } -> reads domain || reads value
  | Generic_ast.SetComp { domain; pred; _ } -> reads domain || reads pred
  | Generic_ast.IfExpr (a, b, c) -> reads a || reads b || reads c
  | Generic_ast.Quant { domain; body; _ } -> reads domain || reads body

(* Does the action observe or modify any module variable? Reads and writes
   both count: either way the action marks one instant of shared-state
   interaction, and a fused action may contain at most one such instant. *)
let touches_shared env (a : action) : bool =
  List.exists
    (function AssignVar (v, _) | AssignPath (v, _, _) -> env.is_global v)
    a.assignments
  || (match a.stack_op with StackPopAssign v -> env.is_global v | _ -> false)
  || List.exists (expr_reads_global env) (Ir.action_exprs a)

(* ===== Substitution ===== *)

let rec subst_expr (map : (string * Normalized_ast.expr) list)
    (e : Normalized_ast.expr) : Normalized_ast.expr =
  if map = [] then e
  else
    let sub = subst_expr map in
    let without (i : Resolved_ast.ident) =
      List.filter (fun (v, _) -> v <> i.name) map
    in
    let desc =
      match e.desc with
      | Generic_ast.Var i -> (
          match List.assoc_opt i.name map with
          | Some r -> r.Generic_ast.desc
          | None -> e.desc)
      | Generic_ast.IntLit _ | Generic_ast.BoolLit _ | Generic_ast.StrLit _
      | Generic_ast.AtomLit _ | Generic_ast.Self ->
          e.desc
      | Generic_ast.UnOp (op, x) -> Generic_ast.UnOp (op, sub x)
      | Generic_ast.BinOp (op, a, b) -> Generic_ast.BinOp (op, sub a, sub b)
      | Generic_ast.App (f, args) -> Generic_ast.App (f, List.map sub args)
      | Generic_ast.Builtin (b, args) ->
          Generic_ast.Builtin (b, List.map sub args)
      | Generic_ast.Subscript (a, b) -> Generic_ast.Subscript (sub a, sub b)
      | Generic_ast.Field (r, f) -> Generic_ast.Field (sub r, f)
      | Generic_ast.Record fields ->
          Generic_ast.Record (List.map (fun (l, x) -> (l, sub x)) fields)
      | Generic_ast.Range (a, b) -> Generic_ast.Range (sub a, sub b)
      | Generic_ast.MapInit { binder; domain; value } ->
          (* Binders are globally unique after alpha conversion, so shadowing
             cannot happen; dropping the binder from the map is defensive. *)
          Generic_ast.MapInit
            {
              binder;
              domain = sub domain;
              value = subst_expr (without binder) value;
            }
      | Generic_ast.SetLit elems -> Generic_ast.SetLit (List.map sub elems)
      | Generic_ast.SetComp { binder; domain; pred } ->
          Generic_ast.SetComp
            {
              binder;
              domain = sub domain;
              pred = subst_expr (without binder) pred;
            }
      | Generic_ast.Tuple elems -> Generic_ast.Tuple (List.map sub elems)
      | Generic_ast.Sequence elems -> Generic_ast.Sequence (List.map sub elems)
      | Generic_ast.IfExpr (a, b, c) -> Generic_ast.IfExpr (sub a, sub b, sub c)
      | Generic_ast.Quant { quant; binder; domain; body } ->
          Generic_ast.Quant
            {
              quant;
              binder;
              domain = sub domain;
              body = subst_expr (without binder) body;
            }
    in
    { e with desc }

let rec count_var (name : string) (e : Normalized_ast.expr) : int =
  let sum es = List.fold_left (fun n e -> n + count_var name e) 0 es in
  match e.desc with
  | Generic_ast.Var i -> if i.name = name then 1 else 0
  | Generic_ast.IntLit _ | Generic_ast.BoolLit _ | Generic_ast.StrLit _
  | Generic_ast.AtomLit _ | Generic_ast.Self ->
      0
  | Generic_ast.UnOp (_, x) | Generic_ast.Field (x, _) -> count_var name x
  | Generic_ast.BinOp (_, a, b)
  | Generic_ast.Range (a, b)
  | Generic_ast.Subscript (a, b) ->
      sum [ a; b ]
  | Generic_ast.Record fields -> sum (List.map snd fields)
  | Generic_ast.App (_, args)
  | Generic_ast.Builtin (_, args)
  | Generic_ast.Tuple args
  | Generic_ast.Sequence args
  | Generic_ast.SetLit args ->
      sum args
  | Generic_ast.MapInit { domain; value; _ } -> sum [ domain; value ]
  | Generic_ast.SetComp { domain; pred; _ } -> sum [ domain; pred ]
  | Generic_ast.IfExpr (a, b, c) -> sum [ a; b; c ]
  | Generic_ast.Quant { domain; body; _ } -> sum [ domain; body ]

let rec expr_size (e : Normalized_ast.expr) : int =
  let sum es = List.fold_left (fun n e -> n + expr_size e) 0 es in
  1
  +
  match e.desc with
  | Generic_ast.IntLit _ | Generic_ast.BoolLit _ | Generic_ast.StrLit _
  | Generic_ast.AtomLit _ | Generic_ast.Self | Generic_ast.Var _ ->
      0
  | Generic_ast.UnOp (_, x) | Generic_ast.Field (x, _) -> expr_size x
  | Generic_ast.BinOp (_, a, b)
  | Generic_ast.Range (a, b)
  | Generic_ast.Subscript (a, b) ->
      sum [ a; b ]
  | Generic_ast.Record fields -> sum (List.map snd fields)
  | Generic_ast.App (_, args)
  | Generic_ast.Builtin (_, args)
  | Generic_ast.Tuple args
  | Generic_ast.Sequence args
  | Generic_ast.SetLit args ->
      sum args
  | Generic_ast.MapInit { domain; value; _ } -> sum [ domain; value ]
  | Generic_ast.SetComp { domain; pred; _ } -> sum [ domain; pred ]
  | Generic_ast.IfExpr (a, b, c) -> sum [ a; b; c ]
  | Generic_ast.Quant { domain; body; _ } -> sum [ domain; body ]

(* ===== Pairwise fusion ===== *)

let subst_assignment map = function
  | AssignVar (v, e) -> AssignVar (v, subst_expr map e)
  | AssignPath (v, path, e) ->
      AssignPath
        ( v,
          List.map
            (function
              | Generic_ast.AccIndex i ->
                  Generic_ast.AccIndex (subst_expr map i)
              | Generic_ast.AccField f -> Generic_ast.AccField f)
            path,
          subst_expr map e )

let subst_stack_op map = function
  | StackPush (p, ret, args) ->
      StackPush (p, ret, List.map (subst_expr map) args)
  | StackReturn e -> StackReturn (subst_expr map e)
  | (StackNone | StackDiscard | StackPopAssign _) as op -> op

let subst_pc_dest map = function
  | PcBranch (cond, t, f) -> PcBranch (subst_expr map cond, t, f)
  | (PcNext _ | PcCall _ | PcReturn) as d -> d

let conjoin (a : Normalized_ast.expr option) (b : Normalized_ast.expr option) :
    Normalized_ast.expr option =
  match (a, b) with
  | None, g | g, None -> g
  | Some ga, Some gb ->
      Some
        {
          Generic_ast.desc = Generic_ast.BinOp (Generic_ast.And, ga, gb);
          loc = ga.Generic_ast.loc;
        }

(* Guards against pathological expression growth through repeated
   substitution; generous enough that real code never hits them. *)
let max_subst_occurrences = 8
let max_subst_size = 120

(* Fuse leader [a] with follower [b] (the action [a] unconditionally jumps
   to). Returns [None] when the pair is not fusable. *)
let fuse_pair env (a : action) (b : action) : action option =
  let a_shared = touches_shared env a and b_shared = touches_shared env b in
  if a_shared && b_shared then None
  else if a.stack_op <> StackNone then None
  else if
    (* Caller-side pops run while a transient value record tops the stack;
       their frame arithmetic is not expressible in a composed action. *)
    match b.stack_op with
    | StackDiscard | StackPopAssign _ -> true
    | StackNone | StackPush _ | StackReturn _ -> false
  then None
  else if
    (* A leader assert must not silently vanish behind a follower guard
       that blocks: originally the assert fired even when the follower
       stayed disabled. *)
    a.asserts <> [] && b.guard <> None
  then None
  else if
    (* Substitution rewrites whole-variable reads only; a leader path-write
       to a local would need EXCEPT-composition inside the substitution. *)
    List.exists
      (function AssignPath (v, _, _) -> env.is_local v | AssignVar _ -> false)
      a.assignments
  then None
  else
    (* Last write wins: fused leaders may carry several whole-variable
       writes to the same local. *)
    let map =
      List.fold_left
        (fun acc -> function
          | AssignVar (v, e) when env.is_local v ->
              (v, e) :: List.remove_assoc v acc
          | _ -> acc)
        [] a.assignments
    in
    let b_exprs = action_exprs b in
    let within_budget (v, e) =
      let occurrences =
        List.fold_left (fun n bexpr -> n + count_var v bexpr) 0 b_exprs
      in
      occurrences <= max_subst_occurrences
      && (occurrences <= 1 || expr_size e <= max_subst_size)
    in
    if not (List.for_all within_budget map) then None
    else
      Some
        {
          label = a.label;
          binders =
            a.binders @ List.map (fun (v, d) -> (v, subst_expr map d)) b.binders;
          guard = conjoin a.guard (Option.map (subst_expr map) b.guard);
          asserts =
            a.asserts @ List.map (fun (c, m) -> (subst_expr map c, m)) b.asserts;
          assignments =
            a.assignments @ List.map (subst_assignment map) b.assignments;
          pc_dest = subst_pc_dest map b.pc_dest;
          stack_op = subst_stack_op map b.stack_op;
          source =
            {
              a.source with
              description = a.source.description ^ "; " ^ b.source.description;
            };
        }

(* ===== Per-procedure fusion driver ===== *)

let fuse_proc env (proc : proc_ir) : proc_ir =
  let nodes = Hashtbl.create 64 in
  let order = List.map node_label proc.actions in
  List.iter (fun n -> Hashtbl.replace nodes (node_label n) n) proc.actions;
  (* Constituent labels per node: fusing is refused when the sets overlap,
     which caps absorption at one traversal of any loop and guarantees
     termination. *)
  let constituents = Hashtbl.create 64 in
  List.iter
    (fun l -> Hashtbl.replace constituents l (StringSet.singleton l))
    order;
  let cons l =
    Option.value
      (Hashtbl.find_opt constituents l)
      ~default:(StringSet.singleton l)
  in
  let try_fuse_action (owner_label : string) (a : action) : action option =
    match a.pc_dest with
    | PcNext target when target <> Ir.done_label -> (
        if not (StringSet.disjoint (cons owner_label) (cons target)) then None
        else
          match Hashtbl.find_opt nodes target with
          | Some (Action b) -> fuse_pair env a b
          | Some (Choice _) | None -> None)
    | _ -> None
  in
  let fuse_into_choice (a : action) : action_node option =
    (* A leader jumping to a Choice distributes over the arms:
       A;(arm1 | arm2) = (A;arm1) | (A;arm2). *)
    match a.pc_dest with
    | PcNext target when target <> Ir.done_label -> (
        if not (StringSet.disjoint (cons a.label) (cons target)) then None
        else
          match Hashtbl.find_opt nodes target with
          | Some (Choice c) ->
              let fused_arms = List.map (fuse_pair env a) c.arms in
              if List.for_all Option.is_some fused_arms then
                Some
                  (Choice
                     {
                       label = a.label;
                       arms =
                         List.map
                           (fun arm ->
                             { (Option.get arm) with label = a.label })
                           fused_arms;
                       source = a.source;
                     })
              else None
          | _ -> None)
    | _ -> None
  in
  let record_fusion owner target =
    Hashtbl.replace constituents owner
      (StringSet.union (cons owner) (cons target))
  in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun label ->
        match Hashtbl.find_opt nodes label with
        | None -> ()
        | Some (Action a) -> (
            match try_fuse_action label a with
            | Some fused ->
                let target =
                  match a.pc_dest with PcNext l -> l | _ -> assert false
                in
                record_fusion label target;
                Hashtbl.replace nodes label (Action fused);
                changed := true
            | None -> (
                match fuse_into_choice a with
                | Some fused_choice ->
                    let target =
                      match a.pc_dest with PcNext l -> l | _ -> assert false
                    in
                    record_fusion label target;
                    Hashtbl.replace nodes label fused_choice;
                    changed := true
                | None -> ()))
        | Some (Choice c) ->
            let fused_any = ref false in
            let arms =
              List.map
                (fun arm ->
                  match try_fuse_action label arm with
                  | Some fused ->
                      let target =
                        match arm.pc_dest with
                        | PcNext l -> l
                        | _ -> assert false
                      in
                      record_fusion label target;
                      fused_any := true;
                      fused
                  | None -> arm)
                c.arms
            in
            if !fused_any then begin
              Hashtbl.replace nodes label (Choice { c with arms });
              changed := true
            end)
      order
  done;
  let actions = List.filter_map (fun l -> Hashtbl.find_opt nodes l) order in
  { proc with actions }

(* ===== Reachability pruning ===== *)

(* Fusion bypasses follower labels; a label no process can reach is dead
   weight in the emitted module (an action whose pc test can never hold).
   Roots: procedure entry labels (PcCall targets) and every pushed return
   label (PcReturn targets), wrappers included. *)
let prune_unreachable (ir : module_ir) : module_ir =
  let all_procs =
    ir.procs @ List.map (fun (p : process_ir) -> p.wrapper) ir.processes
  in
  let node_actions = function Action a -> [ a ] | Choice c -> c.arms in
  let push_return_labels =
    List.concat_map
      (fun (p : proc_ir) ->
        List.concat_map
          (fun n ->
            List.filter_map
              (fun (a : action) ->
                match a.stack_op with
                | StackPush (_, ret, _) -> Some ret
                | _ -> None)
              (node_actions n))
          p.actions)
      all_procs
  in
  let roots =
    List.map (fun (p : proc_ir) -> p.entry_label) all_procs @ push_return_labels
  in
  let node_map = Hashtbl.create 256 in
  List.iter
    (fun (p : proc_ir) ->
      List.iter (fun n -> Hashtbl.replace node_map (node_label n) n) p.actions)
    all_procs;
  let reachable = Hashtbl.create 256 in
  let rec visit label =
    if not (Hashtbl.mem reachable label) then begin
      Hashtbl.replace reachable label ();
      match Hashtbl.find_opt node_map label with
      | None -> ()
      | Some node ->
          List.iter
            (fun (a : action) ->
              (match a.pc_dest with
              | PcNext l -> visit l
              | PcBranch (_, t, f) ->
                  visit t;
                  visit f
              | PcCall _ | PcReturn -> ());
              match a.stack_op with
              | StackPush (_, ret, _) -> visit ret
              | _ -> ())
            (node_actions node)
    end
  in
  List.iter visit roots;
  let prune (p : proc_ir) =
    {
      p with
      actions =
        List.filter (fun n -> Hashtbl.mem reachable (node_label n)) p.actions;
    }
  in
  {
    ir with
    procs = List.map prune ir.procs;
    processes =
      List.map
        (fun (p : process_ir) -> { p with wrapper = prune p.wrapper })
        ir.processes;
  }

(* ===== Public API ===== *)

let fuse_module (ir : module_ir) : module_ir =
  let globals = List.map fst ir.var_decls in
  let env =
    {
      is_global = (fun v -> List.mem v globals);
      is_local = (fun v -> List.mem v ir.local_var_decls);
      gdefs =
        global_reading_defs ~is_global:(fun v -> List.mem v globals) ir.defs;
    }
  in
  prune_unreachable { ir with procs = List.map (fuse_proc env) ir.procs }
