(* ===== Dead-local canonicalization =====

   Frame locals keep their last value until the frame is popped, so two
   states that agree on everything a behavior can still observe count as
   distinct whenever a *dead* local differs (e.g. a scan temporary after
   its final read). This pass runs a backward liveness analysis over each
   procedure's action graph and rewrites actions so that a local that is
   dead after an action always holds the [defaultInitValue] sentinel:

   - a whole-variable or path write to a dead local becomes a write of the
     sentinel (the computed value could never be read);
   - a [StackPopAssign] binding a dead call result becomes [StackDiscard];
   - locals that arrive live (or possibly stale from a branching
     predecessor) and are dead afterwards get a sentinel write appended to
     the action.

   Resets only ride along in existing actions — adding an action of its
   own would reintroduce the interleaving point the reduction removes.
   Pop actions carry no assignments (their frame arithmetic runs while a
   transient value record tops the stack), and returning actions discard
   the whole frame, so both are left alone.

   The rewrite never changes what a behavior can read: a replaced or reset
   value is by definition never read before being overwritten, and all
   reads in an action see the pre-state, so a reset riding along with the
   last use cannot disturb that use. *)

open Ir
module StringSet = Set.Make (String)

let default_expr : Normalized_ast.expr =
  {
    Generic_ast.desc =
      Generic_ast.Var (Resolved_ast.ident Reserved_names.default_init_value);
    loc = { Generic_ast.line = 0; col = 0 };
  }

(* Variable names read by an expression. Binder names of comprehensions and
   quantifiers are globally unique after alpha conversion and never frame
   fields, so collecting them is harmless: membership in the frame set
   filters them out. *)
let rec expr_vars (acc : StringSet.t) (e : Normalized_ast.expr) : StringSet.t =
  match e.desc with
  | Generic_ast.Var i -> StringSet.add i.name acc
  | Generic_ast.IntLit _ | Generic_ast.BoolLit _ | Generic_ast.StrLit _
  | Generic_ast.AtomLit _ | Generic_ast.Self ->
      acc
  | Generic_ast.UnOp (_, x) | Generic_ast.Field (x, _) -> expr_vars acc x
  | Generic_ast.BinOp (_, a, b)
  | Generic_ast.Range (a, b)
  | Generic_ast.Subscript (a, b) ->
      expr_vars (expr_vars acc a) b
  | Generic_ast.Record fields ->
      List.fold_left (fun acc (_, x) -> expr_vars acc x) acc fields
  | Generic_ast.App (_, args)
  | Generic_ast.Builtin (_, args)
  | Generic_ast.Tuple args
  | Generic_ast.Sequence args
  | Generic_ast.SetLit args ->
      List.fold_left expr_vars acc args
  | Generic_ast.MapInit { domain; value; _ } ->
      expr_vars (expr_vars acc domain) value
  | Generic_ast.SetComp { domain; pred; _ } ->
      expr_vars (expr_vars acc domain) pred
  | Generic_ast.IfExpr (a, b, c) -> expr_vars (expr_vars (expr_vars acc a) b) c
  | Generic_ast.Quant { domain; body; _ } ->
      expr_vars (expr_vars acc domain) body

(* Frame locals the action reads: locals in any evaluated expression, plus
   path-write targets (an EXCEPT update reads the previous value). *)
let action_reads ~(frame : StringSet.t) (a : action) : StringSet.t =
  let of_exprs = List.fold_left expr_vars StringSet.empty (Ir.action_exprs a) in
  let of_path_targets =
    List.filter_map
      (function AssignPath (v, _, _) -> Some v | AssignVar _ -> None)
      a.assignments
    |> StringSet.of_list
  in
  StringSet.inter (StringSet.union of_exprs of_path_targets) frame

(* Frame locals the action definitely overwrites (whole-variable writes and
   pop bindings kill liveness; a path write does not, it reads the rest of
   the old value). *)
let action_kills ~(frame : StringSet.t) (a : action) : StringSet.t =
  let assigned =
    List.filter_map
      (function AssignVar (v, _) -> Some v | AssignPath _ -> None)
      a.assignments
  in
  let popped = match a.stack_op with StackPopAssign v -> [ v ] | _ -> [] in
  StringSet.inter (StringSet.of_list (assigned @ popped)) frame

(* Intra-procedure control successors. A call transfers to the callee, but
   the caller's locals become relevant again only at the pushed return
   label; the callee cannot touch them, so the edge short-circuits the
   call. A return ends the frame's life. *)
let successors (a : action) : string list =
  match a.pc_dest with
  | PcNext l -> [ l ]
  | PcBranch (_, t, f) -> [ t; f ]
  | PcCall _ -> (
      match a.stack_op with StackPush (_, ret, _) -> [ ret ] | _ -> [])
  | PcReturn -> []

let node_arms = function Action a -> [ a ] | Choice c -> c.arms

let canonicalize_proc ~(frame : StringSet.t) ~(params : StringSet.t)
    (proc : proc_ir) : proc_ir =
  let nodes = proc.actions in
  (* --- Backward liveness fixpoint over labels --- *)
  let live_in_label = Hashtbl.create 64 in
  let label_live l =
    Option.value (Hashtbl.find_opt live_in_label l) ~default:StringSet.empty
  in
  let live_out (a : action) : StringSet.t =
    List.fold_left
      (fun acc l -> StringSet.union acc (label_live l))
      StringSet.empty (successors a)
  in
  let live_in (a : action) : StringSet.t =
    StringSet.union (action_reads ~frame a)
      (StringSet.diff (live_out a) (action_kills ~frame a))
  in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter
      (fun node ->
        let li =
          List.fold_left
            (fun acc arm -> StringSet.union acc (live_in arm))
            StringSet.empty (node_arms node)
        in
        let label = node_label node in
        if not (StringSet.equal li (label_live label)) then begin
          Hashtbl.replace live_in_label label li;
          changed := true
        end)
      nodes
  done;
  (* Locals possibly holding a non-sentinel value on entry to each label:
     the union of the predecessors' live-outs. A branching predecessor may
     keep a local live for its other branch, so the local arrives stale
     here even though nothing on this path reads it. Parameters arrive
     possibly non-sentinel at the entry label (the frame push bound them);
     every other field starts as the sentinel. *)
  let incoming = Hashtbl.create 64 in
  let add_incoming l s =
    let prev =
      Option.value (Hashtbl.find_opt incoming l) ~default:StringSet.empty
    in
    Hashtbl.replace incoming l (StringSet.union prev s)
  in
  add_incoming proc.entry_label params;
  List.iter
    (fun node ->
      List.iter
        (fun arm ->
          let lo = live_out arm in
          List.iter (fun l -> add_incoming l lo) (successors arm))
        (node_arms node))
    nodes;
  let incoming l =
    Option.value (Hashtbl.find_opt incoming l) ~default:StringSet.empty
  in
  (* --- Rewrite --- *)
  let rewrite_action (label : string) (a : action) : action =
    match a.stack_op with
    | StackDiscard | StackReturn _ ->
        (* Pops carry no assignments; a return discards the frame. *)
        a
    | StackPopAssign v ->
        if StringSet.mem v frame && not (StringSet.mem v (live_out a)) then
          { a with stack_op = StackDiscard }
        else a
    | StackNone | StackPush _ ->
        let lo = live_out a in
        let dead v = StringSet.mem v frame && not (StringSet.mem v lo) in
        let assignments =
          List.map
            (function
              | AssignVar (v, e) when dead v ->
                  (* Keep idempotence: a reset stays a reset. *)
                  if e == default_expr then AssignVar (v, e)
                  else AssignVar (v, default_expr)
              | AssignPath (v, _, _) when dead v -> AssignVar (v, default_expr)
              | other -> other)
            a.assignments
        in
        let written =
          List.filter_map
            (function AssignVar (v, _) | AssignPath (v, _, _) -> Some v)
            assignments
          |> StringSet.of_list
        in
        let resets =
          StringSet.diff
            (StringSet.inter
               (StringSet.union (live_in a) (incoming label))
               frame)
            (StringSet.union lo written)
        in
        {
          a with
          assignments =
            assignments
            @ List.map
                (fun v -> AssignVar (v, default_expr))
                (StringSet.elements resets);
        }
  in
  let actions =
    List.map
      (function
        | Action a -> Action (rewrite_action a.label a)
        | Choice c ->
            Choice { c with arms = List.map (rewrite_action c.label) c.arms })
      nodes
  in
  { proc with actions }

let canonicalize_module (ir : module_ir) : module_ir =
  let procs =
    List.map
      (fun (proc : proc_ir) ->
        let fields kind_filter =
          List.filter_map
            (fun (v : var_info) ->
              match v.proc with
              | Some owner when owner = proc.proc_name && kind_filter v.kind ->
                  Some v.tla_name
              | _ -> None)
            ir.var_infos
          |> StringSet.of_list
        in
        let frame = fields (fun _ -> true) in
        let params = fields (function Param -> true | _ -> false) in
        if StringSet.is_empty frame then proc
        else canonicalize_proc ~frame ~params proc)
      ir.procs
  in
  { ir with procs }
