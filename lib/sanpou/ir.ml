(* ===== Source info for source map ===== *)

type source_info = {
  proc_name : string;
  description : string;
  line : int;
  col : int;
}

(* ===== Variable table (TLA name -> source name) ===== *)

type var_kind = Global | Param | Local | CallRet of string (* callee proc *)

type var_info = {
  tla_name : string;
  original : string;
  proc : string option; (* owning procedure; None for module-level vars *)
  kind : var_kind;
}

(* The terminal pc value; not an action label. *)
let done_label = "Done"

(* ===== Action IR ===== *)

type stack_op =
  | StackNone
  | StackPush of string * string * Normalized_ast.expr list
    (* procedure name, return label, arguments *)
  | StackReturn of Normalized_ast.expr (* return value *)
  | StackDiscard (* pop return value after call *)
  | StackPopAssign of string (* assign return value, then pop *)

(* Where control goes after the action; fully determines pc'. *)
type pc_dest =
  | PcNext of string
  | PcBranch of Normalized_ast.expr * string * string
  | PcCall of string (* enter the named procedure at its entry label *)
  | PcReturn (* jump to the top frame's return_pc *)

type assignment =
  | AssignVar of string * Normalized_ast.expr
  | AssignIndex of string * Normalized_ast.expr list * Normalized_ast.expr
    (* variable, index path (outermost first), value *)

type action = {
  label : string;
  binders : (string * Normalized_ast.expr) list;
      (* existentially quantified over a set: the action fires for any binder
         value in the set that satisfies its guard *)
  guard : Normalized_ast.expr option;
  asserts : (Normalized_ast.expr * string) list;
      (* condition and failure message; unlike a guard, a false assert is a
         verification error, not a disabled action *)
  assignments : assignment list;
  pc_dest : pc_dest;
  stack_op : stack_op;
  source : source_info;
}

let make_action ?(binders = []) ?guard ?(asserts = []) ?(assignments = [])
    ?(stack_op = StackNone) ~label ~pc_dest ~source () =
  { label; binders; guard; asserts; assignments; pc_dest; stack_op; source }

(* A [Choice] is an either statement: one pc value whose transition is the
   disjunction of its arms' first actions. Embedding the arm actions (rather
   than jumping to their labels) makes the choice commit only to an *enabled*
   arm: an arm whose first action's guard is false is simply not offered, and
   the process blocks only when every arm is disabled — PlusCal's either
   semantics. The embedded actions are copies; the originals stay in the
   action list because internal jumps (loop back edges) may still target
   their labels. *)
type action_node =
  | Action of action
  | Choice of { label : string; arms : action list; source : source_info }

let node_label = function Action a -> a.label | Choice c -> c.label
let node_source = function Action a -> a.source | Choice c -> c.source

type proc_ir = {
  proc_name : string;
  params : Generic_ast.id list;
  actions : action_node list;
  entry_label : string;
}

type process_ir = {
  name : string;
  proc : string;
  fairness : Generic_ast.fairness;
  domain : Normalized_ast.expr;
  loc : Generic_ast.loc;
  wrapper : proc_ir;
      (* synthetic proc that pushes the root call's frame and discards its
         return value; synthesized by Linearize, never a callee *)
}

type module_ir = {
  name : string;
  const_defs : (string * Normalized_ast.expr) list;
  prop_defs : (string * Normalized_ast.expr) list;
  fun_defs : (string * string list * Normalized_ast.expr) list;
  var_decls : (string * Normalized_ast.var_init) list;
  local_var_decls : string list;
  var_infos : var_info list;
  procs : proc_ir list;
  processes : process_ir list;
}
