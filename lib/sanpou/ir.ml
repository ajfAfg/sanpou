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
  | AssignIndex of string * Normalized_ast.expr * Normalized_ast.expr

type action = {
  label : string;
  guard : Normalized_ast.expr option;
  assignments : assignment list;
  pc_dest : pc_dest;
  stack_op : stack_op;
  source : source_info;
}

let make_action ?guard ?(assignments = []) ?(stack_op = StackNone) ~label
    ~pc_dest ~source () =
  { label; guard; assignments; pc_dest; stack_op; source }

type proc_ir = {
  proc_name : string;
  params : Ast.id list;
  actions : action list;
  entry_label : string;
}

type process_ir = {
  name : string;
  proc : string;
  fair : bool;
  lo : Normalized_ast.expr;
  hi : Normalized_ast.expr;
  loc : Ast.loc;
  wrapper : proc_ir;
      (* synthetic proc that pushes the root call's frame and discards its
         return value; synthesized by Linearize, never a callee *)
}

type module_ir = {
  name : string;
  const_defs : (string * Normalized_ast.expr) list;
  fun_defs : (string * string list * Normalized_ast.expr) list;
  var_decls : (string * Normalized_ast.expr) list;
  local_var_decls : string list;
  var_infos : var_info list;
  procs : proc_ir list;
  processes : process_ir list;
}
