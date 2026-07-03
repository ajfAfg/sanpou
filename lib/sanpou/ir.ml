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

(* ===== Process wrapper labels (shared by emit_tla and source_map) ===== *)

let wrapper_entry_label process_name = "__w_" ^ process_name ^ "_entry__"
let wrapper_discard_label process_name = "__w_" ^ process_name ^ "_discard__"

(* ===== Action IR ===== *)

type stack_op =
  | StackNone
  | StackPush of string * string * Ast.expr list
    (* procedure name, return label, arguments *)
  | StackReturn of Ast.expr (* return value *)
  | StackDiscard (* pop return value after call *)
  | StackPopAssign of string (* assign return value, then pop *)

type pc_dest = PcNext of string | PcBranch of Ast.expr * string * string

type assignment =
  | AssignVar of string * Ast.expr
  | AssignIndex of string * Ast.expr * Ast.expr

type action = {
  label : string;
  guard : Ast.expr option;
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
  lo : Ast.expr;
  hi : Ast.expr;
  loc : Ast.loc;
}

type module_ir = {
  name : string;
  const_defs : (string * Ast.expr) list;
  fun_defs : (string * string list * Ast.expr) list;
  var_decls : (string * Ast.expr) list;
  local_var_decls : string list;
  var_infos : var_info list;
  procs : proc_ir list;
  processes : process_ir list;
}
