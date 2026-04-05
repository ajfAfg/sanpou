(* ===== Source info for source map ===== *)

type source_info = {
  proc_name : string;
  description : string;
  line : int;
  col : int;
}

(* ===== Action IR ===== *)

type stack_op =
  | StackNone
  | StackPush of string * string (* procedure name, return label *)
  | StackReturn of Cst.expr (* return value *)
  | StackDiscard (* pop return value after call *)

type pc_dest = PcNext of string | PcBranch of Cst.expr * string * string

type action = {
  label : string;
  guard : Cst.expr option;
  assignments : (string * Cst.expr) list;
  pc_dest : pc_dest;
  stack_op : stack_op;
  source : source_info;
}

type proc_ir = {
  proc_name : string;
  params : Cst.id list;
  actions : action list;
  entry_label : string;
}

type process_ir = {
  name : string;
  proc : string;
  fair : bool;
  lo : Cst.expr;
  hi : Cst.expr;
}

type module_ir = {
  name : string;
  const_defs : (string * Cst.expr) list;
  fun_defs : (string * string list * Cst.expr) list;
  var_decls : (string * Cst.expr) list;
  local_var_decls : string list;
  procs : proc_ir list;
  processes : process_ir list;
}
