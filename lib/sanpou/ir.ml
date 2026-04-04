open Tla.Tla_ast

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
  | StackReturn of tla_expr (* return value *)
  | StackDiscard (* pop return value after call *)

type pc_dest = PcNext of string | PcBranch of tla_expr * string * string

type action = {
  label : string;
  guard : tla_expr option;
  assignments : (string * tla_expr) list;
  pc_dest : pc_dest;
  stack_op : stack_op;
  unchanged : string list;
  source : source_info;
}

type proc_ir = {
  proc_name : string;
  params : Cst.id list;
  actions : action list;
  entry_label : string;
}

type process_ir = { name : string; proc : string; lo : tla_expr; hi : tla_expr }

type module_ir = {
  name : string;
  const_defs : (string * tla_expr) list;
  fun_defs : (string * string list * tla_expr) list;
  var_decls : (string * tla_expr) list;
  local_var_decls : string list;
  procs : proc_ir list;
  processes : process_ir list;
}
