(* ===== The call-normalized syntax tree =====

   [Normalize_calls] produces this tree and [Linearize] consumes it.
   Procedure calls appear only in statement position — as a [Call] simple
   statement or a [CallBindStep] binding the return value — never inside
   expressions: the callee type has no [Proc] case, so an expression that
   calls a procedure is unrepresentable and expressions can be lowered
   without threading a continuation. *)

type callee = Fun of Ast.id [@@deriving show, eq]

let callee_name (Fun name) = name

type expr = (Resolved_ast.ident, callee) Ast.expr
type assign_target = (Resolved_ast.ident, callee) Ast.assign_target
type simple_stmt = (Resolved_ast.ident, callee) Ast.simple_stmt

let equal_expr = Ast.equal_expr Resolved_ast.equal_ident equal_callee
let pp_expr = Ast.pp_expr Resolved_ast.pp_ident pp_callee

let equal_simple_stmt =
  Ast.equal_simple_stmt Resolved_ast.equal_ident equal_callee

let pp_simple_stmt = Ast.pp_simple_stmt Resolved_ast.pp_ident pp_callee

(* The step layer is this stage's own rather than [Ast]'s: normalization
   introduces [CallBindStep], and [While] gains the steps re-executed
   before every evaluation of its condition. *)
type step = step_desc Ast.node

and step_desc =
  | SimpleStep of simple_stmt list
  | EmptyStep
  | BlockStep of block_stmt
  | VarStep of Resolved_ast.ident * expr
  | CallBindStep of {
      bind : Resolved_ast.ident;
      callee : Ast.id;
      args : expr list;
    }
(* a procedure call hoisted out of an expression; [bind] is the
         compiler temporary receiving the return value *)

and block_stmt =
  | While of { pre : body; cond : expr; body : body }
    (* [pre] holds the calls hoisted out of [cond]; [continue] and the
         back edge re-enter at [pre], so the condition is re-evaluated
         from scratch on every iteration *)
  | If of { cond : expr; body : body; else_body : body option }

and body = step list [@@deriving show, eq]

(* ===== Module layer ===== *)

type proc_def = {
  name : Ast.id;
  params : Resolved_ast.ident list;
  body : body;
  loc : Ast.loc;
}
[@@deriving show, eq]

type process_def = {
  name : Ast.id;
  proc : Ast.id;
  fair : bool;
  lo : expr;
  hi : expr;
  loc : Ast.loc;
}
[@@deriving show, eq]

(* Items arrive partitioned by kind, so consumers never scan a flat item
   list. Module-level expressions are call-free by construction. *)
type module_def = {
  name : Ast.id;
  const_defs : (Ast.id * expr) list;
  fun_defs : (Ast.id * Ast.id list * expr) list;
  var_decls : (Ast.id * expr) list;
  procs : proc_def list;
  processes : process_def list;
}
[@@deriving show, eq]

type program = module_def list [@@deriving show, eq]
