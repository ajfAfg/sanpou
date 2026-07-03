open Ast

(* ===== Pretty printer (canonical formatting) =====

   The AST carries no parenthesization, so the printer re-inserts the
   parentheses the grammar requires: each subexpression is printed within a
   minimum precedence level and wrapped in parens when it binds looser. *)

let binop_str = function
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Lt -> "<"
  | LtEq -> "<="
  | GtEq -> ">="
  | Eq -> "=="
  | Neq -> "!="
  | And -> "&&"
  | Or -> "||"

let unop_str = function Neg -> "-"

(* Higher binds tighter; mirrors the stratified grammar rules
   (or < and < comparison < add < mult < subscript < unary < atom). *)
let binop_prec = function
  | Or -> 1
  | And -> 2
  | Lt | LtEq | GtEq | Eq | Neq -> 3
  | Plus | Minus -> 4
  | Mult -> 5

let prec (e : expr) =
  match e.desc with
  | BinOp (op, _, _) -> binop_prec op
  | Subscript _ -> 6
  | UnOp _ -> 7
  | IntLit _ | BoolLit _ | Var _ | Self | App _ | Builtin _ | MapInit _
  | Tuple _ | Sequence _ -> 8

(* [rename] maps identifiers back to their display names (e.g. undoing
   alpha conversion). It applies only to variable positions, never to
   function/procedure names. *)
let rec pretty_expr ?(rename = Fun.id) (e : expr) =
  let at level e' =
    let s = pretty_expr ~rename e' in
    if prec e' < level then "(" ^ s ^ ")" else s
  in
  match e.desc with
  | IntLit value -> string_of_int value
  | BoolLit value -> if value then "true" else "false"
  | Var name -> rename name
  | Self -> "self"
  | UnOp (op, rhs) -> unop_str op ^ at 7 rhs
  | BinOp (op, lhs, rhs) ->
      let level = binop_prec op in
      (* left-associative: the right operand must bind strictly tighter *)
      at level lhs ^ " " ^ binop_str op ^ " " ^ at (level + 1) rhs
  | App (name, args) ->
      name ^ "(" ^ String.concat ", " (List.map (pretty_expr ~rename) args) ^ ")"
  | Builtin (b, args) ->
      Builtin.name b ^ "("
      ^ String.concat ", " (List.map (pretty_expr ~rename) args)
      ^ ")"
  | Subscript (lhs, index) -> at 6 lhs ^ "[" ^ pretty_expr ~rename index ^ "]"
  | MapInit { binder; lo; hi; value } ->
      "{ " ^ rename binder ^ " in " ^ pretty_expr ~rename lo ^ ".."
      ^ pretty_expr ~rename hi ^ ": "
      ^ pretty_expr ~rename value
      ^ " }"
  | Tuple elems -> (
      match elems with
      | [] -> "()"
      | [ e ] -> "(" ^ pretty_expr ~rename e ^ ",)"
      | es -> "(" ^ String.concat ", " (List.map (pretty_expr ~rename) es) ^ ")")
  | Sequence elems ->
      "[" ^ String.concat ", " (List.map (pretty_expr ~rename) elems) ^ "]"

let pretty_assign_target ?(rename = Fun.id) = function
  | VarTarget name -> rename name
  | SubscriptTarget (name, index) ->
      rename name ^ "[" ^ pretty_expr ~rename index ^ "]"

let pretty_simple_stmt ?(rename = Fun.id) (stmt : simple_stmt) =
  match stmt.desc with
  | Assign (target, value) ->
      pretty_assign_target ~rename target ^ " = " ^ pretty_expr ~rename value
  | Call (name, args) ->
      name ^ "(" ^ String.concat ", " (List.map (pretty_expr ~rename) args) ^ ")"
  | Return value -> "return " ^ pretty_expr ~rename value
  | Break -> "break"
  | Continue -> "continue"
  | Await cond -> "await " ^ pretty_expr ~rename cond

let rec pretty_step indent (step : step) =
  match step.desc with
  | SimpleStep stmts ->
      indent ^ String.concat ", " (List.map pretty_simple_stmt stmts) ^ ";\n"
  | EmptyStep -> indent ^ ";\n"
  | BlockStep stmt -> pretty_block_stmt indent stmt
  | VarStep (name, value) ->
      indent ^ "var " ^ name ^ " = " ^ pretty_expr value ^ ";\n"

and pretty_block_stmt indent = function
  | While { cond; body } ->
      indent ^ "while (" ^ pretty_expr cond ^ ") {\n"
      ^ pretty_body (indent ^ "  ") body
      ^ indent ^ "}\n"
  | If { cond; body; else_body } -> (
      indent ^ "if (" ^ pretty_expr cond ^ ") {\n"
      ^ pretty_body (indent ^ "  ") body
      ^ indent ^ "}"
      ^
      match else_body with
      | Some else_body ->
          " else {\n" ^ pretty_body (indent ^ "  ") else_body ^ indent ^ "}\n"
      | None -> "\n")

and pretty_body indent steps =
  String.concat "" (List.map (pretty_step indent) steps)

let pretty_item indent (item : item) =
  match item.desc with
  | ConstDef { name; value } ->
      indent ^ "def " ^ name ^ " = " ^ pretty_expr value ^ ";\n"
  | FunDef { name; params; body_expr } ->
      indent ^ "def " ^ name ^ "(" ^ String.concat ", " params ^ ") = "
      ^ pretty_expr body_expr ^ ";\n"
  | VarDecl { name; value } ->
      indent ^ "var " ^ name ^ " = " ^ pretty_expr value ^ ";\n"
  | ProcDef { name; params; body } ->
      indent ^ "fn " ^ name ^ "(" ^ String.concat ", " params ^ ") {\n"
      ^ pretty_body (indent ^ "  ") body
      ^ indent ^ "}\n"
  | Process { name; proc; fair; lo; hi } ->
      indent
      ^ (if fair then "fair process " else "process ")
      ^ name ^ " = " ^ proc ^ " in " ^ pretty_expr lo ^ ".." ^ pretty_expr hi
      ^ ";\n"

let pretty_module_def m =
  "mod " ^ m.mod_name ^ " {\n"
  ^ String.concat "" (List.map (pretty_item "  ") m.items)
  ^ "}\n"

let print_pretty (prog : program) =
  String.concat "" (List.map pretty_module_def prog)
