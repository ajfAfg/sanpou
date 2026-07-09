open Generic_ast

(* ===== Pretty printer (canonical formatting) =====

   The AST carries no parenthesization, so the printer re-inserts the
   parentheses the grammar requires: each subexpression is printed within a
   minimum precedence level and wrapped in parens when it binds looser.

   The tree is parameterized by its name type, so every function takes a
   [name_of] rendering the variable names and a [callee_of] rendering the
   applied callees; [print_pretty] fixes both to the surface tree, and
   alpha-converted trees pass [Resolved_ast.display] and
   [Resolved_ast.callee_name] to print source names. *)

let binop_str = function
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Lt -> "<"
  | Gt -> ">"
  | LtEq -> "<="
  | GtEq -> ">="
  | Eq -> "=="
  | Neq -> "!="
  | And -> "&&"
  | Or -> "||"
  | In -> "in"

let unop_str = function Neg -> "-" | Not -> "!"

(* Higher binds tighter; mirrors the stratified grammar rules
   (or < and < comparison < range < add < mult < subscript < unary < atom). *)
let binop_prec = function
  | Or -> 1
  | And -> 2
  | Lt | Gt | LtEq | GtEq | Eq | Neq | In -> 3
  | Plus | Minus -> 5
  | Mult | Div | Mod -> 6

let prec (e : ('n, 'c) expr) =
  match e.desc with
  | BinOp (op, _, _) -> binop_prec op
  | Range _ -> 4
  | Subscript _ | Field _ -> 7
  | UnOp _ -> 8
  | IntLit _ | BoolLit _ | StrLit _ | AtomLit _ | Var _ | Self | App _
  | Builtin _ | MapInit _ | SetLit _ | SetComp _ | Record _ | Tuple _
  | Sequence _ | IfExpr _ | Quant _ ->
      9

let rec pretty_expr name_of callee_of (e : ('n, 'c) expr) =
  let at level e' =
    let s = pretty_expr name_of callee_of e' in
    if prec e' < level then "(" ^ s ^ ")" else s
  in
  match e.desc with
  | IntLit value -> string_of_int value
  | BoolLit value -> if value then "true" else "false"
  | StrLit s -> "\"" ^ s ^ "\""
  | AtomLit a -> "`" ^ a
  | Var n -> name_of n
  | Self -> "self"
  | UnOp (op, rhs) -> unop_str op ^ at 8 rhs
  | BinOp (op, lhs, rhs) ->
      let level = binop_prec op in
      (* left-associative: the right operand must bind strictly tighter *)
      at level lhs ^ " " ^ binop_str op ^ " " ^ at (level + 1) rhs
  | App (callee, args) ->
      callee_of callee ^ "("
      ^ String.concat ", " (List.map (pretty_expr name_of callee_of) args)
      ^ ")"
  | Builtin (b, args) ->
      Builtin.name b ^ "("
      ^ String.concat ", " (List.map (pretty_expr name_of callee_of) args)
      ^ ")"
  | Subscript (lhs, index) ->
      at 7 lhs ^ "[" ^ pretty_expr name_of callee_of index ^ "]"
  | Field (record, label) -> at 7 record ^ "." ^ label
  | Record fields ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (label, e) -> label ^ ": " ^ pretty_expr name_of callee_of e)
             fields)
      ^ "}"
  | Range (lo, hi) -> at 5 lo ^ ".." ^ at 5 hi
  | MapInit { binder; domain; value } ->
      "{ " ^ name_of binder ^ " in "
      ^ pretty_expr name_of callee_of domain
      ^ " -> "
      ^ pretty_expr name_of callee_of value
      ^ " }"
  | Tuple elems -> (
      match elems with
      | [] -> "()"
      | [ e ] -> "(" ^ pretty_expr name_of callee_of e ^ ",)"
      | es ->
          "("
          ^ String.concat ", " (List.map (pretty_expr name_of callee_of) es)
          ^ ")")
  | Sequence elems ->
      "["
      ^ String.concat ", " (List.map (pretty_expr name_of callee_of) elems)
      ^ "]"
  | SetLit elems ->
      "{"
      ^ String.concat ", " (List.map (pretty_expr name_of callee_of) elems)
      ^ "}"
  | SetComp { binder; domain; pred } ->
      "{ " ^ name_of binder ^ " in "
      ^ pretty_expr name_of callee_of domain
      ^ " : "
      ^ pretty_expr name_of callee_of pred
      ^ " }"
  | IfExpr (cond, then_e, else_e) ->
      "if ("
      ^ pretty_expr name_of callee_of cond
      ^ ") { "
      ^ pretty_expr name_of callee_of then_e
      ^ " } else { "
      ^ pretty_expr name_of callee_of else_e
      ^ " }"
  | Quant { quant; binder; domain; body } ->
      (match quant with Forall -> "forall (" | Exists -> "exists (")
      ^ name_of binder ^ " in "
      ^ pretty_expr name_of callee_of domain
      ^ ") { "
      ^ pretty_expr name_of callee_of body
      ^ " }"

let pretty_accessor name_of callee_of = function
  | AccIndex index -> "[" ^ pretty_expr name_of callee_of index ^ "]"
  | AccField field -> "." ^ field

let pretty_assign_target name_of callee_of = function
  | VarTarget n -> name_of n
  | PathTarget (n, path) ->
      name_of n
      ^ String.concat "" (List.map (pretty_accessor name_of callee_of) path)

let pretty_simple_stmt name_of callee_of (stmt : ('n, 'c) simple_stmt) =
  match stmt.desc with
  | Assign (target, value) ->
      pretty_assign_target name_of callee_of target
      ^ " = "
      ^ pretty_expr name_of callee_of value
  | Call (name, args) ->
      name ^ "("
      ^ String.concat ", " (List.map (pretty_expr name_of callee_of) args)
      ^ ")"
  | Return value -> "return " ^ pretty_expr name_of callee_of value
  | Break -> "break"
  | Continue -> "continue"
  | Await cond -> "await " ^ pretty_expr name_of callee_of cond
  | Assert cond -> "assert " ^ pretty_expr name_of callee_of cond

let rec pretty_step name_of callee_of indent (step : ('n, 'c) step) =
  match step.desc with
  | SimpleStep stmts ->
      indent
      ^ String.concat ", "
          (List.map (pretty_simple_stmt name_of callee_of) stmts)
      ^ ";\n"
  | EmptyStep -> indent ^ ";\n"
  | BlockStep stmt -> pretty_block_stmt name_of callee_of indent stmt
  | VarStep (n, value) ->
      indent ^ "var " ^ name_of n ^ " = "
      ^ pretty_expr name_of callee_of value
      ^ ";\n"
  | WithStep { binder; domain; stmts } ->
      indent ^ "with (" ^ name_of binder ^ " in "
      ^ pretty_expr name_of callee_of domain
      ^ ") { "
      ^ String.concat ", "
          (List.map (pretty_simple_stmt name_of callee_of) stmts)
      ^ "; }\n"

and pretty_block_stmt name_of callee_of indent = function
  | While { cond; body } ->
      indent ^ "while ("
      ^ pretty_expr name_of callee_of cond
      ^ ") {\n"
      ^ pretty_body name_of callee_of (indent ^ "  ") body
      ^ indent ^ "}\n"
  | If { cond; body; else_body } ->
      indent ^ pretty_if name_of callee_of indent cond body else_body
  | Either arms ->
      indent ^ "either {\n"
      ^ String.concat (indent ^ "} or {\n")
          (List.map
             (fun arm -> pretty_body name_of callee_of (indent ^ "  ") arm)
             arms)
      ^ indent ^ "}\n"

(* Printed without the leading indent so an else-if chain can continue on
   the closing brace's line. An else body that is exactly a nested if (the
   shape [else if] parses to) prints back as [else if]. *)
and pretty_if name_of callee_of indent cond body else_body =
  "if ("
  ^ pretty_expr name_of callee_of cond
  ^ ") {\n"
  ^ pretty_body name_of callee_of (indent ^ "  ") body
  ^ indent ^ "}"
  ^
  match else_body with
  | Some [ { desc = BlockStep (If { cond = c; body = b; else_body = eb }); _ } ]
    ->
      " else " ^ pretty_if name_of callee_of indent c b eb
  | Some else_body ->
      " else {\n"
      ^ pretty_body name_of callee_of (indent ^ "  ") else_body
      ^ indent ^ "}\n"
  | None -> "\n"

and pretty_body name_of callee_of indent steps =
  String.concat "" (List.map (pretty_step name_of callee_of indent) steps)

let pretty_item name_of callee_of indent (item : ('n, 'c) item) =
  match item.desc with
  | ConstDef { name; value } ->
      indent ^ "def " ^ name ^ " = "
      ^ pretty_expr name_of callee_of value
      ^ ";\n"
  | PropDef { name; value } ->
      indent ^ "property " ^ name ^ " = "
      ^ pretty_expr name_of callee_of value
      ^ ";\n"
  | FunDef { name; params; body_expr } ->
      indent ^ "def " ^ name ^ "(" ^ String.concat ", " params ^ ") = "
      ^ pretty_expr name_of callee_of body_expr
      ^ ";\n"
  | VarDecl { name; init } -> (
      match init with
      | InitValue value ->
          indent ^ "var " ^ name ^ " = "
          ^ pretty_expr name_of callee_of value
          ^ ";\n"
      | InitIn domain ->
          indent ^ "var " ^ name ^ " in "
          ^ pretty_expr name_of callee_of domain
          ^ ";\n")
  | ProcDef { name; params; body } ->
      indent ^ "procedure " ^ name ^ "("
      ^ String.concat ", " (List.map name_of params)
      ^ ") {\n"
      ^ pretty_body name_of callee_of (indent ^ "  ") body
      ^ indent ^ "}\n"
  | Process { name; proc; fairness; domain } ->
      indent
      ^ (match fairness with
        | Unfair -> "process "
        | WeakFair -> "fair process "
        | StrongFair -> "fair+ process ")
      ^ name ^ "(self in "
      ^ pretty_expr name_of callee_of domain
      ^ ") = " ^ proc ^ ";\n"

let pretty_module_def name_of callee_of (m : ('n, 'c) module_def) =
  "mod " ^ m.mod_name ^ " {\n"
  ^ String.concat "" (List.map (pretty_item name_of callee_of "  ") m.items)
  ^ "}\n"

let print_pretty (prog : Surface_ast.program) =
  String.concat "" (List.map (pretty_module_def Fun.id Fun.id) prog)
