open Cst

(* ===== Exact source reconstruction (trivia-based) ===== *)

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

let unop_str = function Neg -> "-"

let print_comma_list f (cl : _ comma_list) =
  match cl.items with
  | [] -> ""
  | [ x ] -> f x
  | first :: rest ->
      let buf = Buffer.create 64 in
      Buffer.add_string buf (f first);
      List.iter2
        (fun comma_t item ->
          Buffer.add_string buf comma_t;
          Buffer.add_char buf ',';
          Buffer.add_string buf (f item))
        cl.commas rest;
      Buffer.contents buf

let rec print_expr = function
  | IntLit { t; value } -> t ^ string_of_int value
  | BoolLit { t; value } -> t ^ if value then "true" else "false"
  | Var { t; name } -> t ^ name
  | UnOp { op_t; op; rhs } -> op_t ^ unop_str op ^ print_expr rhs
  | BinOp { lhs; op_t; op; rhs } ->
      print_expr lhs ^ op_t ^ binop_str op ^ print_expr rhs
  | App { name_t; name; lp; args; rp } ->
      name_t ^ name ^ lp ^ "(" ^ print_comma_list print_expr args ^ rp ^ ")"
  | Tuple { lp; elems; trailing_comma; rp } ->
      lp ^ "("
      ^ print_comma_list print_expr elems
      ^ (match trailing_comma with Some t -> t ^ "," | None -> "")
      ^ rp ^ ")"
  | Sequence { lb; elems; trailing_comma; rb } ->
      lb ^ "["
      ^ print_comma_list print_expr elems
      ^ (match trailing_comma with Some t -> t ^ "," | None -> "")
      ^ rb ^ "]"
  | Paren { lp; inner; rp } -> lp ^ "(" ^ print_expr inner ^ rp ^ ")"

let print_simple_stmt = function
  | Assign { name_t; name; eq_t; value } ->
      name_t ^ name ^ eq_t ^ "=" ^ print_expr value
  | Call { name_t; name; lp; args; rp } ->
      name_t ^ name ^ lp ^ "(" ^ print_comma_list print_expr args ^ rp ^ ")"
  | Return { t; value } -> t ^ "return" ^ print_expr value
  | Break { t } -> t ^ "break"
  | Await { t; cond } -> t ^ "await" ^ print_expr cond

let rec print_step = function
  | SimpleStep { stmts; semi_t; _ } ->
      print_comma_list print_simple_stmt stmts ^ semi_t ^ ";"
  | EmptyStep { semi_t; _ } -> semi_t ^ ";"
  | BlockStep { stmt; _ } -> print_block_stmt stmt
  | LetStep { let_t; name_t; name; eq_t; value; semi_t; _ } ->
      let_t ^ "let" ^ name_t ^ name ^ eq_t ^ "=" ^ print_expr value ^ semi_t
      ^ ";"

and print_block_stmt = function
  | While { while_t; lp; cond; rp; lb; body; rb } ->
      while_t ^ "while" ^ lp ^ "(" ^ print_expr cond ^ rp ^ ")" ^ lb ^ "{"
      ^ print_body body ^ rb ^ "}"
  | If { if_t; lp; cond; rp; lb; body; rb } ->
      if_t ^ "if" ^ lp ^ "(" ^ print_expr cond ^ rp ^ ")" ^ lb ^ "{"
      ^ print_body body ^ rb ^ "}"

and print_body steps = String.concat "" (List.map print_step steps)

let print_param (t, name) = t ^ name

let print_item = function
  | ConstDef { def_t; name_t; name; eq_t; value; semi_t } ->
      def_t ^ "def" ^ name_t ^ name ^ eq_t ^ "=" ^ print_expr value ^ semi_t
      ^ ";"
  | FunDef { def_t; name_t; name; lp; params; rp; eq_t; body_expr; semi_t } ->
      def_t ^ "def" ^ name_t ^ name ^ lp ^ "("
      ^ print_comma_list print_param params
      ^ rp ^ ")" ^ eq_t ^ "=" ^ print_expr body_expr ^ semi_t ^ ";"
  | VarDecl { let_t; name_t; name; eq_t; value; semi_t } ->
      let_t ^ "let" ^ name_t ^ name ^ eq_t ^ "=" ^ print_expr value ^ semi_t
      ^ ";"
  | ProcDef { fn_t; name_t; name; lp; params; rp; lb; body; rb } ->
      fn_t ^ "fn" ^ name_t ^ name ^ lp ^ "("
      ^ print_comma_list print_param params
      ^ rp ^ ")" ^ lb ^ "{" ^ print_body body ^ rb ^ "}"
  | Process
      {
        fair_t;
        process_t;
        name_t;
        name;
        eq_t;
        proc_t;
        proc;
        in_t;
        lo;
        dotdot_t;
        hi;
        semi_t;
      } ->
      (match fair_t with Some fair_t -> fair_t ^ "fair" | None -> "")
      ^ process_t ^ "process" ^ name_t ^ name ^ eq_t ^ "=" ^ proc_t ^ proc
      ^ in_t ^ "in" ^ print_expr lo ^ dotdot_t ^ ".." ^ print_expr hi ^ semi_t
      ^ ";"

let print_module_def m =
  m.mod_t ^ "mod" ^ m.name_t ^ m.mod_name ^ m.lb ^ "{"
  ^ String.concat "" (List.map print_item m.items)
  ^ m.rb ^ "}"

let print prog =
  String.concat "" (List.map print_module_def prog.modules) ^ prog.eof_t

(* ===== Pretty printer (ignores trivia, canonical formatting) ===== *)

let pretty_binop = function
  | Plus -> " + "
  | Minus -> " - "
  | Mult -> " * "
  | Lt -> " < "
  | LtEq -> " <= "
  | GtEq -> " >= "
  | Eq -> " == "
  | Neq -> " != "
  | And -> " && "

let rec pretty_expr = function
  | IntLit { value; _ } -> string_of_int value
  | BoolLit { value; _ } -> if value then "true" else "false"
  | Var { name; _ } -> name
  | UnOp { op; rhs; _ } -> unop_str op ^ pretty_expr rhs
  | BinOp { lhs; op; rhs; _ } ->
      pretty_expr lhs ^ pretty_binop op ^ pretty_expr rhs
  | App { name; args; _ } ->
      name ^ "(" ^ String.concat ", " (List.map pretty_expr args.items) ^ ")"
  | Tuple { elems; _ } -> (
      match elems.items with
      | [] -> "()"
      | [ e ] -> "(" ^ pretty_expr e ^ ",)"
      | es -> "(" ^ String.concat ", " (List.map pretty_expr es) ^ ")")
  | Sequence { elems; _ } ->
      "[" ^ String.concat ", " (List.map pretty_expr elems.items) ^ "]"
  | Paren { inner; _ } -> "(" ^ pretty_expr inner ^ ")"

let pretty_simple_stmt = function
  | Assign { name; value; _ } -> name ^ " = " ^ pretty_expr value
  | Call { name; args; _ } ->
      name ^ "(" ^ String.concat ", " (List.map pretty_expr args.items) ^ ")"
  | Return { value; _ } -> "return " ^ pretty_expr value
  | Break _ -> "break"
  | Await { cond; _ } -> "await " ^ pretty_expr cond

let rec pretty_step indent = function
  | SimpleStep { stmts; _ } ->
      indent
      ^ String.concat ", " (List.map pretty_simple_stmt stmts.items)
      ^ ";\n"
  | EmptyStep _ -> indent ^ ";\n"
  | BlockStep { stmt; _ } -> pretty_block_stmt indent stmt
  | LetStep { name; value; _ } ->
      indent ^ "let " ^ name ^ " = " ^ pretty_expr value ^ ";\n"

and pretty_block_stmt indent = function
  | While { cond; body; _ } ->
      indent ^ "while (" ^ pretty_expr cond ^ ") {\n"
      ^ pretty_body (indent ^ "  ") body
      ^ indent ^ "}\n"
  | If { cond; body; _ } ->
      indent ^ "if (" ^ pretty_expr cond ^ ") {\n"
      ^ pretty_body (indent ^ "  ") body
      ^ indent ^ "}\n"

and pretty_body indent steps =
  String.concat "" (List.map (pretty_step indent) steps)

let pretty_item indent = function
  | ConstDef { name; value; _ } ->
      indent ^ "def " ^ name ^ " = " ^ pretty_expr value ^ ";\n"
  | FunDef { name; params; body_expr; _ } ->
      indent ^ "def " ^ name ^ "("
      ^ String.concat ", " (List.map snd params.items)
      ^ ") = " ^ pretty_expr body_expr ^ ";\n"
  | VarDecl { name; value; _ } ->
      indent ^ "let " ^ name ^ " = " ^ pretty_expr value ^ ";\n"
  | ProcDef { name; params; body; _ } ->
      indent ^ "fn " ^ name ^ "("
      ^ String.concat ", " (List.map snd params.items)
      ^ ") {\n"
      ^ pretty_body (indent ^ "  ") body
      ^ indent ^ "}\n"
  | Process { fair_t; name; proc; lo; hi; _ } ->
      indent
      ^ (match fair_t with Some _ -> "fair process " | None -> "process ")
      ^ name ^ " = " ^ proc ^ " in " ^ pretty_expr lo ^ ".." ^ pretty_expr hi
      ^ ";\n"

let pretty_module_def m =
  "mod " ^ m.mod_name ^ " {\n"
  ^ String.concat "" (List.map (pretty_item "  ") m.items)
  ^ "}\n"

let print_pretty prog =
  String.concat "" (List.map pretty_module_def prog.modules)
