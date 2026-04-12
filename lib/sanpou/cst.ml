(* trivia = whitespace/comments preceding a token *)
type trivia = string [@@deriving show]

let equal_trivia _ _ = true

type loc = { line : int; col : int } [@@deriving show]

let equal_loc _ _ = true

type id = string [@@deriving show, eq]

type binop = Plus | Minus | Mult | Lt | LtEq | GtEq | Eq | Neq | And | Or
[@@deriving show, eq]

type unop = Neg [@@deriving show, eq]

(* A comma-separated list preserving trivia before each comma *)
type 'a comma_list = { items : 'a list; commas : trivia list }
[@@deriving show, eq]

type assign_target =
  | VarTarget of { name_t : trivia; name : id }
  | SubscriptTarget of {
      name_t : trivia;
      name : id;
      lb_t : trivia;
      index : expr;
      rb_t : trivia;
    }

and expr =
  | IntLit of { t : trivia; value : int }
  | BoolLit of { t : trivia; value : bool }
  | Var of { t : trivia; name : id }
  | Self of { t : trivia }
  | UnOp of { op_t : trivia; op : unop; rhs : expr }
  | BinOp of { lhs : expr; op_t : trivia; op : binop; rhs : expr }
  | App of {
      name_t : trivia;
      name : id;
      lp : trivia;
      args : expr comma_list;
      rp : trivia;
    }
  | Subscript of { lhs : expr; lb_t : trivia; index : expr; rb_t : trivia }
  | MapInit of {
      lb : trivia;
      binder_t : trivia;
      binder : id;
      in_t : trivia;
      lo : expr;
      dotdot_t : trivia;
      hi : expr;
      colon_t : trivia;
      value : expr;
      trailing_semi_t : trivia option;
      rb : trivia;
    }
  | Tuple of {
      lp : trivia;
      elems : expr comma_list;
      trailing_comma : trivia option;
      rp : trivia;
    }
  | Sequence of {
      lb : trivia;
      elems : expr comma_list;
      trailing_comma : trivia option;
      rb : trivia;
    }
  | Paren of { lp : trivia; inner : expr; rp : trivia }
[@@deriving show, eq]

type simple_stmt =
  | Assign of { target : assign_target; eq_t : trivia; value : expr }
  | Call of {
      name_t : trivia;
      name : id;
      lp : trivia;
      args : expr comma_list;
      rp : trivia;
    }
  | Return of { t : trivia; value : expr }
  | Break of { t : trivia }
  | Continue of { t : trivia }
  | Await of { t : trivia; cond : expr }
[@@deriving show, eq]

type block_stmt =
  | While of {
      while_t : trivia;
      lp : trivia;
      cond : expr;
      rp : trivia;
      lb : trivia;
      body : body;
      rb : trivia;
    }
  | If of {
      if_t : trivia;
      lp : trivia;
      cond : expr;
      rp : trivia;
      lb : trivia;
      body : body;
      rb : trivia;
      else_branch : (trivia * trivia * body * trivia) option;
    }

and step =
  | SimpleStep of { loc : loc; stmts : simple_stmt comma_list; semi_t : trivia }
  | EmptyStep of { loc : loc; semi_t : trivia }
  | BlockStep of { loc : loc; stmt : block_stmt }
  | VarStep of {
      loc : loc;
      var_t : trivia;
      name_t : trivia;
      name : id;
      eq_t : trivia;
      value : expr;
      semi_t : trivia;
    }

and body = step list [@@deriving show, eq]

type item =
  | ConstDef of {
      def_t : trivia;
      name_t : trivia;
      name : id;
      eq_t : trivia;
      value : expr;
      semi_t : trivia;
    }
  | FunDef of {
      def_t : trivia;
      name_t : trivia;
      name : id;
      lp : trivia;
      params : (trivia * id) comma_list;
      rp : trivia;
      eq_t : trivia;
      body_expr : expr;
      semi_t : trivia;
    }
  | VarDecl of {
      var_t : trivia;
      name_t : trivia;
      name : id;
      eq_t : trivia;
      value : expr;
      semi_t : trivia;
    }
  | ProcDef of {
      fn_t : trivia;
      name_t : trivia;
      name : id;
      lp : trivia;
      params : (trivia * id) comma_list;
      rp : trivia;
      lb : trivia;
      body : body;
      rb : trivia;
    }
  | Process of {
      fair_t : trivia option;
      process_t : trivia;
      name_t : trivia;
      name : id;
      eq_t : trivia;
      proc_t : trivia;
      proc : id;
      in_t : trivia;
      lo : expr;
      dotdot_t : trivia;
      hi : expr;
      semi_t : trivia;
    }
[@@deriving show, eq]

type module_def = {
  mod_t : trivia;
  name_t : trivia;
  mod_name : id;
  lb : trivia;
  items : item list;
  rb : trivia;
}
[@@deriving show, eq]

type program = { modules : module_def list; eof_t : trivia }
[@@deriving show, eq]
