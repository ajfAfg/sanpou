type loc = { line : int; col : int } [@@deriving show]

(* Locations are metadata, not semantics: structural equality ignores them so
   tests can compare trees built with a dummy location against parsed ones. *)
let equal_loc _ _ = true

(* Every syntax node is a semantic description paired with its source
   location. Semantics live in the *_desc variants; [loc] never influences
   the meaning of the tree. *)
type 'a node = { desc : 'a; loc : loc } [@@deriving show, eq]

type id = string [@@deriving show, eq]

type binop = Plus | Minus | Mult | Lt | LtEq | GtEq | Eq | Neq | And | Or
[@@deriving show, eq]

type unop = Neg [@@deriving show, eq]

type expr = expr_desc node

and expr_desc =
  | IntLit of int
  | BoolLit of bool
  | Var of id
  | Self
  | UnOp of unop * expr
  | BinOp of binop * expr * expr
  | App of id * expr list
  | Builtin of Builtin.t * expr list
  | Subscript of expr * expr
  | MapInit of { binder : id; lo : expr; hi : expr; value : expr }
  | Tuple of expr list
  | Sequence of expr list
[@@deriving show, eq]

type assign_target = VarTarget of id | SubscriptTarget of id * expr
[@@deriving show, eq]

type simple_stmt = simple_stmt_desc node

and simple_stmt_desc =
  | Assign of assign_target * expr
  | Call of id * expr list
  | Return of expr
  | Break
  | Continue
  | Await of expr
[@@deriving show, eq]

type step = step_desc node

and step_desc =
  | SimpleStep of simple_stmt list
  | EmptyStep
  | BlockStep of block_stmt
  | VarStep of id * expr

and block_stmt =
  | While of { cond : expr; body : body }
  | If of { cond : expr; body : body; else_body : body option }

and body = step list [@@deriving show, eq]

type item = item_desc node

and item_desc =
  | ConstDef of { name : id; value : expr }
  | FunDef of { name : id; params : id list; body_expr : expr }
  | VarDecl of { name : id; value : expr }
  | ProcDef of { name : id; params : id list; body : body }
  | Process of { name : id; proc : id; fair : bool; lo : expr; hi : expr }
[@@deriving show, eq]

type module_def = { mod_name : id; items : item list; mod_loc : loc }
[@@deriving show, eq]

type program = module_def list [@@deriving show, eq]
