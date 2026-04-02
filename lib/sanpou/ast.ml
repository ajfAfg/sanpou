type id = string [@@deriving show, eq]
type binop = Plus | Minus | Mult | Lt | Eq [@@deriving show, eq]

type expr =
  | IntLit of int
  | BoolLit of bool
  | Var of id
  | BinOp of binop * expr * expr
  | App of id * expr list
  | Tuple of expr list
[@@deriving show, eq]

type stmt =
  | Assign of id * expr
  | Call of id * expr list
  | Return of expr
  | Break
  | While of expr * body
  | If of expr * body
  | Await of expr
  | Let of id * expr
[@@deriving show, eq]

and step = stmt list [@@deriving show, eq]
and body = step list [@@deriving show, eq]

type item =
  | ConstDef of id * expr
  | FunDef of id * id list * expr
  | VarDecl of id * expr
  | ProcDef of id * id list * body
  | Process of id * id * expr * expr
[@@deriving show, eq]

type module_def = { mod_name : id; items : item list } [@@deriving show, eq]
type program = module_def list [@@deriving show, eq]
