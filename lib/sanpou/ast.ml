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

(* The tree is parameterized by the type ['n] of variable names: the parser
   produces [id program] (plain source names) and later passes may
   instantiate ['n] with richer name representations. Names that are never
   rebound per scope — module-level definitions, procedure/function names —
   stay [id] at every stage. *)
type 'n expr = 'n expr_desc node

and 'n expr_desc =
  | IntLit of int
  | BoolLit of bool
  | Var of 'n
  | Self
  | UnOp of unop * 'n expr
  | BinOp of binop * 'n expr * 'n expr
  | App of id * 'n expr list
  | Builtin of Builtin.t * 'n expr list
  | Subscript of 'n expr * 'n expr
  | MapInit of { binder : 'n; lo : 'n expr; hi : 'n expr; value : 'n expr }
  | Tuple of 'n expr list
  | Sequence of 'n expr list
[@@deriving show, eq]

type 'n assign_target = VarTarget of 'n | SubscriptTarget of 'n * 'n expr
[@@deriving show, eq]

type 'n simple_stmt = 'n simple_stmt_desc node

and 'n simple_stmt_desc =
  | Assign of 'n assign_target * 'n expr
  | Call of id * 'n expr list
  | Return of 'n expr
  | Break
  | Continue
  | Await of 'n expr
[@@deriving show, eq]

type 'n step = 'n step_desc node

and 'n step_desc =
  | SimpleStep of 'n simple_stmt list
  | EmptyStep
  | BlockStep of 'n block_stmt
  | VarStep of 'n * 'n expr

and 'n block_stmt =
  | While of { cond : 'n expr; body : 'n body }
  | If of { cond : 'n expr; body : 'n body; else_body : 'n body option }

and 'n body = 'n step list [@@deriving show, eq]

type 'n item = 'n item_desc node

and 'n item_desc =
  | ConstDef of { name : id; value : 'n expr }
  | FunDef of { name : id; params : id list; body_expr : 'n expr }
  | VarDecl of { name : id; value : 'n expr }
  | ProcDef of { name : id; params : 'n list; body : 'n body }
  | Process of { name : id; proc : id; fair : bool; lo : 'n expr; hi : 'n expr }
[@@deriving show, eq]

type 'n module_def = { mod_name : id; items : 'n item list; mod_loc : loc }
[@@deriving show, eq]

type 'n program = 'n module_def list [@@deriving show, eq]

(* Structural map over the name type; the shape of the tree is unchanged. *)
let rec map_expr (f : 'n -> 'm) (e : 'n expr) : 'm expr =
  let desc =
    match e.desc with
    | IntLit v -> IntLit v
    | BoolLit b -> BoolLit b
    | Var n -> Var (f n)
    | Self -> Self
    | UnOp (op, rhs) -> UnOp (op, map_expr f rhs)
    | BinOp (op, lhs, rhs) -> BinOp (op, map_expr f lhs, map_expr f rhs)
    | App (name, args) -> App (name, List.map (map_expr f) args)
    | Builtin (b, args) -> Builtin (b, List.map (map_expr f) args)
    | Subscript (lhs, index) -> Subscript (map_expr f lhs, map_expr f index)
    | MapInit { binder; lo; hi; value } ->
        MapInit
          {
            binder = f binder;
            lo = map_expr f lo;
            hi = map_expr f hi;
            value = map_expr f value;
          }
    | Tuple elems -> Tuple (List.map (map_expr f) elems)
    | Sequence elems -> Sequence (List.map (map_expr f) elems)
  in
  { desc; loc = e.loc }
