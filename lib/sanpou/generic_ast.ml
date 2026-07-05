type loc = { line : int; col : int } [@@deriving show]

(* Locations are metadata, not semantics: structural equality ignores them so
   tests can compare trees built with a dummy location against parsed ones. *)
let equal_loc _ _ = true

(* Every syntax node is a semantic description paired with its source
   location. Semantics live in the *_desc variants; [loc] never influences
   the meaning of the tree. *)
type 'a node = { desc : 'a; loc : loc } [@@deriving show, eq]
type id = string [@@deriving show, eq]

type binop =
  | Plus
  | Minus
  | Mult
  | Div
  | Mod
  | Lt
  | Gt
  | LtEq
  | GtEq
  | Eq
  | Neq
  | And
  | Or
[@@deriving show, eq]

type unop = Neg | Not [@@deriving show, eq]
type quantifier = Forall | Exists [@@deriving show, eq]

(* Process fairness: none, weak (WF_vars), or strong (SF_vars). *)
type fairness = Unfair | WeakFair | StrongFair [@@deriving show, eq]

(* The tree is parameterized by the type ['n] of variable names and the type
   ['c] of applied callees: the parser produces [Surface_ast.program] (plain
   source names, undistinguished callees) and later passes may instantiate
   them with richer representations — ['n] with per-scope renamed idents,
   ['c] with a resolved function/procedure distinction. Names that are never
   rebound per scope — module-level definitions, procedure/function names in
   defining or statement position — stay [id] at every stage. *)
type ('n, 'c) expr = ('n, 'c) expr_desc node

and ('n, 'c) expr_desc =
  | IntLit of int
  | BoolLit of bool
  | Var of 'n
  | Self
  | UnOp of unop * ('n, 'c) expr
  | BinOp of binop * ('n, 'c) expr * ('n, 'c) expr
  | App of 'c * ('n, 'c) expr list
  | Builtin of Builtin.t * ('n, 'c) expr list
  | Subscript of ('n, 'c) expr * ('n, 'c) expr
  | MapInit of {
      binder : 'n;
      lo : ('n, 'c) expr;
      hi : ('n, 'c) expr;
      value : ('n, 'c) expr;
    }
  | Tuple of ('n, 'c) expr list
  | Sequence of ('n, 'c) expr list
  | IfExpr of ('n, 'c) expr * ('n, 'c) expr * ('n, 'c) expr
  | Quant of {
      quant : quantifier;
      binder : 'n;
      lo : ('n, 'c) expr;
      hi : ('n, 'c) expr;
      body : ('n, 'c) expr;
    }
[@@deriving show, eq]

type ('n, 'c) assign_target =
  | VarTarget of 'n
  | SubscriptTarget of 'n * ('n, 'c) expr
[@@deriving show, eq]

type ('n, 'c) simple_stmt = ('n, 'c) simple_stmt_desc node

and ('n, 'c) simple_stmt_desc =
  | Assign of ('n, 'c) assign_target * ('n, 'c) expr
  | Call of id * ('n, 'c) expr list
  | Return of ('n, 'c) expr
  | Break
  | Continue
  | Await of ('n, 'c) expr
[@@deriving show, eq]

type ('n, 'c) step = ('n, 'c) step_desc node

and ('n, 'c) step_desc =
  | SimpleStep of ('n, 'c) simple_stmt list
  | EmptyStep
  | BlockStep of ('n, 'c) block_stmt
  | VarStep of 'n * ('n, 'c) expr
  | WithStep of {
      binder : 'n;
      lo : ('n, 'c) expr;
      hi : ('n, 'c) expr;
      stmts : ('n, 'c) simple_stmt list;
    }
      (* one atomic step under a non-deterministically chosen binder;
         restricting the body to simple statements keeps the binder's
         scope within a single action *)

and ('n, 'c) block_stmt =
  | While of { cond : ('n, 'c) expr; body : ('n, 'c) body }
  | If of {
      cond : ('n, 'c) expr;
      body : ('n, 'c) body;
      else_body : ('n, 'c) body option;
    }
  | Either of ('n, 'c) body list (* non-deterministic arms; at least two *)

and ('n, 'c) body = ('n, 'c) step list [@@deriving show, eq]

(* How a module-level variable starts out: a single value, or any value
   drawn from a range — the model checker then explores every choice. *)
type ('n, 'c) var_init =
  | InitValue of ('n, 'c) expr
  | InitRange of ('n, 'c) expr * ('n, 'c) expr
[@@deriving show, eq]

type ('n, 'c) item = ('n, 'c) item_desc node

and ('n, 'c) item_desc =
  | ConstDef of { name : id; value : ('n, 'c) expr }
  | FunDef of { name : id; params : id list; body_expr : ('n, 'c) expr }
  | VarDecl of { name : id; init : ('n, 'c) var_init }
  | ProcDef of { name : id; params : 'n list; body : ('n, 'c) body }
  | Process of {
      name : id;
      proc : id;
      fairness : fairness;
      lo : ('n, 'c) expr;
      hi : ('n, 'c) expr;
    }
[@@deriving show, eq]

type ('n, 'c) module_def = {
  mod_name : id;
  items : ('n, 'c) item list;
  mod_loc : loc;
}
[@@deriving show, eq]

type ('n, 'c) program = ('n, 'c) module_def list [@@deriving show, eq]

(* Structural map over the name and callee types; the shape of the tree is
   unchanged. *)
let rec map_expr (f : 'n -> 'm) (g : 'c -> 'd) (e : ('n, 'c) expr) :
    ('m, 'd) expr =
  let desc =
    match e.desc with
    | IntLit v -> IntLit v
    | BoolLit b -> BoolLit b
    | Var n -> Var (f n)
    | Self -> Self
    | UnOp (op, rhs) -> UnOp (op, map_expr f g rhs)
    | BinOp (op, lhs, rhs) -> BinOp (op, map_expr f g lhs, map_expr f g rhs)
    | App (callee, args) -> App (g callee, List.map (map_expr f g) args)
    | Builtin (b, args) -> Builtin (b, List.map (map_expr f g) args)
    | Subscript (lhs, index) -> Subscript (map_expr f g lhs, map_expr f g index)
    | MapInit { binder; lo; hi; value } ->
        MapInit
          {
            binder = f binder;
            lo = map_expr f g lo;
            hi = map_expr f g hi;
            value = map_expr f g value;
          }
    | Tuple elems -> Tuple (List.map (map_expr f g) elems)
    | Sequence elems -> Sequence (List.map (map_expr f g) elems)
    | IfExpr (cond, then_e, else_e) ->
        IfExpr (map_expr f g cond, map_expr f g then_e, map_expr f g else_e)
    | Quant { quant; binder; lo; hi; body } ->
        Quant
          {
            quant;
            binder = f binder;
            lo = map_expr f g lo;
            hi = map_expr f g hi;
            body = map_expr f g body;
          }
  in
  { desc; loc = e.loc }

let map_var_init (f : 'n -> 'm) (g : 'c -> 'd) :
    ('n, 'c) var_init -> ('m, 'd) var_init = function
  | InitValue e -> InitValue (map_expr f g e)
  | InitRange (lo, hi) -> InitRange (map_expr f g lo, map_expr f g hi)
