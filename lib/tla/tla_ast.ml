type layout = Inline | Block

(* A selector step in an EXCEPT update path: ![idx] or !.field *)
type except_selector = SubSel of tla_expr | FieldSel of string

and tla_expr =
  | TInt of int
  | TBool of bool
  | TStr of string
  | TId of string
  | TBinOp of string * tla_expr * tla_expr
  | TApp of string * tla_expr list
  | TPrimed of tla_expr
  | TExcept of tla_expr * (except_selector list * tla_expr) list
    (* [f EXCEPT !<path1> = v1, !<path2> = v2, ...] *)
  | TSeqLit of tla_expr list
  | TRecord of (string * tla_expr) list
  | TRange of tla_expr * tla_expr
  | TSet of tla_expr list
  | TSetFilter of string * tla_expr * tla_expr
    (* {x \in S : p} *)
  | TCup of tla_expr list
  | TNot of tla_expr
  | TConj of layout * tla_expr list
  | TDisj of layout * tla_expr list
  | TExists of string * tla_expr * tla_expr
  | TForall of string * tla_expr * tla_expr
  | TCase of (tla_expr * tla_expr) list
  | TUnchanged of tla_expr list
  | TUnchangedExpr of tla_expr
  | TFuncMap of string * tla_expr * tla_expr
  | TSubscript of tla_expr * tla_expr
  | TConcat of tla_expr * tla_expr
  | TDot of tla_expr * string
  | TIn of tla_expr * tla_expr
  | TBoxAction of tla_expr * tla_expr
  | THead of tla_expr
  | TTail of tla_expr
  | TParens of tla_expr
  | TIf of tla_expr * tla_expr * tla_expr
  | TGlobally of tla_expr
  | TFinally of tla_expr

type tla_decl =
  | DExtends of string list
  | DConstants of string list
  | DOpDef of string * string list * tla_expr
  | DVariables of string list
  | DSeparator

type tla_module = { name : string; body : tla_decl list }
