type layout = Inline | Block

type tla_expr =
  | TInt of int
  | TBool of bool
  | TStr of string
  | TId of string
  | TBinOp of string * tla_expr * tla_expr
  | TApp of string * tla_expr list
  | TPrimed of tla_expr
  | TExcept of tla_expr * tla_expr * tla_expr
  | TSeqLit of tla_expr list
  | TRecord of (string * tla_expr) list
  | TRange of tla_expr * tla_expr
  | TCup of tla_expr list
  | TNot of tla_expr
  | TConj of layout * tla_expr list
  | TDisj of layout * tla_expr list
  | TExists of string * tla_expr * tla_expr
  | TCase of (tla_expr * tla_expr) list
  | TUnchanged of tla_expr list
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

type tla_decl =
  | DExtends of string list
  | DOpDef of string * string list * tla_expr
  | DVariables of string list
  | DSeparator

type tla_module = { name : string; body : tla_decl list }
