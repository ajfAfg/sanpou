(* The fixed set of built-in functions. Built-ins are recognized by name in
   application position at parse time, so they shadow module-level
   definitions of the same name (the precedence rule the checker previously
   applied). Their type signatures live in Typing and their TLA+ translations
   in Emit_tla, both as exhaustive matches on this type. *)

type t =
  | Globally
  | Finally
  | Head
  | Tail
  | Append
  | Concat
  | Len
  | Union
  | Intersection
  | Difference
  | Cardinality
  | Subseteq
[@@deriving show, eq]

let of_name = function
  | "globally" -> Some Globally
  | "finally" -> Some Finally
  | "head" -> Some Head
  | "tail" -> Some Tail
  | "append" -> Some Append
  | "concat" -> Some Concat
  | "len" -> Some Len
  | "union" -> Some Union
  | "intersection" -> Some Intersection
  | "difference" -> Some Difference
  | "cardinality" -> Some Cardinality
  | "subseteq" -> Some Subseteq
  | _ -> None

let name = function
  | Globally -> "globally"
  | Finally -> "finally"
  | Head -> "head"
  | Tail -> "tail"
  | Append -> "append"
  | Concat -> "concat"
  | Len -> "len"
  | Union -> "union"
  | Intersection -> "intersection"
  | Difference -> "difference"
  | Cardinality -> "cardinality"
  | Subseteq -> "subseteq"
