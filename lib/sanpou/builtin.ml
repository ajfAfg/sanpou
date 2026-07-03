(* The fixed set of built-in functions. Built-ins are recognized by name in
   application position at parse time, so they shadow module-level
   definitions of the same name (the precedence rule the checker previously
   applied). Their type signatures live in Typing and their TLA+ translations
   in Emit_tla, both as exhaustive matches on this type. *)

type t = Globally | Finally | Head | Tail | Append | Concat
[@@deriving show, eq]

let of_name = function
  | "globally" -> Some Globally
  | "finally" -> Some Finally
  | "head" -> Some Head
  | "tail" -> Some Tail
  | "append" -> Some Append
  | "concat" -> Some Concat
  | _ -> None

let name = function
  | Globally -> "globally"
  | Finally -> "finally"
  | Head -> "head"
  | Tail -> "tail"
  | Append -> "append"
  | Concat -> "concat"
