(* The JSON value tree, in its own module so the generated parser can
   produce it without depending on [Json] (which itself depends on the
   parser). [Json] re-exports the type, so users never reference this
   module directly. *)

type t =
  | String of string
  | Int of int
  | Bool of bool
  | Array of t list
  | Object of (string * t) list
