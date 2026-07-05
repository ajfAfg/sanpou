open Tla_ast

(* Bullet lists are column-sensitive in TLA+: a nested list's bullets must
   sit strictly to the right of the enclosing bullet. Blocks therefore
   render flush-left relative to themselves — [bullet] prefixes the item's
   first line and pads its continuation lines by the bullet's width, so
   nesting indents naturally — and the operator definition indents the
   whole body once. *)
let bullet prefix s =
  let pad = String.make (String.length prefix) ' ' in
  match String.split_on_char '\n' s with
  | [] -> prefix
  | first :: rest ->
      String.concat "\n"
        ((prefix ^ first) :: List.map (fun line -> pad ^ line) rest)

let indent_lines n s =
  let pad = String.make n ' ' in
  String.split_on_char '\n' s |> List.map (fun line -> pad ^ line)
  |> String.concat "\n"

let rec render_expr = function
  | TInt i -> string_of_int i
  | TBool b -> if b then "TRUE" else "FALSE"
  | TStr s -> "\"" ^ s ^ "\""
  | TId s -> s
  | TBinOp (op, e1, e2) -> render_expr e1 ^ " " ^ op ^ " " ^ render_expr e2
  | TApp (name, args) ->
      name ^ "(" ^ String.concat ", " (List.map render_expr args) ^ ")"
  | TPrimed e -> render_expr e ^ "'"
  | TExcept (f, updates) ->
      let render_selector = function
        | SubSel idx -> "[" ^ render_expr idx ^ "]"
        | FieldSel field -> "." ^ field
      in
      let render_update (path, v) =
        "!"
        ^ String.concat "" (List.map render_selector path)
        ^ " = " ^ render_expr v
      in
      "[" ^ render_expr f ^ " EXCEPT "
      ^ String.concat ", " (List.map render_update updates)
      ^ "]"
  | TSeqLit [] -> "<< >>"
  | TSeqLit es -> "<< " ^ String.concat ", " (List.map render_expr es) ^ " >>"
  | TRecord fields ->
      "["
      ^ String.concat ", "
          (List.map (fun (k, v) -> k ^ " |-> " ^ render_expr v) fields)
      ^ "]"
  | TRange (lo, hi) -> render_expr lo ^ ".." ^ render_expr hi
  | TCup parts -> String.concat " \\cup " (List.map render_expr parts)
  | TNot e -> "~(" ^ render_expr e ^ ")"
  | TConj (Inline, es) -> String.concat " /\\ " (List.map render_expr es)
  | TConj (Block, es) ->
      String.concat "\n"
        (List.map (fun e -> bullet "/\\ " (render_expr e)) es)
  | TDisj (Inline, es) -> String.concat " \\/ " (List.map render_expr es)
  | TDisj (Block, es) ->
      String.concat "\n"
        (List.map (fun e -> bullet "\\/ " (render_expr e)) es)
  (* [bullet] pads a multi-line body so a nested bullet list keeps its
     column alignment after the binder prefix. *)
  | TExists (x, set, body) ->
      bullet
        ("\\E " ^ x ^ " \\in " ^ render_expr set ^ ": ")
        (render_expr body)
  | TForall (x, set, body) ->
      bullet
        ("\\A " ^ x ^ " \\in " ^ render_expr set ^ ": ")
        (render_expr body)
  | TCase cases -> (
      let rendered =
        List.map
          (fun (cond, result) -> render_expr cond ^ " -> " ^ render_expr result)
          cases
      in
      match rendered with
      | [] -> "CASE TRUE -> TRUE"
      | first :: rest ->
          "CASE " ^ first
          ^ String.concat ""
              (List.map
                 (fun c -> "\n                                        [] " ^ c)
                 rest))
  | TUnchanged vars ->
      "UNCHANGED << " ^ String.concat ", " (List.map render_expr vars) ^ " >>"
  | TUnchangedExpr expr -> "UNCHANGED " ^ render_expr expr
  | TFuncMap (x, set, body) ->
      "[" ^ x ^ " \\in " ^ render_expr set ^ " |-> " ^ render_expr body ^ "]"
  | TSubscript (f, idx) -> render_expr f ^ "[" ^ render_expr idx ^ "]"
  | TConcat (e1, e2) -> render_expr e1 ^ " \\o " ^ render_expr e2
  | TDot (e, field) -> render_expr e ^ "." ^ field
  | TIn (e, set) -> render_expr e ^ " \\in " ^ render_expr set
  | TBoxAction (action, v) -> "[][" ^ render_expr action ^ "]_" ^ render_expr v
  | THead e -> "Head(" ^ render_expr e ^ ")"
  | TTail e -> "Tail(" ^ render_expr e ^ ")"
  | TParens e -> "(" ^ render_expr e ^ ")"
  | TIf (cond, then_e, else_e) ->
      "IF " ^ render_expr cond ^ " THEN " ^ render_expr then_e ^ " ELSE "
      ^ render_expr else_e
  | TGlobally e -> "[](" ^ render_expr e ^ ")"
  | TFinally e -> "<>(" ^ render_expr e ^ ")"

let render_decl = function
  | DExtends modules -> "EXTENDS " ^ String.concat ", " modules
  | DConstants names -> "CONSTANT " ^ String.concat ", " names
  | DOpDef (name, params, body) -> (
      let lhs =
        if params = [] then name
        else name ^ "(" ^ String.concat ", " params ^ ")"
      in
      match body with
      | TConj (Block, _) | TDisj (Block, _) ->
          lhs ^ " ==\n" ^ indent_lines 4 (render_expr body)
      | _ -> lhs ^ " == " ^ render_expr body)
  | DVariables vars -> "VARIABLES " ^ String.concat ", " vars
  | DSeparator -> ""

let render (m : tla_module) : string =
  let buf = Buffer.create 4096 in
  let addln s =
    Buffer.add_string buf s;
    Buffer.add_char buf '\n'
  in
  addln ("---- MODULE " ^ m.name ^ " ----");
  List.iter (fun d -> addln (render_decl d)) m.body;
  addln "====";
  Buffer.contents buf
