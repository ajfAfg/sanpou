open Tla_ast

let indent n s =
  let prefix = String.make n ' ' in
  prefix ^ s

let rec render_expr = function
  | TInt i -> string_of_int i
  | TBool b -> if b then "TRUE" else "FALSE"
  | TStr s -> "\"" ^ s ^ "\""
  | TId s -> s
  | TBinOp (op, e1, e2) -> render_expr e1 ^ " " ^ op ^ " " ^ render_expr e2
  | TApp (name, args) ->
      name ^ "(" ^ String.concat ", " (List.map render_expr args) ^ ")"
  | TPrimed e -> render_expr e ^ "'"
  | TExcept (f, idx, v) ->
      "[" ^ render_expr f ^ " EXCEPT ![" ^ render_expr idx ^ "] = "
      ^ render_expr v ^ "]"
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
        (List.map (fun e -> indent 4 ("/\\ " ^ render_expr e)) es)
  | TDisj (Inline, es) -> String.concat " \\/ " (List.map render_expr es)
  | TDisj (Block, es) ->
      String.concat "\n"
        (List.map (fun e -> indent 4 ("\\/ " ^ render_expr e)) es)
  | TExists (x, set, body) ->
      "\\E " ^ x ^ " \\in " ^ render_expr set ^ ": " ^ render_expr body
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
  | DOpDef (name, params, body) -> (
      let lhs =
        if params = [] then name
        else name ^ "(" ^ String.concat ", " params ^ ")"
      in
      match body with
      | TConj (Block, _) -> lhs ^ " ==\n" ^ render_expr body
      | TDisj (Block, _) -> lhs ^ " ==\n" ^ render_expr body
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
