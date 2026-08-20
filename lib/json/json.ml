(* The type lives in [Json_ast] so the generated parser can produce it;
   re-exported here so users write [Json.t] and [Json.String]. *)
type t = Json_ast.t =
  | String of string
  | Int of int
  | Bool of bool
  | Array of t list
  | Object of (string * t) list

(* ===== Serialization ===== *)

let escape_string s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\r' -> Buffer.add_string buf "\\r"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let rec to_string_indent indent = function
  | String s -> "\"" ^ escape_string s ^ "\""
  | Int i -> string_of_int i
  | Bool b -> if b then "true" else "false"
  | Object fields ->
      let parts =
        List.map
          (fun (k, v) ->
            "\"" ^ escape_string k ^ "\": " ^ to_string_indent indent v)
          fields
      in
      "{ " ^ String.concat ", " parts ^ " }"
  | Array items ->
      let prefix = String.make indent ' ' in
      let parts =
        List.map (fun v -> prefix ^ to_string_indent indent v) items
      in
      "[\n" ^ String.concat ",\n" parts ^ "\n]\n"

let to_string v = to_string_indent 4 v

(* ===== Deserialization ===== *)

(* Lexical and syntax errors become [Failure] so callers can report the
   message without knowing about the generated lexer/parser. *)
let parse s =
  let lexbuf = Lexing.from_string s in
  try Json_parser.json Json_lexer.token lexbuf with
  | Json_lexer.Error msg -> failwith ("Json.parse: " ^ msg)
  | Json_parser.Error ->
      failwith
        (Printf.sprintf "Json.parse: syntax error at offset %d"
           (Lexing.lexeme_start lexbuf))

(* ===== Accessors ===== *)

let to_object = function
  | Object fs -> fs
  | _ -> failwith "Json: expected object"

let to_array = function Array xs -> xs | _ -> failwith "Json: expected array"

let to_string_value = function
  | String s -> s
  | _ -> failwith "Json: expected string"

let to_int = function Int i -> i | _ -> failwith "Json: expected int"
let to_bool = function Bool b -> b | _ -> failwith "Json: expected bool"

let field key obj =
  match List.assoc_opt key (to_object obj) with
  | Some v -> v
  | None -> failwith ("Json: missing field " ^ key)

let field_opt key obj = List.assoc_opt key (to_object obj)
