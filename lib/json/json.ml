type t =
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

(* Parsers take a position and return (value, next position) *)

let rec skip_ws s pos =
  if
    pos < String.length s
    && (s.[pos] = ' ' || s.[pos] = '\t' || s.[pos] = '\n' || s.[pos] = '\r')
  then skip_ws s (pos + 1)
  else pos

let parse_string_value s pos =
  (* pos should point to opening '"' *)
  let len = String.length s in
  let buf = Buffer.create 32 in
  let rec go p =
    if p >= len || s.[p] = '"' then (Buffer.contents buf, p + 1)
      (* p + 1 skips the closing quote *)
    else if s.[p] = '\\' && p + 1 < len then (
      (match s.[p + 1] with
      | '"' -> Buffer.add_char buf '"'
      | '\\' -> Buffer.add_char buf '\\'
      | 'n' -> Buffer.add_char buf '\n'
      | 't' -> Buffer.add_char buf '\t'
      | 'r' -> Buffer.add_char buf '\r'
      | c ->
          Buffer.add_char buf '\\';
          Buffer.add_char buf c);
      go (p + 2))
    else (
      Buffer.add_char buf s.[p];
      go (p + 1))
  in
  go (pos + 1)

let parse_int_value s pos =
  let len = String.length s in
  let rec digits_end p =
    if p < len && s.[p] >= '0' && s.[p] <= '9' then digits_end (p + 1) else p
  in
  let p = digits_end pos in
  (int_of_string (String.sub s pos (p - pos)), p)

let rec parse_value s pos =
  let p = skip_ws s pos in
  let len = String.length s in
  if p >= len then failwith "Json.parse: unexpected end of input"
  else
    match s.[p] with
    | 't' when p + 3 < len && String.sub s p 4 = "true" -> (Bool true, p + 4)
    | 'f' when p + 4 < len && String.sub s p 5 = "false" -> (Bool false, p + 5)
    | '"' ->
        let v, next = parse_string_value s p in
        (String v, next)
    | '0' .. '9' ->
        let v, next = parse_int_value s p in
        (Int v, next)
    | '[' -> parse_array s (p + 1)
    | '{' -> parse_object s (p + 1)
    | c ->
        failwith (Printf.sprintf "Json.parse: unexpected char '%c' at %d" c p)

and parse_array s pos =
  let len = String.length s in
  let rec items acc p =
    if p < len && s.[p] <> ']' then
      let v, next = parse_value s p in
      let next = skip_ws s next in
      let p =
        if next < len && s.[next] = ',' then skip_ws s (next + 1) else next
      in
      items (v :: acc) p
    else (List.rev acc, p)
  in
  let elems, p = items [] (skip_ws s pos) in
  (Array elems, p + 1)

and parse_object s pos =
  let len = String.length s in
  let rec fields acc p =
    if p < len && s.[p] <> '}' then
      let key, next = parse_string_value s p in
      let next = skip_ws s next in
      let next = if next < len && s.[next] = ':' then next + 1 else next in
      let v, next = parse_value s next in
      let next = skip_ws s next in
      let p =
        if next < len && s.[next] = ',' then skip_ws s (next + 1) else next
      in
      fields ((key, v) :: acc) p
    else (List.rev acc, p)
  in
  let fs, p = fields [] (skip_ws s pos) in
  (Object fs, p + 1)

let parse s =
  let s = String.trim s in
  if String.length s = 0 then failwith "Json.parse: empty input"
  else
    let v, _ = parse_value s 0 in
    v

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
