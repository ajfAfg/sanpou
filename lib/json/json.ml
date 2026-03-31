type t =
  | String of string
  | Int of int
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

let skip_ws s pos =
  let len = String.length s in
  let p = ref pos in
  while
    !p < len && (s.[!p] = ' ' || s.[!p] = '\t' || s.[!p] = '\n' || s.[!p] = '\r')
  do
    incr p
  done;
  !p

let parse_string_value s pos =
  (* pos should point to opening '"' *)
  let buf = Buffer.create 32 in
  let p = ref (pos + 1) in
  let len = String.length s in
  while !p < len && s.[!p] <> '"' do
    if s.[!p] = '\\' && !p + 1 < len then (
      (match s.[!p + 1] with
      | '"' -> Buffer.add_char buf '"'
      | '\\' -> Buffer.add_char buf '\\'
      | 'n' -> Buffer.add_char buf '\n'
      | 't' -> Buffer.add_char buf '\t'
      | 'r' -> Buffer.add_char buf '\r'
      | c ->
          Buffer.add_char buf '\\';
          Buffer.add_char buf c);
      p := !p + 2)
    else (
      Buffer.add_char buf s.[!p];
      incr p)
  done;
  (* skip closing quote *)
  (Buffer.contents buf, !p + 1)

let parse_int_value s pos =
  let len = String.length s in
  let start = pos in
  let p = ref pos in
  while !p < len && s.[!p] >= '0' && s.[!p] <= '9' do
    incr p
  done;
  (int_of_string (String.sub s start (!p - start)), !p)

let rec parse_value s pos =
  let p = skip_ws s pos in
  let len = String.length s in
  if p >= len then failwith "Json.parse: unexpected end of input"
  else
    match s.[p] with
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
  let items = ref [] in
  let p = ref (skip_ws s pos) in
  let len = String.length s in
  while !p < len && s.[!p] <> ']' do
    let v, next = parse_value s !p in
    items := !items @ [ v ];
    let next = skip_ws s next in
    if next < len && s.[next] = ',' then p := skip_ws s (next + 1)
    else p := next
  done;
  (Array !items, !p + 1)

and parse_object s pos =
  let fields = ref [] in
  let p = ref (skip_ws s pos) in
  let len = String.length s in
  while !p < len && s.[!p] <> '}' do
    let key, next = parse_string_value s !p in
    let next = skip_ws s next in
    let next = if next < len && s.[next] = ':' then next + 1 else next in
    let v, next = parse_value s next in
    fields := !fields @ [ (key, v) ];
    let next = skip_ws s next in
    if next < len && s.[next] = ',' then p := skip_ws s (next + 1)
    else p := next
  done;
  (Object !fields, !p + 1)

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

let field key obj =
  match List.assoc_opt key (to_object obj) with
  | Some v -> v
  | None -> failwith ("Json: missing field " ^ key)
