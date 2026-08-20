(* Property-based serialize/parse roundtrip for the hand-written [Json]
   module: for a random value, [Json.parse (Json.to_string v)] must be [v].
   This pair runs in production — the compiler writes the source map with
   [to_string] and the trace annotator reads it back with [parse] — so the
   roundtrip is exactly the contract between them.

   The generator covers the whole [Json.t] type, including edge cases the
   module must handle: negative integers, strings mixing escaped characters
   (the writer escapes only the double quote, backslash, LF, TAB, and CR;
   everything else roundtrips as itself), empty arrays/objects, and duplicate
   object keys (fields are kept in order, so duplicates roundtrip too). *)

module Gen = QCheck2.Gen

(* Weighted toward the characters the writer treats specially. *)
let gen_json_string =
  Gen.string_size
    ~gen:(Gen.oneof_list [ 'a'; 'Z'; '0'; ' '; '"'; '\\'; '\n'; '\t'; '\r' ])
    (Gen.int_bound 6)

let gen_json : Json.t Gen.t =
  Gen.sized @@ Gen.fix
  @@ fun self n ->
  let leaf =
    Gen.oneof
      [
        Gen.map (fun s -> Json.String s) gen_json_string;
        Gen.map (fun i -> Json.Int i) Gen.int;
        Gen.map (fun b -> Json.Bool b) Gen.bool;
      ]
  in
  if n <= 0 then leaf
  else
    let sub = self (n / 2) in
    Gen.oneof
      [
        leaf;
        Gen.map (fun vs -> Json.Array vs) (Gen.list_size (Gen.int_bound 3) sub);
        Gen.map
          (fun fields -> Json.Object fields)
          (Gen.list_size (Gen.int_bound 3) (Gen.pair gen_json_string sub));
      ]

let rec show_json = function
  | Json.String s -> Printf.sprintf "String %S" s
  | Json.Int i -> Printf.sprintf "Int %d" i
  | Json.Bool b -> Printf.sprintf "Bool %b" b
  | Json.Array vs ->
      "Array [" ^ String.concat "; " (List.map show_json vs) ^ "]"
  | Json.Object fields ->
      "Object ["
      ^ String.concat "; "
          (List.map
             (fun (k, v) -> Printf.sprintf "(%S, %s)" k (show_json v))
             fields)
      ^ "]"

let roundtrip v =
  let printed = Json.to_string v in
  match Json.parse printed with
  | reparsed ->
      reparsed = v
      || QCheck2.Test.fail_reportf
           "reparsed value differs@.--- printed ---@.%s@.--- reparsed ---@.%s"
           printed (show_json reparsed)
  | exception exn ->
      QCheck2.Test.fail_reportf "printed value does not parse: %s@.%s"
        (Printexc.to_string exn) printed

let roundtrip_test =
  QCheck2.Test.make ~count:1000 ~name:"serialize/parse roundtrip"
    ~print:show_json gen_json roundtrip

let () =
  Alcotest.run "JsonPbt"
    [ ("roundtrip", [ QCheck_alcotest.to_alcotest roundtrip_test ]) ]
