(* Tests for source map (de)serialization, trace module-name detection, and
   trace rendering (demangling, per-process value unwrapping) *)

open Sanpou

let entry label proc_name description line : Source_map.entry =
  { label; proc_name; description; line; col = 1 }

let smap_v2 : Source_map.t =
  {
    module_name = "factorial";
    entries =
      [
        entry "L2" "fact" "if (x == 0) [check]" 7;
        entry "L5" "fact" "var ans = x * fact(x - 1)" 10;
        entry "__w_workers_entry__" "workers" "[process workers starts worker]"
          20;
      ];
    vars =
      [
        { tla_name = "x"; original = "x"; proc = None; kind = Global };
        { tla_name = "x__1"; original = "x"; proc = Some "fact"; kind = Param };
        {
          tla_name = "ans__2";
          original = "ans";
          proc = Some "fact";
          kind = Local;
        };
        {
          tla_name = "callRet__1";
          original = "callRet__1";
          proc = Some "fact";
          kind = CallRet "fact";
        };
      ];
  }

(* ===== Source map JSON ===== *)

let test_v2_roundtrip () =
  let json = Source_map.to_json smap_v2 in
  let parsed = Source_map.from_json json in
  Alcotest.(check string) "module" "factorial" parsed.module_name;
  Alcotest.(check int) "entries" 3 (List.length parsed.entries);
  Alcotest.(check int) "vars" 4 (List.length parsed.vars);
  let x1 =
    List.find
      (fun (v : Source_map.var_entry) -> v.tla_name = "x__1")
      parsed.vars
  in
  Alcotest.(check string) "original" "x" x1.original;
  Alcotest.(check (option string)) "proc" (Some "fact") x1.proc;
  Alcotest.(check bool) "kind param" true (x1.kind = Source_map.Param);
  let cr =
    List.find
      (fun (v : Source_map.var_entry) -> v.tla_name = "callRet__1")
      parsed.vars
  in
  Alcotest.(check bool)
    "kind callret" true
    (cr.kind = Source_map.CallRet "fact")

let test_v1_backcompat () =
  let v1_json =
    {|[
    { "label": "L2", "proc": "fact", "desc": "if (x__1 == 0) [check]", "line": 7, "col": 5 }
]|}
  in
  let parsed = Source_map.from_json v1_json in
  Alcotest.(check string) "no module" "" parsed.module_name;
  Alcotest.(check int) "entries" 1 (List.length parsed.entries);
  Alcotest.(check int) "no vars" 0 (List.length parsed.vars)

(* ===== Module name detection ===== *)

let test_module_name_detection () =
  let out =
    "Semantic processing of module Naturals\n\
     Semantic processing of module _TLCTrace\n\
     Semantic processing of module factorial\n"
  in
  let trace = Trace_reader.parse out in
  Alcotest.(check (option string))
    "last non-internal module" (Some "factorial") trace.module_name

let test_module_name_absent () =
  let trace = Trace_reader.parse "no sany output here\n" in
  Alcotest.(check (option string)) "none" None trace.module_name

(* ===== Tuple splitting ===== *)

let test_split_flat () =
  Alcotest.(check (option (list string)))
    "flat" (Some [ "1"; "2"; "3" ])
    (Trace_printer.split_tuple_elements "<<1, 2, 3>>")

let test_split_nested () =
  Alcotest.(check (option (list string)))
    "nested"
    (Some [ "<<1, 2>>"; "3" ])
    (Trace_printer.split_tuple_elements "<<<<1, 2>>, 3>>")

let test_split_records () =
  Alcotest.(check (option (list string)))
    "record with commas"
    (Some [ "[a |-> 1, b |-> 2]"; "0" ])
    (Trace_printer.split_tuple_elements "<<[a |-> 1, b |-> 2], 0>>")

let test_split_not_tuple () =
  Alcotest.(check (option (list string)))
    "function over non-1..n domain" None
    (Trace_printer.split_tuple_elements "(5 :> 0 @@ 6 :> 0)");
  Alcotest.(check (option (list string)))
    "scalar" None
    (Trace_printer.split_tuple_elements "42")

let test_split_empty () =
  Alcotest.(check (option (list string)))
    "empty tuple" (Some [])
    (Trace_printer.split_tuple_elements "<<>>")

(* ===== Display names ===== *)

let test_display_names () =
  (* "x" collides between the global and fact's param: global stays bare,
     the param is qualified. "ans" is unique. *)
  Alcotest.(check string)
    "global bare" "x"
    (Trace_printer.display_var_name smap_v2 "x");
  Alcotest.(check string)
    "param qualified" "x (fact)"
    (Trace_printer.display_var_name smap_v2 "x__1");
  Alcotest.(check string)
    "unique local bare" "ans"
    (Trace_printer.display_var_name smap_v2 "ans__2");
  Alcotest.(check string)
    "unknown passthrough" "pc"
    (Trace_printer.display_var_name smap_v2 "pc")

(* ===== Rendering ===== *)

let test_render_demangles_and_unwraps () =
  let trace : Trace_reader.trace =
    {
      is_deadlock = false;
      module_name = Some "factorial";
      steps =
        [
          {
            step_num = 1;
            header = Trace_reader.InitialPredicate;
            state =
              [
                { var_name = "x"; value = "0" };
                { var_name = "x__1"; value = "<<0>>" };
                { var_name = "ans__2"; value = "<<0>>" };
                { var_name = "callRet__1"; value = "<<0>>" };
                { var_name = "pc"; value = "<<\"L2\">>" };
              ];
          };
          {
            step_num = 2;
            header = Trace_reader.Action { label = "L5"; process_id = 1 };
            state =
              [
                { var_name = "x"; value = "0" };
                { var_name = "x__1"; value = "<<3>>" };
                { var_name = "ans__2"; value = "<<6>>" };
                { var_name = "callRet__1"; value = "<<2>>" };
                { var_name = "pc"; value = "<<\"L4\">>" };
              ];
          };
        ];
    }
  in
  let rendered = Trace_printer.render trace smap_v2 in
  (* Mangled names must not appear *)
  Alcotest.(check bool)
    "no x__1" false
    (Str.string_match (Str.regexp ".*x__1.*") rendered 0);
  Alcotest.(check bool)
    "no callRet" false
    (Str.string_match (Str.regexp ".*callRet.*") rendered 0);
  (* Per-process values unwrapped *)
  Alcotest.(check bool)
    "unwrapped param" true
    (Str.string_match (Str.regexp ".*x (fact) = 3.*")
       (Str.global_replace (Str.regexp "\n") " " rendered)
       0);
  Alcotest.(check bool)
    "unwrapped local" true
    (Str.string_match (Str.regexp ".*ans = 6.*")
       (Str.global_replace (Str.regexp "\n") " " rendered)
       0);
  (* Header uses the demangled description *)
  Alcotest.(check bool)
    "header desc" true
    (Str.string_match
       (Str.regexp ".*fact (process 1): var ans = x \\* fact(x - 1).*")
       (Str.global_replace (Str.regexp "\n") " " rendered)
       0)

let () =
  Alcotest.run "Trace"
    [
      ( "source_map",
        [
          Alcotest.test_case "v2 roundtrip" `Quick test_v2_roundtrip;
          Alcotest.test_case "v1 backcompat" `Quick test_v1_backcompat;
        ] );
      ( "module_name",
        [
          Alcotest.test_case "detected" `Quick test_module_name_detection;
          Alcotest.test_case "absent" `Quick test_module_name_absent;
        ] );
      ( "split_tuple",
        [
          Alcotest.test_case "flat" `Quick test_split_flat;
          Alcotest.test_case "nested" `Quick test_split_nested;
          Alcotest.test_case "records" `Quick test_split_records;
          Alcotest.test_case "not a tuple" `Quick test_split_not_tuple;
          Alcotest.test_case "empty" `Quick test_split_empty;
        ] );
      ( "display",
        [
          Alcotest.test_case "names" `Quick test_display_names;
          Alcotest.test_case "render" `Quick test_render_demangles_and_unwraps;
        ] );
    ]
