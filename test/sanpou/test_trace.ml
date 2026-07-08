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
  Alcotest.(check bool) "kind callret" true (cr.kind = Source_map.CallRet "fact")

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

(* ===== Action header parsing ===== *)

(* TLC prints action headers without a process id ("<L1 line ... of module
   m>"); the older "<L1(1) line ...>" form carries one. Both arrive inside
   tool-mode state blocks (message code 2217). *)
let state_block body =
  "@!@!@STARTMSG 2217:4 @!@!@\n" ^ body ^ "@!@!@ENDMSG 2217 @!@!@\n"

let test_action_header_without_pid () =
  let out =
    state_block "1: <Initial predicate>\n/\\ x = 0\n"
    ^ state_block
        "2: <L1 line 16, col 5 to line 18, col 31 of module dl>\n/\\ x = 1\n"
  in
  let trace = Trace_reader.parse out in
  match trace.steps with
  | [ s1; s2 ] ->
      Alcotest.(check bool)
        "initial" true
        (s1.header = Trace_reader.InitialPredicate);
      Alcotest.(check bool)
        "action label, no pid" true
        (s2.header = Trace_reader.Action { label = "L1"; process_id = None })
  | steps ->
      Alcotest.fail
        (Printf.sprintf "expected 2 steps, got %d" (List.length steps))

let test_action_header_with_pid () =
  let out = state_block "2: <L34(1) line 5, col 1 of module m>\n/\\ x = 1\n" in
  let trace = Trace_reader.parse out in
  match trace.steps with
  | [ s ] ->
      Alcotest.(check bool)
        "action label and pid" true
        (s.header = Trace_reader.Action { label = "L34"; process_id = Some 1 })
  | steps ->
      Alcotest.fail
        (Printf.sprintf "expected 1 step, got %d" (List.length steps))

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
    "flat"
    (Some [ "1"; "2"; "3" ])
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

(* ===== Record field splitting ===== *)

let test_record_fields () =
  Alcotest.(check (option (list (pair string string))))
    "simple"
    (Some [ ("x__1", "3"); ("ans__2", "0") ])
    (Trace_printer.split_record_fields "[x__1 |-> 3, ans__2 |-> 0]");
  (* TLC reorders fields and quotes strings *)
  Alcotest.(check (option (list (pair string string))))
    "reordered with strings"
    (Some
       [ ("ans__2", "0"); ("return_pc", "\"L7\""); ("procedure", "\"fact\"") ])
    (Trace_printer.split_record_fields
       "[ans__2 |-> 0, return_pc |-> \"L7\", procedure |-> \"fact\"]");
  Alcotest.(check (option (list (pair string string))))
    "nested values"
    (Some [ ("value", "<<1, 2>>"); ("m", "[a |-> 1]") ])
    (Trace_printer.split_record_fields "[value |-> <<1, 2>>, m |-> [a |-> 1]]");
  Alcotest.(check (option (list (pair string string))))
    "not a record" None
    (Trace_printer.split_record_fields "<<1, 2>>")

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

let test_display_null () =
  Alcotest.(check string)
    "model value" "null"
    (Trace_printer.display_field_value "defaultInitValue");
  Alcotest.(check string)
    "legacy string sentinel" "null"
    (Trace_printer.display_field_value "\"__null__\"");
  Alcotest.(check string)
    "user value passthrough" "42"
    (Trace_printer.display_field_value "42")

(* ===== Rendering ===== *)

(* Locals live inside the stack frames: the printer must dig them out of the
   top frame, diff same-depth frames, and show frame context on call/return. *)
let test_render_frame_locals () =
  (* [ans] is a raw TLC value string: unassigned fields hold the
     defaultInitValue model value *)
  let fact_frame ~x ~ans ~call_ret =
    Printf.sprintf
      "[x__1 |-> %d, ans__2 |-> %s, callRet__1 |-> %d, return_pc |-> \"L11\", \
       procedure |-> \"fact\"]"
      x ans call_ret
  in
  let null = "defaultInitValue" in
  let stack frames = "<<<<" ^ String.concat ", " frames ^ ">>>>" in
  let state ~stack_value ~x ~pc =
    [
      { Trace_reader.var_name = "x"; value = string_of_int x };
      { var_name = "stack"; value = stack_value };
      { var_name = "pc"; value = pc };
    ]
  in
  let trace : Trace_reader.trace =
    {
      is_deadlock = false;
      module_name = Some "factorial";
      errors = [];
      stuttering_step = None;
      steps =
        [
          {
            step_num = 1;
            header = Trace_reader.InitialPredicate;
            state = state ~stack_value:"<<<<>>>>" ~x:0 ~pc:"<<\"L10\">>";
          };
          (* Same depth: plain assignment inside the frame -> diff *)
          {
            step_num = 2;
            header = Trace_reader.Action { label = "L5"; process_id = Some 1 };
            state =
              state
                ~stack_value:(stack [ fact_frame ~x:3 ~ans:null ~call_ret:2 ])
                ~x:0 ~pc:"<<\"L5\">>";
          };
          {
            step_num = 3;
            header = Trace_reader.Action { label = "L5"; process_id = Some 1 };
            state =
              state
                ~stack_value:(stack [ fact_frame ~x:3 ~ans:"6" ~call_ret:2 ])
                ~x:0 ~pc:"<<\"L4\">>";
          };
          (* Depth increased (call): frame context, not a diff *)
          {
            step_num = 4;
            header = Trace_reader.Action { label = "L2"; process_id = Some 1 };
            state =
              state
                ~stack_value:
                  (stack
                     [
                       fact_frame ~x:2 ~ans:null ~call_ret:0;
                       fact_frame ~x:3 ~ans:"6" ~call_ret:2;
                     ])
                ~x:0 ~pc:"<<\"L2\">>";
          };
          (* Depth decreased (return): frame context, not a diff *)
          {
            step_num = 5;
            header = Trace_reader.Action { label = "L2"; process_id = Some 1 };
            state =
              state
                ~stack_value:(stack [ fact_frame ~x:3 ~ans:"6" ~call_ret:2 ])
                ~x:0 ~pc:"<<\"L2\">>";
          };
        ];
    }
  in
  let rendered = Trace_printer.render trace smap_v2 in
  let flat = Str.global_replace (Str.regexp "\n") " " rendered in
  let contains hay needle =
    Str.string_match (Str.regexp (".*" ^ Str.quote needle ^ ".*")) hay 0
  in
  (* The rendered text of one step, newlines flattened *)
  let step_block n =
    let marker = Printf.sprintf "Step %d:" n in
    let blocks = Str.full_split (Str.regexp "Step [0-9]+:") flat in
    let rec find = function
      | Str.Delim d :: Str.Text t :: rest ->
          if d = marker then d ^ t else find rest
      | _ :: rest -> find rest
      | [] -> Alcotest.fail ("step not rendered: " ^ marker)
    in
    find blocks
  in
  (* Mangled and internal names must not appear *)
  Alcotest.(check bool) "no x__1" false (contains flat "x__1");
  Alcotest.(check bool) "no callRet" false (contains flat "callRet");
  Alcotest.(check bool) "no return_pc" false (contains flat "return_pc");
  (* Same-depth assignment diffs only the changed field *)
  Alcotest.(check bool) "diffed local" true (contains (step_block 3) "ans = 6");
  Alcotest.(check bool)
    "unchanged field not diffed" false
    (contains (step_block 3) "x (fact)");
  (* Depth change (call at step 4, return at step 5) shows frame context;
     unassigned fields render as null *)
  Alcotest.(check bool)
    "call frame context" true
    (contains (step_block 4) "(frame: x (fact) = 2, ans = null)");
  Alcotest.(check bool)
    "return frame context" true
    (contains (step_block 5) "(frame: x (fact) = 3, ans = 6)");
  (* Initial state: empty stacks, globals only *)
  Alcotest.(check bool) "initial global" true (contains (step_block 1) "x = 0");
  (* Header uses the demangled description *)
  Alcotest.(check bool)
    "header desc" true
    (contains flat "fact (process 1): var ans = x * fact(x - 1)")

(* Headers without a pid: the acting process is recovered from the pc diff *)
let test_render_infers_process () =
  let state ~x ~pc =
    [
      { Trace_reader.var_name = "x"; value = string_of_int x };
      { var_name = "pc"; value = pc };
    ]
  in
  let trace : Trace_reader.trace =
    {
      is_deadlock = false;
      module_name = Some "factorial";
      errors = [];
      stuttering_step = None;
      steps =
        [
          {
            step_num = 1;
            header = Trace_reader.InitialPredicate;
            state = state ~x:0 ~pc:"<<\"L2\", \"L2\">>";
          };
          (* Second process moved: its pc changed, the first one's did not *)
          {
            step_num = 2;
            header = Trace_reader.Action { label = "L5"; process_id = None };
            state = state ~x:1 ~pc:"<<\"L2\", \"L5\">>";
          };
        ];
    }
  in
  let rendered = Trace_printer.render trace smap_v2 in
  let contains needle =
    Str.string_match
      (Str.regexp (".*" ^ Str.quote needle ^ ".*"))
      (Str.global_replace (Str.regexp "\n") " " rendered)
      0
  in
  Alcotest.(check bool)
    "inferred process in header" true
    (contains "fact (process 2): var ans = x * fact(x - 1)")

let () =
  Alcotest.run "Trace"
    [
      ( "source_map",
        [
          Alcotest.test_case "v2 roundtrip" `Quick test_v2_roundtrip;
          Alcotest.test_case "v1 backcompat" `Quick test_v1_backcompat;
        ] );
      ( "action_header",
        [
          Alcotest.test_case "without pid" `Quick test_action_header_without_pid;
          Alcotest.test_case "with pid" `Quick test_action_header_with_pid;
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
          Alcotest.test_case "record fields" `Quick test_record_fields;
        ] );
      ( "display",
        [
          Alcotest.test_case "names" `Quick test_display_names;
          Alcotest.test_case "null sentinel" `Quick test_display_null;
          Alcotest.test_case "render" `Quick test_render_frame_locals;
          Alcotest.test_case "render infers process" `Quick
            test_render_infers_process;
        ] );
    ]
