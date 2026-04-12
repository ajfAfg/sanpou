let parse input =
  input |> Lexing.from_string |> Sanpou.Parser.program Sanpou.Lexer.main

let compile ?(config = Sanpou.Config.default) cst =
  Sanpou.Typing.check cst;
  cst |> Sanpou.Alpha_convert.transform |> Sanpou.Linearize.linearize
  |> List.map (Sanpou.Emit_tla.generate_module ~config)

let has_substring needle haystack =
  try
    let _ = Str.search_forward (Str.regexp_string needle) haystack 0 in
    true
  with Not_found -> false

let check_has needle haystack =
  Alcotest.(check bool) needle true (has_substring needle haystack)

let check_not_has needle haystack =
  Alcotest.(check bool) needle false (has_substring needle haystack)

let check_before first second haystack =
  let first_pos = Str.search_forward (Str.regexp_string first) haystack 0 in
  let second_pos = Str.search_forward (Str.regexp_string second) haystack 0 in
  Alcotest.(check bool)
    (first ^ " before " ^ second)
    true (first_pos < second_pos)

let () =
  let open Alcotest in
  run "Config"
    [
      ( "parse",
        [
          test_case "defaults when optional fields missing" `Quick (fun () ->
              let config = Sanpou.Config.from_string "{}" in
              check bool "deadlock" true config.checks.deadlock;
              check bool "termination" false config.checks.termination;
              check (list string) "properties" [] config.properties);
          test_case "reads booleans and properties" `Quick (fun () ->
              let config =
                Sanpou.Config.from_string
                  {|{
  "checks": { "deadlock": false, "termination": true },
  "properties": ["convergence"]
}|}
              in
              check bool "deadlock" false config.checks.deadlock;
              check bool "termination" true config.checks.termination;
              check (list string) "properties" [ "convergence" ]
                config.properties);
        ] );
      ( "cfg",
        [
          test_case "formats cfg and auto-adds termination property" `Quick
            (fun () ->
              let config =
                Sanpou.Config.from_string
                  {|{
  "checks": { "deadlock": false, "termination": true },
  "properties": ["convergence"]
}|}
              in
              let cfg = Sanpou.Config.to_cfg_string config in
              check_has "SPECIFICATION Spec" cfg;
              check_has "CHECK_DEADLOCK FALSE" cfg;
              check_has "PROPERTIES" cfg;
              check_has "    convergence" cfg;
              check_has "    Termination" cfg);
          test_case "does not duplicate Termination property" `Quick (fun () ->
              let config =
                Sanpou.Config.from_string
                  {|{
  "checks": { "deadlock": true, "termination": true },
  "properties": ["Termination"]
}|}
              in
              let cfg = Sanpou.Config.to_cfg_string config in
              let first =
                Str.search_forward (Str.regexp_string "    Termination") cfg 0
              in
              let duplicated =
                try
                  ignore
                    (Str.search_forward
                       (Str.regexp_string "    Termination")
                       cfg (first + 1));
                  true
                with Not_found -> false
              in
              check bool "duplicated" false duplicated);
        ] );
      ( "emit",
        [
          test_case "termination disabled leaves tla unchanged" `Quick
            (fun () ->
              let cst =
                parse
                  "mod m {\n\
                   var x = 0;\n\
                   fn f() { x = 1 - x; }\n\
                   fair process p = f in 1..1;\n\
                   }\n"
              in
              let tla = compile cst |> List.hd |> Tla.Tla_printer.render in
              check_not_has "Terminating ==" tla;
              check_not_has "Termination ==" tla;
              check_not_has "\\/ Terminating" tla);
          test_case "termination enabled emits predicates and next branch"
            `Quick (fun () ->
              let cst =
                parse
                  "mod m {\n\
                   var x = 0;\n\
                   fn f() { x = 1 - x; }\n\
                   fair process p = f in 1..1;\n\
                   }\n"
              in
              let config =
                Sanpou.Config.from_string
                  {|{ "checks": { "termination": true, "deadlock": false } }|}
              in
              let tla =
                compile ~config cst |> List.hd |> Tla.Tla_printer.render
              in
              check_has "Terminating ==" tla;
              check_has "\\A self \\in ProcSet: pc[self] = \"Done\"" tla;
              check_has "UNCHANGED vars" tla;
              check_has
                "Termination == <>(\\A self \\in ProcSet: pc[self] = \"Done\")"
                tla;
              check_has "\\/ Terminating" tla;
              check_before "Terminating ==" "Next ==" tla;
              check_has "WF_vars(__process_p_wrapper__(self))" tla;
              check_has "WF_vars(f(self))" tla);
        ] );
    ]
