let parse input =
  input |> Lexing.from_string |> Sanpou.Parser.program Sanpou.Lexer.main

(* The pass runs on the resolved tree, after scoping is settled. *)
let run input = parse input |> Sanpou.Alpha_convert.transform

let check_ok input = Sanpou.Check_temporal.check (run input)

let check_fails input =
  let resolved = run input in
  try
    Sanpou.Check_temporal.check resolved;
    Alcotest.fail "Expected a temporal placement error"
  with Sanpou.Check_temporal.Error _ -> ()

let () =
  let open Alcotest in
  run "CheckTemporal"
    [
      ( "allowed",
        [
          test_case "temporal def" `Quick (fun () ->
              check_ok "mod m { var x = 0; def p = globally(x == 0); }");
          test_case "nested temporal operators" `Quick (fun () ->
              check_ok
                "mod m { var x = 0; def p = globally(finally(x == 0)); }");
          test_case "def referencing a temporal def" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var x = 0;\n\
                \  def p = globally(x == 0);\n\
                \  def q = p && finally(x == 1);\n\
                \  }");
          test_case "local shadowing a temporal def is not a reference" `Quick
            (fun () ->
              check_ok
                "mod m {\n\
                \  var x = 0;\n\
                \  def p = globally(x == 0);\n\
                \  fn f() { var p = 1; x = p; return (); }\n\
                \  process ps = f in 1..1;\n\
                \  }");
        ] );
      ( "rejected",
        [
          test_case "temporal operator in await" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var x = 0;\n\
                \  fn f() { await globally(x == 0); return (); }\n\
                \  process ps = f in 1..1;\n\
                \  }");
          test_case "temporal operator in function def" `Quick (fun () ->
              check_fails "mod m { def g(y) = globally(y == 0); }");
          test_case "temporal operator in var init" `Quick (fun () ->
              check_fails "mod m { var x = 0; var b = globally(x == 0); }");
          test_case "temporal def referenced in a procedure" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var x = 0;\n\
                \  def p = globally(x == 0);\n\
                \  fn f() { await p; return (); }\n\
                \  process ps = f in 1..1;\n\
                \  }");
          test_case "temporal def referenced in a var init" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var x = 0;\n\
                \  def p = globally(x == 0);\n\
                \  var b = p;\n\
                \  }");
          test_case "temporal def referenced transitively" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var x = 0;\n\
                \  def p = globally(x == 0);\n\
                \  def q = p;\n\
                \  fn f() { await q; return (); }\n\
                \  process ps = f in 1..1;\n\
                \  }");
        ] );
    ]
