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
          test_case "temporal property" `Quick (fun () ->
              check_ok "mod m { var x = 0; property p = globally(x == 0); }");
          test_case "nested temporal operators" `Quick (fun () ->
              check_ok
                "mod m { var x = 0; property p = globally(finally(x == 0)); }");
          test_case "property referencing a property" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var x = 0;\n\
                \  property p = globally(x == 0);\n\
                \  property q = p && finally(x == 1);\n\
                \  }");
          test_case "state predicate as a property" `Quick (fun () ->
              check_ok "mod m { var x = 0; property p = x >= 0; }");
          test_case "temporal formulas under all boolean connectives" `Quick
            (fun () ->
              check_ok
                "mod m {\n\
                \  var x = 0;\n\
                \  property a = globally(x >= 0);\n\
                \  property b = finally(x == 1) && !globally(x == 0) || a;\n\
                \  property c = globally(x >= 0 && finally(x == 1));\n\
                \  }");
          test_case "shadowed globally is an ordinary function" `Quick
            (fun () ->
              (* a user def shadows the builtin (see #79), so this call is
                 not a temporal operator *)
              check_ok
                "mod m {\n\
                \  var x = 0;\n\
                \  def globally(p) = p;\n\
                \  procedure f() { await globally(x == 0); return (); }\n\
                \  process ps(self in 1..1) = f;\n\
                \  }");
          test_case "local shadowing a property is not a reference" `Quick
            (fun () ->
              check_ok
                "mod m {\n\
                \  var x = 0;\n\
                \  property p = globally(x == 0);\n\
                \  procedure f() { var p = 1; x = p; return (); }\n\
                \  process ps(self in 1..1) = f;\n\
                \  }");
        ] );
      ( "rejected",
        [
          test_case "temporal operand of a comparison" `Quick (fun () ->
              (* TLA+ rejects a temporal formula as an = operand *)
              check_fails
                "mod m { var x = 0; property q = globally(x >= 0) == true; }");
          test_case "temporal formula in a set literal" `Quick (fun () ->
              check_fails
                "mod m { var x = 0; property q = finally(x == 1) in {true}; }");
          test_case "property reference as a comparison operand" `Quick
            (fun () ->
              check_fails
                "mod m {\n\
                \  var x = 0;\n\
                \  property p = globally(x == 0);\n\
                \  property q = p == true;\n\
                \  }");
          test_case "temporal operator in a def" `Quick (fun () ->
              check_fails "mod m { var x = 0; def p = globally(x == 0); }");
          test_case "temporal operator in await" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure f() { await globally(x == 0); return (); }\n\
                \  process ps(self in 1..1) = f;\n\
                \  }");
          test_case "temporal operator in function def" `Quick (fun () ->
              check_fails "mod m { def g(y) = globally(y == 0); }");
          test_case "temporal operator in var init" `Quick (fun () ->
              check_fails "mod m { var x = 0; var b = globally(x == 0); }");
          test_case "property referenced in a procedure" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var x = 0;\n\
                \  property p = globally(x == 0);\n\
                \  procedure f() { await p; return (); }\n\
                \  process ps(self in 1..1) = f;\n\
                \  }");
          test_case "property referenced in a def" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var x = 0;\n\
                \  property p = globally(x == 0);\n\
                \  def q = p;\n\
                \  }");
          test_case "property referenced in a var init" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var x = 0;\n\
                \  property p = globally(x == 0);\n\
                \  var b = p;\n\
                \  }");
        ] );
    ]
