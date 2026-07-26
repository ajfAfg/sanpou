let parse input =
  input |> Lexing.from_string |> Sanpou.Parser.program Sanpou.Lexer.main

let check_ok input =
  let ast = parse input in
  Sanpou.Check_steps.check ast

let check_fails input =
  let ast = parse input in
  try
    Sanpou.Check_steps.check ast;
    Alcotest.fail "Expected step-structure error"
  with Sanpou.Check_steps.Error _ -> ()

let () =
  let open Alcotest in
  run "Check_steps"
    [
      ( "well_formed",
        [
          test_case "multiple path writes to one variable in one step" `Quick
            (fun () ->
              check_ok
                "mod m {\n\
                \  var r = {a: 0, b: 0};\n\
                \  procedure foo() { r.a = 1, r.b = 2; return (); }\n\
                \  process ps(self in 1..1) = foo;\n\
                \  }");
          test_case "whole writes to one variable in separate steps" `Quick
            (fun () ->
              check_ok
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() { x = 1; x = 2; return (); }\n\
                \  process ps(self in 1..1) = foo;\n\
                \  }");
          test_case "control transfer last in its step" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure g() { return (); }\n\
                \  procedure foo() {\n\
                \    while (x < 1) { x = 1, continue; }\n\
                \    x = 2, g();\n\
                \    x = 3, return ();\n\
                \  }\n\
                \  process ps(self in 1..1) = foo;\n\
                \  }");
        ] );
      ( "ill_formed",
        [
          test_case "two whole writes in one step" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() { x = 1, x = 2; return (); }\n\
                \  process ps(self in 1..1) = foo;\n\
                \  }");
          test_case "whole write then path write in one step" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var r = {a: 0, b: 0};\n\
                \  procedure foo() { r = {a: 9, b: 9}, r.a = 5; return (); }\n\
                \  process ps(self in 1..1) = foo;\n\
                \  }");
          test_case "path write then whole write in one step" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var r = {a: 0, b: 0};\n\
                \  procedure foo() { r.a = 5, r = {a: 9, b: 9}; return (); }\n\
                \  process ps(self in 1..1) = foo;\n\
                \  }");
          test_case "two whole writes to a local in one step" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  procedure foo() { var l = 0; l = 1, l = 2; return (); }\n\
                \  process ps(self in 1..1) = foo;\n\
                \  }");
          test_case "conflicting writes in a with step" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() {\n\
                \    with (v in 1..3) { x = v, x = 0; }\n\
                \    return ();\n\
                \  }\n\
                \  process ps(self in 1..1) = foo;\n\
                \  }");
          test_case "two calls in one step" `Quick (fun () ->
              check_fails
                {|mod m {
                    procedure a() { return (); }
                    procedure b() { return (); }
                    procedure f() { a(), b(); return (); }
                    process p(self in 1..1) = f;
                  }|});
          test_case "call then return in one step" `Quick (fun () ->
              check_fails
                {|mod m {
                    procedure a() { return (); }
                    procedure f() { a(), return (); }
                    process p(self in 1..1) = f;
                  }|});
          test_case "statements after return in one step" `Quick (fun () ->
              check_fails
                {|mod m {
                    var x = 0;
                    procedure f() { return (), x = 1; }
                    process p(self in 1..1) = f;
                  }|});
          test_case "statements after break in one step" `Quick (fun () ->
              check_fails
                {|mod m {
                    var x = 0;
                    procedure f() {
                      while (true) { break, x = 1; }
                      return ();
                    }
                    process p(self in 1..1) = f;
                  }|});
        ] );
    ]
