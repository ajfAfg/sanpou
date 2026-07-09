let parse input =
  input |> Lexing.from_string |> Sanpou.Parser.program Sanpou.Lexer.main

let check_ok input =
  let ast = parse input in
  Sanpou.Check_scope.check ast

let check_fails input =
  let ast = parse input in
  try
    Sanpou.Check_scope.check ast;
    Alcotest.fail "Expected scope error"
  with Sanpou.Check_scope.Error _ -> ()

let () =
  let open Alcotest in
  run "Check_scope"
    [
      ( "well_scoped",
        [
          test_case "non-callable defs may reuse builtin names" `Quick
            (fun () -> check_ok "mod m { def len = 5; var head = 0; }");
          test_case "label lookalikes are not reserved" `Quick (fun () ->
              check_ok "mod m { def L = 1; def L3x = 2; def l3 = 3; }");
          test_case "distinct module names" `Quick (fun () ->
              check_ok "mod a { def x = 1; } mod b { def y = 2; }");
          test_case "var still assignable outside the with" `Quick (fun () ->
              check_ok
                {|mod m {
                    var x = 0;
                    procedure p() {
                      with (x in {1, 2}) { await x > 0; }
                      x = 5;
                      return ();
                    }
                    process ps(self in 1..1) = p;
                  }|});
        ] );
      ( "ill_scoped",
        [
          test_case "unbound var" `Quick (fun () ->
              check_fails "mod m { def x = y; }");
          test_case "unbound in proc" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  procedure foo() { y = 1; return (); }\n\
                \  process ps(self in 1..2) = foo;\n\
                \  }");
          test_case "with binder scoped to its step" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() {\n\
                \    with (v in 1..3) { x = v; }\n\
                \    x = v;\n\
                \    return ();\n\
                \  }\n\
                \  process ps(self in 1..1) = foo;\n\
                \  }");
          test_case "assign to const" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  def x = 0;\n\
                \  procedure foo() { x = 1; return (); }\n\
                \  process ps(self in 1..2) = foo;\n\
                \  }");
          test_case "assign to with-binder shadowing a var" `Quick (fun () ->
              (* alpha resolves the target to the binder, so the outer
                 mutable x is not writable here; approving it against the
                 global used to emit a primed bound identifier *)
              check_fails
                {|mod m {
                    var x = 0;
                    procedure p() {
                      with (x in {1, 2}) { x = x + 1; }
                      return ();
                    }
                    process ps(self in 1..1) = p;
                  }|});
          test_case "assign to param shadowing a var" `Quick (fun () ->
              check_fails
                {|mod m {
                    var g = 0;
                    procedure p(g) { g = 5; return (); }
                    procedure main() { p(1); return (); }
                    process ps(self in 1..1) = main;
                  }|});
          test_case "with binder not assignable" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  procedure foo() { with (v in 1..3) { v = 1; } return (); }\n\
                \  process ps(self in 1..1) = foo;\n\
                \  }");
          test_case "statement call to a def function" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  def f(x) = x;\n\
                \  procedure g() { f(1); return (); }\n\
                \  process p(self in 1..1) = g;\n\
                \  }");
          test_case "applying a parameter" `Quick (fun () ->
              check_fails "mod m { def apply(g) = g(1); }");
          test_case "function used as a value" `Quick (fun () ->
              check_fails "mod m { def f(x) = x; def alias = f; }");
          test_case "procedure used as a value" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  procedure f() { return (); }\n\
                \  procedure g() { var h = f; return (); }\n\
                \  process p(self in 1..1) = g;\n\
                \  }");
          test_case "def function as process root" `Quick (fun () ->
              check_fails "mod m { def f(x) = x; process p(self in 1..1) = f; }");
          test_case "process root with parameters" `Quick (fun () ->
              (* the wrapper pushes no arguments, so params would start as
                 the null sentinel and crash TLC at runtime *)
              check_fails
                {|mod m {
                    var x = 0;
                    procedure f(n) { x = n + 1; return (); }
                    process p(self in 1..1) = f;
                  }|});
          test_case "self used outside a procedure" `Quick (fun () ->
              check_fails "mod m { def d = self; }");
          test_case "break in either arm outside loop" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  procedure foo() { either { break; } or { ; } return (); }\n\
                \  process ps(self in 1..1) = foo;\n\
                \  }");
          test_case "continue outside loop" `Quick (fun () ->
              check_fails
                "mod m { procedure foo() { continue; return (); } process \
                 ps(self in 1..1) = foo; }");
          test_case "duplicate module names" `Quick (fun () ->
              (* each module writes <name>.tla; a duplicate would silently
                 overwrite the previous module's output *)
              check_fails "mod m { def x = 1; } mod m { def y = 2; }");
          test_case "reserved: generated spec name" `Quick (fun () ->
              check_fails "mod m { def vars = 1; }");
          test_case "reserved: generated variable name" `Quick (fun () ->
              check_fails "mod m { var pc = 0; }");
          test_case "reserved: the null frame sentinel" `Quick (fun () ->
              check_fails "mod m { def x = `defaultInitValue; }");
          test_case "reserved: stdlib operator from EXTENDS" `Quick (fun () ->
              check_fails "mod m { def Cardinality(s) = 42; }");
          test_case "reserved: generated action label shape" `Quick (fun () ->
              check_fails "mod m { def L3 = 1; }");
        ] );
    ]
