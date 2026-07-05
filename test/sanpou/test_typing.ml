let parse input =
  input |> Lexing.from_string |> Sanpou.Parser.program Sanpou.Lexer.main

let check_ok input =
  let ast = parse input in
  Sanpou.Typing.check ast

let check_fails input =
  let ast = parse input in
  try
    Sanpou.Typing.check ast;
    Alcotest.fail "Expected type error"
  with Sanpou.Typing.Type_error _ -> ()

let () =
  let open Alcotest in
  run "Typing"
    [
      ( "well_typed",
        [
          test_case "int literal" `Quick (fun () ->
              check_ok "mod m { def x = 42; }");
          test_case "bool literal" `Quick (fun () ->
              check_ok "mod m { def x = true; }");
          test_case "arithmetic" `Quick (fun () ->
              check_ok "mod m { def x = 1 + 2 * 3; }");
          test_case "comparison" `Quick (fun () ->
              check_ok "mod m { def x = 1 < 2; }");
          test_case "equality" `Quick (fun () ->
              check_ok "mod m { def x = 1 == 2; }");
          test_case "inequality" `Quick (fun () ->
              check_ok "mod m { def x = 1 != 2; }");
          test_case "or" `Quick (fun () ->
              check_ok "mod m { def x = true || false; }");
          test_case "greater than" `Quick (fun () ->
              check_ok "mod m { def x = 2 > 1; }");
          test_case "division" `Quick (fun () ->
              check_ok "mod m { def x = 7 / 2; }");
          test_case "modulo" `Quick (fun () ->
              check_ok "mod m { def x = 7 % 2; }");
          test_case "logical not" `Quick (fun () ->
              check_ok "mod m { def x = !true; }");
          test_case "len of sequence" `Quick (fun () ->
              check_ok "mod m { def x = len([1, 2, 3]); }");
          test_case "if expression" `Quick (fun () ->
              check_ok "mod m { def x = if (true) { 1 } else { 2 }; }");
          test_case "if expression in function def" `Quick (fun () ->
              check_ok "mod m { def abs(x) = if (x < 0) { -x } else { x }; }");
          test_case "if expression in assignment" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() { x = if (x == 0) { 1 } else { x }; return (); }\n\
                \  process ps = foo in 1..1;\n\
                \  }");
          test_case "forall over range" `Quick (fun () ->
              check_ok "mod m { def p = forall (i in 1..3) { i < 4 }; }");
          test_case "exists over range" `Quick (fun () ->
              check_ok "mod m { def p = exists (i in 1..3) { i == 2 }; }");
          test_case "nested quantifiers" `Quick (fun () ->
              check_ok
                "mod m { def p = forall (i in 1..2) { exists (j in 1..2) { i \
                 == j } }; }");
          test_case "quantifier as operand" `Quick (fun () ->
              check_ok
                "mod m { def p = forall (i in 1..2) { i < 3 } && true; }");
          test_case "quantifier in await" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var xs = [1, 2, 3];\n\
                \  procedure foo() {\n\
                \    await forall (i in 1..3) { xs[i] > 0 };\n\
                \    return ();\n\
                \  }\n\
                \  process ps = foo in 1..1;\n\
                \  }");
          test_case "non-callable defs may reuse builtin names" `Quick
            (fun () -> check_ok "mod m { def len = 5; var head = 0; }");
          test_case "def shadows the builtin from its point onward" `Quick
            (fun () ->
              (* builtin head needs a sequence; the shadowing def takes an
                 int, so both calls typecheck only under lexical
                 resolution *)
              check_ok
                "mod m {\n\
                \  def a = head([1, 2]);\n\
                \  def head(x) = x + 1;\n\
                \  def b = head(2);\n\
                \  }");
          test_case "procedure shadows the builtin" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var x = [];\n\
                \  procedure append(v) { x = [v]; return (); }\n\
                \  procedure f() { append(1); return (); }\n\
                \  process ps = f in 1..1;\n\
                \  }");
          test_case "property is bool" `Quick (fun () ->
              check_ok
                "mod m { var x = 0; property p = finally(x == 1); }");
          test_case "assert statement" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() { assert x >= 0, x = x + 1; return (); }\n\
                \  process ps = foo in 1..1;\n\
                \  }");
          test_case "nested subscript assignment" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var grid = { i in 1..2 -> { j in 1..2 -> 0 } };\n\
                \  procedure foo() { grid[1][2] = 5; return (); }\n\
                \  process ps = foo in 1..1;\n\
                \  }");
          test_case "with statement" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() {\n\
                \    with (v in 1..3) { await v > x, x = v; }\n\
                \    return ();\n\
                \  }\n\
                \  process ps = foo in 1..1;\n\
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
                \  process ps = foo in 1..1;\n\
                \  }");
          test_case "either arms" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() {\n\
                \    either { x = 1; } or { x = 2; } or { await x > 0; }\n\
                \    return ();\n\
                \  }\n\
                \  process ps = foo in 1..1;\n\
                \  }");
          test_case "break in either arm inside loop" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() {\n\
                \    while (true) {\n\
                \      either { break; } or { x = x + 1; }\n\
                \    }\n\
                \    return ();\n\
                \  }\n\
                \  process ps = foo in 1..1;\n\
                \  }");
          test_case "else if chain" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() {\n\
                \    if (x == 0) { x = 1; } else if (x == 1) { x = 2; }\n\
                \    else { x = 0; }\n\
                \    return ();\n\
                \  }\n\
                \  process ps = foo in 1..1;\n\
                \  }");
          test_case "len is int" `Quick (fun () ->
              check_ok "mod m { def x = len([1]) + 1; }");
          test_case "unary minus literal" `Quick (fun () ->
              check_ok "mod m { def x = -1; }");
          test_case "unary minus paren expr" `Quick (fun () ->
              check_ok "mod m { def x = -(1 + 2); }");
          test_case "function def" `Quick (fun () ->
              check_ok "mod m { def f(x) = x + 1; }");
          test_case "function app in const" `Quick (fun () ->
              check_ok "mod m { def f(x) = x + 1; def y = f(3); }");
          test_case "var decl int" `Quick (fun () ->
              check_ok "mod m { var x = 0; }");
          test_case "var decl range" `Quick (fun () ->
              check_ok "mod m { var x in 1..3; }");
          test_case "var decl range is int" `Quick (fun () ->
              check_ok "mod m { var x in 1..3; def y = x + 1; }");
          test_case "var decl bool" `Quick (fun () ->
              check_ok "mod m { var x = false; }");
          test_case "simple proc" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() { x = 1; return (); }\n\
                \  process ps = foo in 1..2;\n\
                \  }");
          test_case "proc with await" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var lock = false;\n\
                \  procedure foo() {\n\
                \    await lock == false, lock = true;\n\
                \    return ();\n\
                \  }\n\
                \  process ps = foo in 1..2;\n\
                \  }");
          test_case "proc with while and break" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() {\n\
                \    while (true) {\n\
                \      x = x + 1;\n\
                \      if (x == 10) { break; }\n\
                \    }\n\
                \    return ();\n\
                \  }\n\
                \  process ps = foo in 1..2;\n\
                \  }");
          test_case "proc with call" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var lock = false;\n\
                \  procedure acquire() {\n\
                \    await lock == false, lock = true;\n\
                \    return ();\n\
                \  }\n\
                \  procedure main() {\n\
                \    acquire();\n\
                \    return ();\n\
                \  }\n\
                \  process ps = main in 1..2;\n\
                \  }");
          test_case "proc call return value" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure fact(y) {\n\
                \    if (y == 0) { return 1; } else {\n\
                \      var ans = y * fact(y - 1);\n\
                \      return ans;\n\
                \    }\n\
                \  }\n\
                \  procedure main() { x = fact(5); return (); }\n\
                \  process ps = main in 1..1;\n\
                \  }");
          test_case "local var" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var g = 0;\n\
                \  procedure foo() {\n\
                \    var x = 5;\n\
                \    g = x;\n\
                \    return ();\n\
                \  }\n\
                \  process ps = foo in 1..2;\n\
                \  }");
          test_case "tuple return" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  procedure foo() { return (1, 2); }\n\
                \  process ps = foo in 1..2;\n\
                \  }");
          test_case "sequence literal" `Quick (fun () ->
              check_ok "mod m { def xs = [1, 2, 3]; }");
          test_case "map init and subscript" `Quick (fun () ->
              check_ok "mod m { var xs = { x in 1..3 -> 0 }; def y = xs[1]; }");
          test_case "self and continue" `Quick (fun () ->
              check_ok
                "mod m { var xs = { x in 1..2 -> 0 }; procedure foo() { while \
                 (true) { xs[self] = 1; continue; } return (); } process ps = \
                 foo in 1..2; }");
          test_case "if else" `Quick (fun () ->
              check_ok
                "mod m { procedure foo() { if (true) { ; } else { ; } return (); } \
                 process ps = foo in 1..1; }");
          test_case "sequence builtins" `Quick (fun () ->
              check_ok
                "mod m { def xs = [1, 2]; def y = head(xs); def zs = tail(xs); \
                 def ws = concat(xs, append([], 3)); }");
          test_case "tuple remains heterogeneous" `Quick (fun () ->
              check_ok "mod m { def x = (1, true); }");
          test_case "while wait" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() {\n\
                \    while (0 < x) {}\n\
                \    return ();\n\
                \  }\n\
                \  process ps = foo in 1..2;\n\
                \  }");
          test_case "empty step" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  procedure foo() { ; return (); }\n\
                \  process ps = foo in 1..2;\n\
                \  }");
          test_case "local var type fixed by first use" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var g = 0;\n\
                \  procedure foo() {\n\
                \    var xs = [];\n\
                \    xs = append(xs, 1);\n\
                \    g = head(xs);\n\
                \    return ();\n\
                \  }\n\
                \  process ps = foo in 1..1;\n\
                \  }");
          test_case "full rwlock" `Quick (fun () ->
              check_ok
                {|mod rwlock {
  def readerNum = 2;
  def writerNum = 2;
  def foo(x) = x + 1;
  var rcnt = 0;
  var wcnt = 0;
  var lock = false;
  procedure lockAcquire() {
    await lock == false, lock = true;
    return ();
  }
  procedure lockRelease() {
    lock = false;
    return ();
  }
  procedure rwlockReadAcquire() {
    while (true) {
      while (0 < wcnt) {}
      rcnt = rcnt + 1;
      if (wcnt == 0) { break; }
      rcnt = rcnt - 1;
    }
    return ();
  }
  procedure rwlockReadRelease() {
    rcnt = rcnt - 1;
    return ();
  }
  procedure rwlockWriteAcquire() {
    wcnt = wcnt + 1;
    while (0 < rcnt) {}
    lockAcquire();
    return ();
  }
  procedure rwlockWriteRelease() {
    lockRelease();
    wcnt = wcnt - 1;
    return ();
  }
  procedure reader() {
    while (true) {
      rwlockReadAcquire();
      ;
      rwlockReadRelease();
    }
  }
  procedure writer() {
    while (true) {
      rwlockWriteAcquire();
      ;
      rwlockWriteRelease();
    }
  }
  process readers = reader in 1..readerNum;
  process writers = writer in 1..writerNum;
}|});
        ] );
      ( "ill_typed",
        [
          test_case "add bool" `Quick (fun () ->
              check_fails "mod m { def x = 1 + true; }");
          test_case "compare bool" `Quick (fun () ->
              check_fails "mod m { def x = true < 1; }");
          test_case "unbound var" `Quick (fun () ->
              check_fails "mod m { def x = y; }");
          test_case "assign type mismatch" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() { x = true; return (); }\n\
                \  process ps = foo in 1..2;\n\
                \  }");
          test_case "assign to const" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  def x = 0;\n\
                \  procedure foo() { x = 1; return (); }\n\
                \  process ps = foo in 1..2;\n\
                \  }");
          test_case "unbound in proc" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  procedure foo() { y = 1; return (); }\n\
                \  process ps = foo in 1..2;\n\
                \  }");
          test_case "process range not int" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  procedure foo() { return (); }\n\
                \  process ps = foo in true..2;\n\
                \  }");
          test_case "await non-bool" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  procedure foo() { await 42; return (); }\n\
                \  process ps = foo in 1..2;\n\
                \  }");
          test_case "while non-bool" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  procedure foo() { while (42) { ; } return (); }\n\
                \  process ps = foo in 1..2;\n\
                \  }");
          test_case "unary minus bool" `Quick (fun () ->
              check_fails "mod m { def x = -true; }");
          test_case "logical not on int" `Quick (fun () ->
              check_fails "mod m { def x = !1; }");
          test_case "division on bool" `Quick (fun () ->
              check_fails "mod m { def x = true / false; }");
          test_case "modulo on bool" `Quick (fun () ->
              check_fails "mod m { def x = true % false; }");
          test_case "greater than on bool" `Quick (fun () ->
              check_fails "mod m { def x = true > false; }");
          test_case "inequality mismatch" `Quick (fun () ->
              check_fails "mod m { def x = 1 != true; }");
          test_case "procedure arity mismatch" `Quick (fun () ->
              check_fails "mod m { def f(x) = x + 1; def y = f(1, 2); }");
          test_case "heterogeneous sequence" `Quick (fun () ->
              check_fails "mod m { def x = [1, true]; }");
          test_case "head on tuple" `Quick (fun () ->
              check_fails "mod m { def x = head((1, 2)); }");
          test_case "len on int" `Quick (fun () ->
              check_fails "mod m { def x = len(1); }");
          test_case "if expression non-bool condition" `Quick (fun () ->
              check_fails "mod m { def x = if (1) { 2 } else { 3 }; }");
          test_case "var decl range non-int bounds" `Quick (fun () ->
              check_fails "mod m { var x in true..false; }");
          test_case "var decl range used as bool" `Quick (fun () ->
              check_fails "mod m { var x in 1..3; def p = x && true; }");
          test_case "builtin call before its shadowing def" `Quick (fun () ->
              (* head is still the builtin here: seq argument required *)
              check_fails
                "mod m { def y = head(2); def head(x) = x + 1; }");
          test_case "statement call to a def function" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  def f(x) = x;\n\
                \  procedure g() { f(1); return (); }\n\
                \  process p = g in 1..1;\n\
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
                \  process p = g in 1..1;\n\
                \  }");
          test_case "def function as process root" `Quick (fun () ->
              check_fails "mod m { def f(x) = x; process p = f in 1..1; }");
          test_case "non-bool property" `Quick (fun () ->
              check_fails "mod m { property p = 1; }");
          test_case "assert non-bool" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  procedure foo() { assert 1; return (); }\n\
                \  process ps = foo in 1..1;\n\
                \  }");
          test_case "nested subscript value mismatch" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var grid = { i in 1..2 -> { j in 1..2 -> 0 } };\n\
                \  procedure foo() { grid[1][2] = true; return (); }\n\
                \  process ps = foo in 1..1;\n\
                \  }");
          test_case "too many subscripts" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var xs = { i in 1..2 -> 0 };\n\
                \  procedure foo() { xs[1][2] = 3; return (); }\n\
                \  process ps = foo in 1..1;\n\
                \  }");
          test_case "with binder not assignable" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  procedure foo() { with (v in 1..3) { v = 1; } return (); }\n\
                \  process ps = foo in 1..1;\n\
                \  }");
          test_case "with bounds non-int" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() {\n\
                \    with (v in true..false) { x = 1; }\n\
                \    return ();\n\
                \  }\n\
                \  process ps = foo in 1..1;\n\
                \  }");
          test_case "with binder is int" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var b = false;\n\
                \  procedure foo() { with (v in 1..3) { b = v; } return (); }\n\
                \  process ps = foo in 1..1;\n\
                \  }");
          test_case "ill-typed either arm" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() {\n\
                \    either { x = true; } or { x = 2; }\n\
                \    return ();\n\
                \  }\n\
                \  process ps = foo in 1..1;\n\
                \  }");
          test_case "break in either arm outside loop" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  procedure foo() { either { break; } or { ; } return (); }\n\
                \  process ps = foo in 1..1;\n\
                \  }");
          test_case "quantifier body non-bool" `Quick (fun () ->
              check_fails "mod m { def x = forall (i in 1..2) { i }; }");
          test_case "quantifier bounds non-int" `Quick (fun () ->
              check_fails
                "mod m { def x = forall (i in true..false) { true }; }");
          test_case "quantifier binder is int" `Quick (fun () ->
              check_fails
                "mod m { def x = forall (i in 1..2) { i && true }; }");
          test_case "if expression branch mismatch" `Quick (fun () ->
              check_fails "mod m { def x = if (true) { 1 } else { false }; }");
          test_case "len on tuple" `Quick (fun () ->
              check_fails "mod m { def x = len((1, 2)); }");
          test_case "head on map" `Quick (fun () ->
              check_fails "mod m { def x = head({ i in 1..2 -> 0 }); }");
          test_case "continue outside loop" `Quick (fun () ->
              check_fails
                "mod m { procedure foo() { continue; return (); } process ps = foo in \
                 1..1; }");
          test_case "local var used at two types" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var g = 0;\n\
                \  procedure foo() {\n\
                \    var xs = [];\n\
                \    if (xs == [true]) { g = 1; }\n\
                \    xs = append(xs, 1);\n\
                \    return ();\n\
                \  }\n\
                \  process ps = foo in 1..1;\n\
                \  }");
        ] );
    ]
