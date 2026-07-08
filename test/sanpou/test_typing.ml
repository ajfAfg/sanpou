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
                \  process ps(self in 1..1) = foo;\n\
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
                \  process ps(self in 1..1) = foo;\n\
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
                \  process ps(self in 1..1) = f;\n\
                \  }");
          test_case "property is bool" `Quick (fun () ->
              check_ok
                "mod m { var x = 0; property p = finally(x == 1); }");
          test_case "constant process domains" `Quick (fun () ->
              (* defs of constants, comprehension binders shadowing a var,
                 and fun params shadowing a var are all constant *)
              check_ok
                "mod m {\n\
                \  var x = 1;\n\
                \  def k = 3;\n\
                \  def widen(x) = x + k;\n\
                \  procedure f() { return (); }\n\
                \  process p(self in { x in 1..widen(2) : x > 0 }) = f;\n\
                \  }");
          test_case "module-level names shadow sequentially" `Quick
            (fun () ->
              (* defs, vars, procedures, and process names all shadow
                 sequentially; alpha-conversion renames the shadowed ones *)
              check_ok
                {|mod m {
                    def a = 1;
                    def a = a == 1;
                    var x = 0;
                    procedure f() { x = 1; return (); }
                    var x = false;
                    procedure f() { x = true, f(); return (); }
                    process p(self in 1..1) = f;
                    process p(self in 2..2) = f;
                  }|});
          test_case "locals and params may shadow module-level names" `Quick
            (fun () ->
              (* only module-level duplicates are rejected; names in bodies
                 shadow as usual *)
              check_ok
                "mod m {\n\
                \  def a = true;\n\
                \  procedure f(a) { var b = a + 1; return (); }\n\
                \  procedure g() { f(1); return (); }\n\
                \  process ps(self in 1..1) = g;\n\
                \  }");
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
          test_case "assert statement" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() { assert x >= 0, x = x + 1; return (); }\n\
                \  process ps(self in 1..1) = foo;\n\
                \  }");
          test_case "string-keyed map with per-process state" `Quick
            (fun () ->
              (* the main use case for non-int process IDs: a state table
                 keyed by the process ID set *)
              check_ok
                {|mod m {
                    def ids = {"a", "b"};
                    var t = { x in ids -> 0 };
                    procedure f() { t[self] = t[self] + 1; return (); }
                    process p(self in ids) = f;
                  }|});
          test_case "atom-keyed map" `Quick (fun () ->
              check_ok
                {|mod m {
                    var t = { x in {`p, `q} -> 0 };
                    procedure f() { t[`p] = 1; return (); }
                    process p(self in 1..1) = f;
                  }|});
          test_case "nested subscript assignment" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var grid = { i in 1..2 -> { j in 1..2 -> 0 } };\n\
                \  procedure foo() { grid[1][2] = 5; return (); }\n\
                \  process ps(self in 1..1) = foo;\n\
                \  }");
          test_case "with statement" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() {\n\
                \    with (v in 1..3) { await v > x, x = v; }\n\
                \    return ();\n\
                \  }\n\
                \  process ps(self in 1..1) = foo;\n\
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
          test_case "either arms" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() {\n\
                \    either { x = 1; } or { x = 2; } or { await x > 0; }\n\
                \    return ();\n\
                \  }\n\
                \  process ps(self in 1..1) = foo;\n\
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
                \  process ps(self in 1..1) = foo;\n\
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
                \  process ps(self in 1..1) = foo;\n\
                \  }");
          test_case "len is int" `Quick (fun () ->
              check_ok "mod m { def x = len([1]) + 1; }");
          test_case "unary minus literal" `Quick (fun () ->
              check_ok "mod m { def x = -1; }");
          test_case "unary minus paren expr" `Quick (fun () ->
              check_ok "mod m { def x = -(1 + 2); }");
          test_case "unary binds looser than postfix" `Quick (fun () ->
              (* -s[1] negates the element, !r.f negates the field; parsing
                 them as (-s)[1] / (!r).f used to be a type error *)
              check_ok
                "mod m {\n\
                \  def s = [1, 2, 3];\n\
                \  def a = -s[1];\n\
                \  def r = {f: true};\n\
                \  def b = !r.f;\n\
                \  }");
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
                \  process ps(self in 1..2) = foo;\n\
                \  }");
          test_case "proc with await" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var lock = false;\n\
                \  procedure foo() {\n\
                \    await lock == false, lock = true;\n\
                \    return ();\n\
                \  }\n\
                \  process ps(self in 1..2) = foo;\n\
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
                \  process ps(self in 1..2) = foo;\n\
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
                \  process ps(self in 1..2) = main;\n\
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
                \  process ps(self in 1..1) = main;\n\
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
                \  process ps(self in 1..2) = foo;\n\
                \  }");
          test_case "tuple return" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  procedure foo() { return (1, 2); }\n\
                \  process ps(self in 1..2) = foo;\n\
                \  }");
          test_case "sequence literal" `Quick (fun () ->
              check_ok "mod m { def xs = [1, 2, 3]; }");
          test_case "map init and subscript" `Quick (fun () ->
              check_ok "mod m { var xs = { x in 1..3 -> 0 }; def y = xs[1]; }");
          test_case "self and continue" `Quick (fun () ->
              check_ok
                "mod m { var xs = { x in 1..2 -> 0 }; procedure foo() { while \
                 (true) { xs[self] = 1; continue; } return (); } process \
                 ps(self in 1..2) = foo; }");
          test_case "if else" `Quick (fun () ->
              check_ok
                "mod m { procedure foo() { if (true) { ; } else { ; } return (); } \
                 process ps(self in 1..1) = foo; }");
          test_case "sequence builtins" `Quick (fun () ->
              check_ok
                "mod m { def xs = [1, 2]; def y = head(xs); def zs = tail(xs); \
                 def ws = concat(xs, append([], 3)); }");
          test_case "tuple remains heterogeneous" `Quick (fun () ->
              check_ok "mod m { def x = (1, true); }");
          test_case "string literal" `Quick (fun () ->
              check_ok {|mod m { def x = "idle"; }|});
          test_case "string equality" `Quick (fun () ->
              check_ok {|mod m { def x = "idle" == "busy"; }|});
          test_case "string inequality" `Quick (fun () ->
              check_ok {|mod m { def x = "idle" != "busy"; }|});
          test_case "set of strings" `Quick (fun () ->
              check_ok {|mod m { def x = {"idle", "busy"}; def y = "idle" in x; }|});
          test_case "atom declaration and reference" `Quick (fun () ->
              check_ok "mod m { def x = `noValue; }");
          test_case "atom equality" `Quick (fun () ->
              check_ok "mod m { def x = `a == `b; def y = `a != `b; }");
          test_case "set of atoms and membership" `Quick (fun () ->
              check_ok "mod m { def s = {`r, `g, `b}; def m2 = `r in s; }");
          test_case "atom as record field" `Quick (fun () ->
              check_ok "mod m { def r = {phase: `idle, n: 0}; }");
          test_case "record literal" `Quick (fun () ->
              check_ok {|mod m { def r = {kind: "req", src: 1}; }|});
          test_case "field access" `Quick (fun () ->
              check_ok {|mod m { def r = {kind: "req", src: 1}; def k = r.kind; }|});
          test_case "field access type" `Quick (fun () ->
              check_ok {|mod m { def r = {n: 0}; def m2 = r.n + 1; }|});
          test_case "records unify regardless of field order" `Quick (fun () ->
              check_ok
                {|mod m { def x = if (true) { {a: 1, b: 2} } else { {b: 3, a: 4} }; }|});
          test_case "field update assignment" `Quick (fun () ->
              check_ok
                {|mod m {
                    var r = {kind: "idle", n: 0};
                    procedure f() { r.kind = "busy", r.n = r.n + 1; return (); }
                    process ps(self in 1..1) = f;
                  }|});
          test_case "record field in map, mixed path update" `Quick (fun () ->
              check_ok
                {|mod m {
                    var grid = { i in 1..2 -> {v: 0} };
                    procedure f() { grid[self].v = 5; return (); }
                    process ps(self in 1..2) = f;
                  }|});
          test_case "range is a set of ints" `Quick (fun () ->
              check_ok "mod m { def s = 1..3; def y = 1 in s; }");
          test_case "set literal homogeneous" `Quick (fun () ->
              check_ok "mod m { def s = {1, 2, 3}; }");
          test_case "empty set is polymorphic" `Quick (fun () ->
              check_ok "mod m { def s = {}; def b = true in {}; }");
          test_case "membership" `Quick (fun () ->
              check_ok "mod m { def b = 1 in {1, 2}; }");
          test_case "membership over bool set" `Quick (fun () ->
              check_ok "mod m { def b = true in {false, true}; }");
          test_case "set comprehension" `Quick (fun () ->
              check_ok "mod m { def s = { x in 1..3 : x > 1 }; def y = 2 in s; }");
          test_case "set comprehension over set literal" `Quick (fun () ->
              check_ok "mod m { def s = { x in {1, 2, 3} : x % 2 == 0 }; }");
          test_case "set operations" `Quick (fun () ->
              check_ok
                "mod m { def a = {1, 2}; def b = {2, 3};\n\
                \  def u = union(a, b); def i = intersection(a, b);\n\
                \  def d = difference(a, b); }");
          test_case "cardinality is int" `Quick (fun () ->
              check_ok "mod m { def n = cardinality({1, 2, 3}) + 1; }");
          test_case "subseteq is bool" `Quick (fun () ->
              check_ok "mod m { def b = subseteq({1}, {1, 2}) && true; }");
          test_case "forall over set literal" `Quick (fun () ->
              check_ok "mod m { def p = forall (x in {1, 2, 3}) { x > 0 }; }");
          test_case "var domain set" `Quick (fun () ->
              check_ok "mod m { var x in {1, 2, 3}; def y = x + 1; }");
          test_case "process over set" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  procedure foo() { return (); }\n\
                \  process ps(self in {1, 2, 3}) = foo;\n\
                \  }");
          test_case "process over a string id set; self is that type" `Quick
            (fun () ->
              check_ok
                {|mod m {
                    def clients = {"alice", "bob"};
                    var turn = "alice";
                    procedure client() { await turn == self; return (); }
                    process cs(self in clients) = client;
                  }|});
          test_case "process over an atom id set" `Quick (fun () ->
              check_ok
                {|mod m {
                    procedure f() { return (); }
                    process p(self in {`main}) = f;
                  }|});
          test_case "self as int when id set is a range" `Quick (fun () ->
              check_ok
                {|mod m {
                    var a = { i in 1..3 -> 0 };
                    procedure f() { a[self] = self; return (); }
                    process p(self in 1..3) = f;
                  }|});
          test_case "with over set" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() { with (v in {1, 2}) { x = v; } return (); }\n\
                \  process ps(self in 1..1) = foo;\n\
                \  }");
          test_case "while wait" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() {\n\
                \    while (0 < x) {}\n\
                \    return ();\n\
                \  }\n\
                \  process ps(self in 1..2) = foo;\n\
                \  }");
          test_case "empty step" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  procedure foo() { ; return (); }\n\
                \  process ps(self in 1..2) = foo;\n\
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
                \  process ps(self in 1..1) = foo;\n\
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
  process readers(self in 1..readerNum) = reader;
  process writers(self in 1..writerNum) = writer;
}|});
        ] );
      ( "ill_typed",
        [
          test_case "add bool" `Quick (fun () ->
              check_fails "mod m { def x = 1 + true; }");
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
          test_case "process root with parameters" `Quick (fun () ->
              (* the wrapper pushes no arguments, so params would start as
                 the null sentinel and crash TLC at runtime *)
              check_fails
                {|mod m {
                    var x = 0;
                    procedure f(n) { x = n + 1; return (); }
                    process p(self in 1..1) = f;
                  }|});
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
          test_case "duplicate module names" `Quick (fun () ->
              (* each module writes <name>.tla; a duplicate would silently
                 overwrite the previous module's output *)
              check_fails "mod m { def x = 1; } mod m { def y = 2; }");
          test_case "distinct module names" `Quick (fun () ->
              check_ok "mod a { def x = 1; } mod b { def y = 2; }");
          test_case "compare bool" `Quick (fun () ->
              check_fails "mod m { def x = true < 1; }");
          test_case "unbound var" `Quick (fun () ->
              check_fails "mod m { def x = y; }");
          test_case "assign type mismatch" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() { x = true; return (); }\n\
                \  process ps(self in 1..2) = foo;\n\
                \  }");
          test_case "assign to const" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  def x = 0;\n\
                \  procedure foo() { x = 1; return (); }\n\
                \  process ps(self in 1..2) = foo;\n\
                \  }");
          test_case "unbound in proc" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  procedure foo() { y = 1; return (); }\n\
                \  process ps(self in 1..2) = foo;\n\
                \  }");
          test_case "process range not int" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  procedure foo() { return (); }\n\
                \  process ps(self in true..2) = foo;\n\
                \  }");
          test_case "await non-bool" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  procedure foo() { await 42; return (); }\n\
                \  process ps(self in 1..2) = foo;\n\
                \  }");
          test_case "while non-bool" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  procedure foo() { while (42) { ; } return (); }\n\
                \  process ps(self in 1..2) = foo;\n\
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
          test_case "heterogeneous set literal" `Quick (fun () ->
              check_fails "mod m { def x = {1, true}; }");
          test_case "atom arithmetic" `Quick (fun () ->
              check_fails "mod m { def x = `a + 1; }");
          test_case "atom compared to int" `Quick (fun () ->
              check_fails "mod m { def x = `a == 1; }");
          test_case "atom ordering" `Quick (fun () ->
              check_fails "mod m { def x = `a < `b; }");
          test_case "set mixing atoms and ints" `Quick (fun () ->
              check_fails "mod m { def x = {`a, 1}; }");
          test_case "field access on non-record" `Quick (fun () ->
              check_fails "mod m { def x = 1; def y = x.f; }");
          test_case "unknown field" `Quick (fun () ->
              check_fails {|mod m { def r = {a: 1}; def y = r.b; }|});
          test_case "record field type mismatch" `Quick (fun () ->
              check_fails
                {|mod m {
                    var r = {n: 0};
                    procedure f() { r.n = true; return (); }
                    process ps(self in 1..1) = f;
                  }|});
          test_case "records with different field sets do not unify" `Quick
            (fun () ->
              check_fails
                {|mod m { def x = if (true) { {a: 1} } else { {a: 1, b: 2} }; }|});
          test_case "string arithmetic" `Quick (fun () ->
              check_fails {|mod m { def x = "a" + 1; }|});
          test_case "string ordering" `Quick (fun () ->
              check_fails {|mod m { def x = "a" < "b"; }|});
          test_case "string/int equality mismatch" `Quick (fun () ->
              check_fails {|mod m { def x = "a" == 1; }|});
          test_case "set mixing strings and ints" `Quick (fun () ->
              check_fails {|mod m { def x = {"a", 1}; }|});
          test_case "membership element/set mismatch" `Quick (fun () ->
              check_fails "mod m { def b = 1 in {true}; }");
          test_case "membership on non-set" `Quick (fun () ->
              check_fails "mod m { def b = 1 in [1, 2]; }");
          test_case "union on non-sets" `Quick (fun () ->
              check_fails "mod m { def x = union([1], [2]); }");
          test_case "union mismatched element types" `Quick (fun () ->
              check_fails "mod m { def x = union({1}, {true}); }");
          test_case "cardinality on non-set" `Quick (fun () ->
              check_fails "mod m { def x = cardinality([1, 2]); }");
          test_case "map subscript key type mismatch" `Quick (fun () ->
              check_fails
                {|mod m { def x = { i in {"a"} -> 0 }[1]; }|});
          test_case "map keys from mixed domains do not unify" `Quick
            (fun () ->
              check_fails
                {|mod m {
                    def x = { i in {"a"} -> 0 };
                    def y = { i in 1..2 -> 0 };
                    def eq = x == y;
                  }|});
          test_case "self used outside a procedure" `Quick (fun () ->
              check_fails "mod m { def d = self; }");
          test_case "self used at two id types" `Quick (fun () ->
              check_fails
                {|mod m {
                    procedure f() { var x = self + 1; return (); }
                    process p(self in {"a"}) = f;
                  }|});
          test_case "processes with different id types" `Quick (fun () ->
              check_fails
                {|mod m {
                    procedure f() { return (); }
                    process a(self in 1..2) = f;
                    process b(self in {"x"}) = f;
                  }|});
          test_case "process domain reads a var" `Quick (fun () ->
              check_fails
                {|mod m {
                    var n = 2;
                    procedure f() { n = n + 1; return (); }
                    process p(self in 1..n) = f;
                  }|});
          test_case "process domain reads a var through a def" `Quick
            (fun () ->
              check_fails
                {|mod m {
                    var n = 2;
                    def d = n + 1;
                    procedure f() { return (); }
                    process p(self in 1..d) = f;
                  }|});
          test_case "self type constrained through a procedure parameter"
            `Quick (fun () ->
              (* f's parameter unifies with self, so f's scheme must keep the
                 module-shared self type monomorphic; generalizing it would
                 let the int argument coexist with a string id set. *)
              check_fails
                {|mod m {
                    var ok = 0;
                    procedure f(x) { await x == self, ok = 1; return (); }
                    procedure g() { f(1); return (); }
                    fair process p(self in {"a"}) = g;
                  }|});
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
          test_case "label lookalikes are not reserved" `Quick (fun () ->
              check_ok "mod m { def L = 1; def L3x = 2; def l3 = 3; }");
          test_case "comprehension predicate non-bool" `Quick (fun () ->
              check_fails "mod m { def s = { x in 1..3 : x }; }");
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
          test_case "non-bool property" `Quick (fun () ->
              check_fails "mod m { property p = 1; }");
          test_case "assert non-bool" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  procedure foo() { assert 1; return (); }\n\
                \  process ps(self in 1..1) = foo;\n\
                \  }");
          test_case "nested subscript value mismatch" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var grid = { i in 1..2 -> { j in 1..2 -> 0 } };\n\
                \  procedure foo() { grid[1][2] = true; return (); }\n\
                \  process ps(self in 1..1) = foo;\n\
                \  }");
          test_case "too many subscripts" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var xs = { i in 1..2 -> 0 };\n\
                \  procedure foo() { xs[1][2] = 3; return (); }\n\
                \  process ps(self in 1..1) = foo;\n\
                \  }");
          test_case "with binder not assignable" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  procedure foo() { with (v in 1..3) { v = 1; } return (); }\n\
                \  process ps(self in 1..1) = foo;\n\
                \  }");
          test_case "with bounds non-int" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() {\n\
                \    with (v in true..false) { x = 1; }\n\
                \    return ();\n\
                \  }\n\
                \  process ps(self in 1..1) = foo;\n\
                \  }");
          test_case "with binder is int" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var b = false;\n\
                \  procedure foo() { with (v in 1..3) { b = v; } return (); }\n\
                \  process ps(self in 1..1) = foo;\n\
                \  }");
          test_case "ill-typed either arm" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  var x = 0;\n\
                \  procedure foo() {\n\
                \    either { x = true; } or { x = 2; }\n\
                \    return ();\n\
                \  }\n\
                \  process ps(self in 1..1) = foo;\n\
                \  }");
          test_case "break in either arm outside loop" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  procedure foo() { either { break; } or { ; } return (); }\n\
                \  process ps(self in 1..1) = foo;\n\
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
                "mod m { procedure foo() { continue; return (); } process \
                 ps(self in 1..1) = foo; }");
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
                \  process ps(self in 1..1) = foo;\n\
                \  }");
        ] );
    ]
