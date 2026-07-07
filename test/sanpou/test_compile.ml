(* Emitter specifics that the cram goldens (test/cram/compile.t) do not pin
   down. Whole-program output, the process wrapper, frame-resident locals and
   parameters, and CLI diagnostics are covered by the cram tests. *)

let parse input =
  input |> Lexing.from_string |> Sanpou.Parser.program Sanpou.Lexer.main

let compile ast =
  Sanpou.Typing.check ast;
  ast |> Sanpou.Alpha_convert.transform |> Sanpou.Normalize_calls.normalize
  |> Sanpou.Linearize.linearize
  |> List.map Sanpou.Emit_tla.generate_module

let has_in s tla =
  Alcotest.(check bool)
    s true
    (try
       let _ = Str.search_forward (Str.regexp_string s) tla 0 in
       true
     with Not_found -> false)

let has_not_in s tla =
  Alcotest.(check bool)
    s false
    (try
       let _ = Str.search_forward (Str.regexp_string s) tla 0 in
       true
     with Not_found -> false)

let () =
  let open Alcotest in
  run "Codegen"
    [
      ( "codegen",
        [
          Alcotest.test_case "plain process omits weak fairness" `Quick
            (fun () ->
              let ast =
                parse
                  "mod foo {\n\
                   var x = 0;\n\
                   procedure foo() { x = 1 - x; }\n\
                   process ps = foo in 1..2;\n\
                   }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              has_not_in "WF_vars(foo(self))" tla);
          Alcotest.test_case "fair+ process emits strong fairness" `Quick
            (fun () ->
              let ast =
                parse
                  "mod foo {\n\
                   var x = 0;\n\
                   procedure foo() { x = 1 - x; }\n\
                   fair+ process ps = foo in 1..2;\n\
                   }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              has_in "SF_vars(foo(self))" tla;
              has_not_in "WF_vars" tla);
          Alcotest.test_case "sequence builtins compile to tla sequences" `Quick
            (fun () ->
              let ast =
                parse
                  "mod seqs {\n\
                   def xs = [1, 2];\n\
                   def y = head(xs);\n\
                   def ys = tail(xs);\n\
                   def zs = append(xs, 3);\n\
                   def ws = concat(xs, [4, 5]);\n\
                   def n = len(xs);\n\
                   procedure main() { return (); }\n\
                   process ps = main in 1..1;\n\
                   }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              let has s = has_in s tla in
              has "xs == << 1, 2 >>";
              has "y == Head(xs)";
              has "ys == Tail(xs)";
              has "zs == Append(xs, 3)";
              has "ws == (xs \\o << 4, 5 >>)";
              has "n == Len(xs)");
          Alcotest.test_case "subscripting a concat result parenthesizes"
            `Quick (fun () ->
              let ast =
                parse
                  "mod m {\n\
                   def a = [1, 2];\n\
                   def b = [3, 4];\n\
                   def first = concat(a, b)[1];\n\
                   procedure main() { return (); }\n\
                   process ps = main in 1..1;\n\
                   }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              (* a \o b[1] would be read by TLA+ as a \o (b[1]) *)
              has_in "first == (a \\o b)[1]" tla);
          Alcotest.test_case "operators compile to tla equivalents" `Quick
            (fun () ->
              let ast =
                parse
                  "mod ops {\n\
                   def differs = [1] != [2];\n\
                   def disjoined = true || false;\n\
                   def literal = -1;\n\
                   def expr = -(1 + 2);\n\
                   def bigger = 2 > 1;\n\
                   def quotient = 7 / 2;\n\
                   def remainder = 7 % 2;\n\
                   def negated = !true;\n\
                   procedure main() { return (); }\n\
                   process ps = main in 1..1;\n\
                   }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              let has s = has_in s tla in
              has "differs == (<< 1 >> /= << 2 >>)";
              has "disjoined == (TRUE \\/ FALSE)";
              has "literal == -1";
              has "expr == (0 - (1 + 2))";
              has "bigger == (2 > 1)";
              has "quotient == (7 \\div 2)";
              has "remainder == (7 % 2)";
              has "negated == ~(TRUE)");
          Alcotest.test_case "string literals compile to tla strings" `Quick
            (fun () ->
              let ast =
                parse
                  "mod strs {\n\
                  \  def idle = \"idle\";\n\
                  \  def tags = {\"idle\", \"busy\"};\n\
                  \  var s = \"idle\";\n\
                  \  procedure main() { s = \"busy\"; return (); }\n\
                  \  process ps = main in 1..1;\n\
                  \  }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              let has s = has_in s tla in
              has "idle == \"idle\"";
              has "tags == {\"idle\", \"busy\"}";
              has "s = \"idle\"";
              has "s' = \"busy\"");
          Alcotest.test_case "model values compile to tla constants" `Quick
            (fun () ->
              let ast =
                parse
                  "mod mv {\n\
                  \  atom NoValue;\n\
                  \  atom Red, Green;\n\
                  \  def missing = NoValue;\n\
                  \  def colors = {Red, Green};\n\
                  \  procedure main() { return (); }\n\
                  \  process ps = main in 1..1;\n\
                  \  }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              let has s = has_in s tla in
              has "CONSTANT NoValue, Red, Green";
              has "missing == NoValue";
              has "colors == {Red, Green}");
          Alcotest.test_case "user atoms and the null sentinel coexist" `Quick
            (fun () ->
              (* the recursive proc uses defaultInitValue for unbound frame
                 fields; the user atom is listed alongside it *)
              let ast =
                parse
                  "mod m {\n\
                  \  atom A;\n\
                  \  var g = A;\n\
                  \  procedure f(n) { if (n == 0) { return 1; } else { var r = f(n - 1); return r; } }\n\
                  \  procedure main() { g = A; return (); }\n\
                  \  process ps = main in 1..1;\n\
                  \  }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              has_in "CONSTANT A, defaultInitValue" tla);
          Alcotest.test_case "records compile to tla records" `Quick (fun () ->
              let ast =
                parse
                  "mod recs {\n\
                  \  def msg = {kind: \"req\", src: 1};\n\
                  \  def k = msg.kind;\n\
                  \  var r = {tag: \"idle\", n: 0};\n\
                  \  procedure main() { r.tag = \"busy\", r.n = r.n + 1; return (); }\n\
                  \  process ps = main in 1..1;\n\
                  \  }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              let has s = has_in s tla in
              (* fields are emitted label-sorted *)
              has "msg == [kind |-> \"req\", src |-> 1]";
              has "k == msg.kind";
              has "r = [n |-> 0, tag |-> \"idle\"]";
              (* both field writes to r merge into a single EXCEPT conjunct *)
              has "r' = [r EXCEPT !.tag = \"busy\", !.n = (r.n + 1)]");
          Alcotest.test_case "field update on a mapped record uses EXCEPT path"
            `Quick (fun () ->
              let ast =
                parse
                  "mod g {\n\
                  \  var grid = { i in 1..2 -> {v: 0} };\n\
                  \  procedure main() { grid[self].v = 5; return (); }\n\
                  \  process ps = main in 1..2;\n\
                  \  }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              has_in "grid' = [grid EXCEPT ![self].v = 5]" tla);
          Alcotest.test_case "set literals, membership and comprehension" `Quick
            (fun () ->
              let ast =
                parse
                  "mod sets {\n\
                   def a = {1, 2, 3};\n\
                   def empty = {};\n\
                   def evens = { x in a : x % 2 == 0 };\n\
                   def mem = 1 in a;\n\
                   def rng = 1..3;\n\
                   procedure main() { return (); }\n\
                   process ps = main in 1..1;\n\
                   }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              let has s = has_in s tla in
              has "a == {1, 2, 3}";
              has "empty == {}";
              has "evens == {x \\in a : ((x % 2) = 0)}";
              has "mem == (1 \\in a)";
              has "rng == 1..3";
              (* no cardinality here, so FiniteSets is not extended *)
              has_not_in "FiniteSets" tla);
          Alcotest.test_case "set operations compile to tla equivalents" `Quick
            (fun () ->
              let ast =
                parse
                  "mod ops {\n\
                   def a = {1, 2};\n\
                   def b = {2, 3};\n\
                   def u = union(a, b);\n\
                   def i = intersection(a, b);\n\
                   def d = difference(a, b);\n\
                   def n = cardinality(a);\n\
                   def sub = subseteq(a, b);\n\
                   procedure main() { return (); }\n\
                   process ps = main in 1..1;\n\
                   }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              let has s = has_in s tla in
              has "u == (a \\cup b)";
              has "i == (a \\cap b)";
              has "d == (a \\ b)";
              has "n == Cardinality(a)";
              has "sub == (a \\subseteq b)";
              (* cardinality pulls in FiniteSets *)
              has "EXTENDS TLC, Sequences, Integers, FiniteSets");
          Alcotest.test_case "process over a string id set" `Quick (fun () ->
              let ast =
                parse
                  "mod m {\n\
                  \  def clients = {\"a\", \"b\"};\n\
                  \  var turn = \"a\";\n\
                  \  procedure client() { turn = self; return (); }\n\
                  \  process cs = client in clients;\n\
                  \  }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              let has s = has_in s tla in
              has "ProcSet == (clients)";
              (* self flows through as the process id; the assignment reads it *)
              has "turn' = self");
          Alcotest.test_case "process over a set literal" `Quick (fun () ->
              let ast =
                parse
                  "mod m {\n\
                   procedure main() { return (); }\n\
                   process ps = main in {1, 2, 3};\n\
                   }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              let has s = has_in s tla in
              has "ProcSet == ({1, 2, 3})";
              has "\\E self \\in {1, 2, 3}");
          Alcotest.test_case
            "range-initialized var compiles to membership in Init" `Quick
            (fun () ->
              let ast =
                parse
                  "mod m {\n\
                   var x in 1..3;\n\
                   var y = 0;\n\
                   procedure main() { y = x; return (); }\n\
                   process ps = main in 1..1;\n\
                   }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              let has s = has_in s tla in
              has "x \\in 1..3";
              has "y = 0");
          Alcotest.test_case "defs shadow builtins from their point onward"
            `Quick (fun () ->
              let ast =
                parse
                  "mod m {\n\
                   def a = head([1, 2]);\n\
                   def head(x) = x + 1;\n\
                   def b = head(2);\n\
                   procedure main() { return (); }\n\
                   process ps = main in 1..1;\n\
                   }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              let has s = has_in s tla in
              has "a == Head(<< 1, 2 >>)";
              has "b == head(2)");
          Alcotest.test_case "assert compiles to TLC Assert with location"
            `Quick (fun () ->
              let ast =
                parse
                  "mod m {\n\
                   var x = 0;\n\
                   procedure foo() { assert x >= 0, x = x + 1; return (); }\n\
                   process ps = foo in 1..1;\n\
                   }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              has_in
                "Assert((x >= 0), \"assertion failed at line 3, col 19: x >= \
                 0\")"
                tla);
          Alcotest.test_case "nested subscript assignment compiles to EXCEPT"
            `Quick (fun () ->
              let ast =
                parse
                  "mod m {\n\
                   var grid = { i in 1..2 -> { j in 1..2 -> 0 } };\n\
                   procedure foo() { grid[1][2] = 5; return (); }\n\
                   process ps = foo in 1..1;\n\
                   }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              has_in "grid' = [grid EXCEPT ![1][2] = 5]" tla);
          Alcotest.test_case
            "with compiles to an existential over the range" `Quick
            (fun () ->
              let ast =
                parse
                  "mod m {\n\
                   var x = 0;\n\
                   procedure foo() {\n\
                   with (v in 1..3) { await v > x, x = v; }\n\
                   return ();\n\
                   }\n\
                   process ps = foo in 1..1;\n\
                   }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              let has s = has_in s tla in
              has "\\E v__1 \\in 1..3: /\\ (v__1 > x)";
              has "x' = v__1");
          Alcotest.test_case
            "either compiles to a disjunction of guarded arms" `Quick
            (fun () ->
              let ast =
                parse
                  "mod m {\n\
                   var x = 0;\n\
                   procedure foo() {\n\
                   either { x = 1; } or { await x > 0, x = 2; }\n\
                   return ();\n\
                   }\n\
                   process ps = foo in 1..1;\n\
                   }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              let has s = has_in s tla in
              has "x' = 1";
              has "(x > 0)";
              has "x' = 2";
              has "\\/ /\\ x' = 1");
          Alcotest.test_case "quantifiers compile to tla quantifiers" `Quick
            (fun () ->
              let ast =
                parse
                  "mod m {\n\
                   def inv = forall (i in 1..3) { i < 4 };\n\
                   def can = exists (i in 1..3) { i == 2 };\n\
                   procedure main() { return (); }\n\
                   process ps = main in 1..1;\n\
                   }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              let has s = has_in s tla in
              has "inv == (\\A i \\in 1..3: (i < 4))";
              has "can == (\\E i \\in 1..3: (i = 2))");
          Alcotest.test_case
            "if expression compiles to atomic conditional update" `Quick
            (fun () ->
              let ast =
                parse
                  "mod m {\n\
                   var x = 0;\n\
                   procedure foo() { x = if (x == 0) { 1 } else { x }; return (); }\n\
                   process ps = foo in 1..1;\n\
                   }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              has_in "x' = (IF (x = 0) THEN 1 ELSE x)" tla);
          Alcotest.test_case "map init and indexed update compile" `Quick
            (fun () ->
              let ast =
                parse
                  "mod maps {\n\
                   var xs = { i in 1..2 -> 0 };\n\
                   procedure main() { xs[1] = self; return (); }\n\
                   process ps = main in 1..2;\n\
                   }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              has_in "xs = [i \\in 1..2 |-> 0]" tla;
              has_in "xs' = [xs EXCEPT ![1] = self]" tla);
          Alcotest.test_case "multiple modules" `Quick (fun () ->
              let ast =
                parse
                  "mod a {\n\
                   var x = 0;\n\
                   procedure f() { x = 1; return (); }\n\
                   process ps = f in 1..2;\n\
                   }\n\
                   mod b {\n\
                   var y = 0;\n\
                   procedure g() { y = 2; return (); }\n\
                   process qs = g in 1..3;\n\
                   }\n"
              in
              let modules = compile ast in
              Alcotest.(check int) "two modules" 2 (List.length modules);
              let tla_a = Tla.Tla_printer.render (List.nth modules 0) in
              let tla_b = Tla.Tla_printer.render (List.nth modules 1) in
              has_in "---- MODULE a ----" tla_a;
              has_in "VARIABLES pc, x, stack" tla_a;
              has_in "---- MODULE b ----" tla_b;
              has_in "VARIABLES pc, y, stack" tla_b);
          Alcotest.test_case "frame sentinel is a declared model-value constant"
            `Quick (fun () ->
              (* A string sentinel would crash TLC's liveness checking when
                 compared with a user value; the constant is assigned a model
                 value in the cfg instead *)
              let ast =
                parse
                  "mod m {\n\
                   var g = 0;\n\
                   procedure foo() { var x = 5; g = x; return (); }\n\
                   process ps = foo in 1..2;\n\
                   }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              has_in "CONSTANT defaultInitValue" tla;
              has_in "x__1 |-> defaultInitValue" tla;
              has_not_in "__null__" tla);
          Alcotest.test_case "no unbound frame fields, no constant" `Quick
            (fun () ->
              let ast =
                parse
                  "mod m {\n\
                   var x = 0;\n\
                   procedure foo() { x = 1 - x; }\n\
                   process ps = foo in 1..2;\n\
                   }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              has_not_in "CONSTANT" tla;
              has_not_in "defaultInitValue" tla);
          Alcotest.test_case "nested shadowing" `Quick (fun () ->
              let ast =
                parse
                  "mod m {\n\
                   var g = 0;\n\
                   procedure foo() {\n\
                   var x = 1;\n\
                   while (true) {\n\
                   var x = 2;\n\
                   g = x;\n\
                   break;\n\
                   }\n\
                   g = x;\n\
                   return ();\n\
                   }\n\
                   process ps = foo in 1..2;\n\
                   }\n"
              in
              let modules = compile ast in
              let tla = Tla.Tla_printer.render (List.hd modules) in
              let has s = has_in s tla in
              has "VARIABLES pc, g, stack";
              has "![1].x__1 = 1";
              has "![1].x__2 = 2";
              has "g' = Head(stack[self]).x__2";
              has "g' = Head(stack[self]).x__1");
        ] );
    ]
