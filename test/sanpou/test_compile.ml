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
                   fn foo() { x = 1 - x; }\n\
                   process ps = foo in 1..2;\n\
                   }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              has_not_in "WF_vars(foo(self))" tla);
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
                   fn main() { return (); }\n\
                   process ps = main in 1..1;\n\
                   }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              let has s = has_in s tla in
              has "xs == << 1, 2 >>";
              has "y == Head(xs)";
              has "ys == Tail(xs)";
              has "zs == Append(xs, 3)";
              has "ws == xs \\o << 4, 5 >>";
              has "n == Len(xs)");
          Alcotest.test_case "operators compile to tla equivalents" `Quick
            (fun () ->
              let ast =
                parse
                  "mod ops {\n\
                   def differs = [1] != [2];\n\
                   def either = true || false;\n\
                   def literal = -1;\n\
                   def expr = -(1 + 2);\n\
                   def bigger = 2 > 1;\n\
                   def quotient = 7 / 2;\n\
                   def remainder = 7 % 2;\n\
                   def negated = !true;\n\
                   fn main() { return (); }\n\
                   process ps = main in 1..1;\n\
                   }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              let has s = has_in s tla in
              has "differs == (<< 1 >> /= << 2 >>)";
              has "either == (TRUE \\/ FALSE)";
              has "literal == -1";
              has "expr == (0 - (1 + 2))";
              has "bigger == (2 > 1)";
              has "quotient == (7 \\div 2)";
              has "remainder == (7 % 2)";
              has "negated == ~(TRUE)");
          Alcotest.test_case
            "range-initialized var compiles to membership in Init" `Quick
            (fun () ->
              let ast =
                parse
                  "mod m {\n\
                   var x in 1..3;\n\
                   var y = 0;\n\
                   fn main() { y = x; return (); }\n\
                   process ps = main in 1..1;\n\
                   }\n"
              in
              let tla = compile ast |> List.hd |> Tla.Tla_printer.render in
              let has s = has_in s tla in
              has "x \\in 1..3";
              has "y = 0");
          Alcotest.test_case "quantifiers compile to tla quantifiers" `Quick
            (fun () ->
              let ast =
                parse
                  "mod m {\n\
                   def inv = forall (i in 1..3) { i < 4 };\n\
                   def can = exists (i in 1..3) { i == 2 };\n\
                   fn main() { return (); }\n\
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
                   fn foo() { x = if (x == 0) { 1 } else { x }; return (); }\n\
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
                   var xs = { i in 1..2: 0; };\n\
                   fn main() { xs[1] = self; return (); }\n\
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
                   fn f() { x = 1; return (); }\n\
                   process ps = f in 1..2;\n\
                   }\n\
                   mod b {\n\
                   var y = 0;\n\
                   fn g() { y = 2; return (); }\n\
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
                   fn foo() { var x = 5; g = x; return (); }\n\
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
                   fn foo() { x = 1 - x; }\n\
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
                   fn foo() {\n\
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
