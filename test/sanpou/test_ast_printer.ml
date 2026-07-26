let parse input =
  input |> Lexing.from_string |> Sanpou.Parser.program Sanpou.Lexer.main

(* The AST carries no layout, so printing cannot reproduce the original text;
   instead pretty-printing then re-parsing must yield the same tree. This also
   exercises the printer's parenthesization: a missing paren would reassociate
   the reparsed tree. *)
let pretty_roundtrip input =
  let ast = parse input in
  let printed = Sanpou.Ast_printer.print_pretty ast in
  let reparsed = parse printed in
  Alcotest.(check bool)
    ("pretty roundtrip of: " ^ printed)
    true
    (Sanpou.Generic_ast.equal_program Sanpou.Generic_ast.equal_id
       Sanpou.Generic_ast.equal_id ast reparsed)

let pretty_prints input expected =
  Alcotest.(check string)
    "pretty print" expected
    (Sanpou.Ast_printer.print_pretty (parse input))

let () =
  let open Alcotest in
  run "AstPrinter"
    [
      ( "pretty_roundtrip",
        [
          test_case "const_def" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = 42; }");
          test_case "const_def expr" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = (1 + 2) * 3; }");
          test_case "fun_def" `Quick (fun () ->
              pretty_roundtrip "mod m { def foo(x, y) = x + y; }");
          test_case "var_decl" `Quick (fun () ->
              pretty_roundtrip "mod m { var x = 0; }");
          test_case "property" `Quick (fun () ->
              pretty_roundtrip
                "mod m { property p = globally(finally(x == 0)); }");
          test_case "var_decl range" `Quick (fun () ->
              pretty_roundtrip "mod m { var x in 1..3; }");
          test_case "proc simple" `Quick (fun () ->
              pretty_roundtrip "mod m { procedure foo() { return (); } }");
          test_case "proc assign" `Quick (fun () ->
              pretty_roundtrip "mod m { procedure foo() { x = 1; return (); } }");
          test_case "skip step" `Quick (fun () ->
              pretty_roundtrip "mod m { procedure foo() { ; } }");
          test_case "atomic step" `Quick (fun () ->
              pretty_roundtrip
                "mod m { procedure foo() { await lock == false, lock = true; } \
                 }");
          test_case "while body" `Quick (fun () ->
              pretty_roundtrip
                "mod m { procedure foo() { while (true) { break; } } }");
          test_case "while wait" `Quick (fun () ->
              pretty_roundtrip "mod m { procedure foo() { while (0 < x){} } }");
          test_case "if" `Quick (fun () ->
              pretty_roundtrip
                "mod m { procedure foo() { if (x == 0) { break; } } }");
          test_case "if else" `Quick (fun () ->
              pretty_roundtrip
                "mod m { procedure foo() { if (x == 0) { break; } else { \
                 continue; } } }");
          test_case "else if chain" `Quick (fun () ->
              pretty_roundtrip
                "mod m { procedure foo() { if (x == 0) { break; } else if (x \
                 == 1) { continue; } else { break; } } }");
          test_case "else if without final else" `Quick (fun () ->
              pretty_roundtrip
                "mod m { procedure foo() { if (x == 0) { break; } else if (x \
                 == 1) { continue; } } }");
          test_case "inequality" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = 1 != 2; }");
          test_case "greater than" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = 2 > 1; }");
          test_case "division and modulo" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = 7 / 2 % 3; }");
          test_case "division of sum" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = (1 + 2) / 3; }");
          test_case "logical not" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = !true; }");
          test_case "logical not of compound operand" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = !(a && b); }");
          test_case "forall" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = forall (i in 1..3) { i < 4 }; }");
          test_case "exists with compound body" `Quick (fun () ->
              pretty_roundtrip
                "mod m { def x = exists (i in 1..3) { p && q }; }");
          test_case "quantifier as operand" `Quick (fun () ->
              pretty_roundtrip
                "mod m { def x = forall (i in 1..2) { p } && q; }");
          test_case "nested quantifiers" `Quick (fun () ->
              pretty_roundtrip
                "mod m { def x = forall (i in 1..2) { exists (j in 1..2) { i \
                 == j } }; }");
          test_case "with statement" `Quick (fun () ->
              pretty_roundtrip
                "mod m { procedure foo() { with (v in 1..3) { x = v; } } }");
          test_case "assert statement" `Quick (fun () ->
              pretty_roundtrip
                "mod m { procedure foo() { assert x > 0, x = x + 1; } }");
          test_case "nested subscript assignment" `Quick (fun () ->
              pretty_roundtrip
                "mod m { procedure foo() { grid[i][j + 1] = 5; } }");
          test_case "with statement multiple stmts" `Quick (fun () ->
              pretty_roundtrip
                "mod m { procedure foo() { with (v in 1..n) { await v > x, x = \
                 v; } } }");
          test_case "with inside either arm" `Quick (fun () ->
              pretty_roundtrip
                "mod m { procedure foo() { either { with (v in 1..2) { x = v; \
                 } } or { x = 0; } } }");
          test_case "either two arms" `Quick (fun () ->
              pretty_roundtrip
                "mod m { procedure foo() { either { x = 1; } or { x = 2; } } }");
          test_case "either three arms" `Quick (fun () ->
              pretty_roundtrip
                "mod m { procedure foo() { either { x = 1; } or { x = 2; } or \
                 { ; } } }");
          test_case "either nested" `Quick (fun () ->
              pretty_roundtrip
                "mod m { procedure foo() { either { either { x = 1; } or { x = \
                 2; } } or { x = 3; } } }");
          test_case "if expression" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = if (a) { 1 } else { 2 }; }");
          test_case "if expression as operand" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = if (a) { 1 } else { 2 } + 3; }");
          test_case "if expression nested" `Quick (fun () ->
              pretty_roundtrip
                "mod m { def x = if (a) { if (b) { 1 } else { 2 } } else { 3 \
                 }; }");
          test_case "if expression in assignment" `Quick (fun () ->
              pretty_roundtrip
                "mod m { procedure foo() { x = if (x == 0) { 1 } else { x }; } \
                 }");
          test_case "unary minus literal" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = -1; }");
          test_case "unary minus paren expr" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = -(1 + 2); }");
          test_case "subtract negative" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = y - -1; }");
          test_case "tuple empty" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = (); }");
          test_case "tuple single" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = (1,); }");
          test_case "tuple pair" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = (1, 2); }");
          test_case "tuple nested" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = ((1, 2), 3); }");
          test_case "sequence empty" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = []; }");
          test_case "sequence single" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = [1]; }");
          test_case "sequence single trailing comma" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = [1,]; }");
          test_case "sequence pair" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = [1, 2]; }");
          test_case "string literal" `Quick (fun () ->
              pretty_roundtrip {|mod m { def x = "idle"; }|});
          test_case "string equality" `Quick (fun () ->
              pretty_roundtrip {|mod m { def x = s == "busy"; }|});
          test_case "set of strings" `Quick (fun () ->
              pretty_roundtrip {|mod m { def x = {"idle", "busy"}; }|});
          test_case "atom literal" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = `noValue; }");
          test_case "atoms in a set" `Quick (fun () ->
              pretty_roundtrip "mod m { def s = {`red, `green}; }");
          test_case "record literal" `Quick (fun () ->
              pretty_roundtrip {|mod m { def x = {kind: "req", src: 1}; }|});
          test_case "record single field" `Quick (fun () ->
              pretty_roundtrip {|mod m { def x = {n: 0}; }|});
          test_case "field access" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = m.kind; }");
          test_case "nested field access" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = a.b.c; }");
          test_case "field access on subscript" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = grid[i].tag; }");
          test_case "field update assignment" `Quick (fun () ->
              pretty_roundtrip {|mod m { procedure f() { m.kind = "busy"; } }|});
          test_case "mixed index and field assignment" `Quick (fun () ->
              pretty_roundtrip "mod m { procedure f() { a[i].f[j] = 5; } }");
          test_case "map init" `Quick (fun () ->
              pretty_roundtrip "mod m { var xs = { x in 1..2 -> false }; }");
          test_case "map init over set literal" `Quick (fun () ->
              pretty_roundtrip "mod m { var xs = { x in {1, 2} -> false }; }");
          test_case "range as set" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = 1..3; }");
          test_case "range of compound bounds" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = k + 1..n - 1; }");
          test_case "set literal empty" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = {}; }");
          test_case "set literal single" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = {1}; }");
          test_case "set literal pair" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = {1, 2}; }");
          test_case "membership" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = 1 in {1, 2}; }");
          test_case "membership over range" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = i in 1..n; }");
          test_case "membership as operand" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = 1 in s || b; }");
          test_case "set comprehension" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = { y in 1..3 : y > 1 }; }");
          test_case "set comprehension over set literal" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = { y in {1, 2, 3} : y > 1 }; }");
          test_case "set builtins" `Quick (fun () ->
              pretty_roundtrip
                "mod m { def x = union(intersection(a, b), difference(a, b)); }");
          test_case "cardinality and subseteq" `Quick (fun () ->
              pretty_roundtrip
                "mod m { def x = cardinality(a) > 0 && subseteq(a, b); }");
          test_case "var domain set literal" `Quick (fun () ->
              pretty_roundtrip "mod m { var x in {1, 2, 3}; }");
          test_case "process over set literal" `Quick (fun () ->
              pretty_roundtrip "mod m { process ps(self in {1, 2}) = foo; }");
          test_case "with over set" `Quick (fun () ->
              pretty_roundtrip
                "mod m { procedure foo() { with (v in {1, 2}) { x = v; } } }");
          test_case "subscript" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = xs[1 + 2]; }");
          test_case "process" `Quick (fun () ->
              pretty_roundtrip "mod m { process ps(self in 1..n) = foo; }");
          test_case "fair process" `Quick (fun () ->
              pretty_roundtrip "mod m { fair process ps(self in 1..n) = foo; }");
          test_case "strongly fair process" `Quick (fun () ->
              pretty_roundtrip "mod m { fair+ process ps(self in 1..n) = foo; }");
          test_case "call stmt" `Quick (fun () ->
              pretty_roundtrip "mod m { procedure foo() { bar(1, 2); } }");
          test_case "app expr" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = foo(1, 2); }");
          test_case "var step" `Quick (fun () ->
              pretty_roundtrip "mod m { procedure foo() { var x = 5; } }");
          test_case "var step expr" `Quick (fun () ->
              pretty_roundtrip
                "mod m { procedure foo() { var y = 1 + 2; return (); } }");
          test_case "multiple modules" `Quick (fun () ->
              pretty_roundtrip "mod a { var x = 0; }\nmod b { var y = 1; }");
          test_case "full module" `Quick (fun () ->
              pretty_roundtrip
                {|mod rwlock {
  def readerNum = 2;
    var rcnt = 0;
    var lock = false;
  procedure lockAcquire() {
    await lock == false, lock = true;
    return ();
  }
  procedure reader() {
    while (true) {
      lockAcquire();
      ;
      rcnt = rcnt - 1;
    }
  }
  process readers(self in 1..readerNum) = reader;
}|});
        ] );
      ( "parenthesization",
        [
          test_case "needed parens are reinserted" `Quick (fun () ->
              pretty_prints "mod m { def x = (1 + 2) * 3; }"
                "mod m {\n  def x = (1 + 2) * 3;\n}\n");
          test_case "redundant parens are dropped" `Quick (fun () ->
              pretty_prints "mod m { def x = ((1)) + (2); }"
                "mod m {\n  def x = 1 + 2;\n}\n");
          test_case "right operand of same precedence" `Quick (fun () ->
              pretty_prints "mod m { def x = 1 - (2 - 3); }"
                "mod m {\n  def x = 1 - (2 - 3);\n}\n");
          test_case "left-assoc chain stays flat" `Quick (fun () ->
              pretty_prints "mod m { def x = 1 - 2 - 3; }"
                "mod m {\n  def x = 1 - 2 - 3;\n}\n");
          test_case "unary minus over compound operand" `Quick (fun () ->
              pretty_prints "mod m { def x = -(1 + 2); }"
                "mod m {\n  def x = -(1 + 2);\n}\n");
          test_case "mixed and/or" `Quick (fun () ->
              pretty_prints "mod m { def x = (a || b) && c; }"
                "mod m {\n  def x = (a || b) && c;\n}\n");
          test_case "range binds looser than arithmetic" `Quick (fun () ->
              pretty_prints "mod m { def x = 1..n - 1; }"
                "mod m {\n  def x = 1..n - 1;\n}\n");
          test_case "membership binds looser than range" `Quick (fun () ->
              pretty_prints "mod m { def x = i in 1..n; }"
                "mod m {\n  def x = i in 1..n;\n}\n");
        ] );
      ( "pretty_print",
        [
          test_case "set literal" `Quick (fun () ->
              pretty_prints "mod m { def x = {1, 2}; }"
                "mod m {\n  def x = {1, 2};\n}\n");
          test_case "empty set" `Quick (fun () ->
              pretty_prints "mod m { def x = {}; }"
                "mod m {\n  def x = {};\n}\n");
          test_case "string literal" `Quick (fun () ->
              pretty_prints {|mod m { def x = "idle"; }|}
                "mod m {\n  def x = \"idle\";\n}\n");
          test_case "atom literal" `Quick (fun () ->
              pretty_prints "mod m { def x = `noValue; }"
                "mod m {\n  def x = `noValue;\n}\n");
          test_case "record literal (fields in source order)" `Quick (fun () ->
              pretty_prints {|mod m { def x = {src: 1, kind: "req"}; }|}
                "mod m {\n  def x = {src: 1, kind: \"req\"};\n}\n");
          test_case "field access" `Quick (fun () ->
              pretty_prints "mod m { def x = m.kind; }"
                "mod m {\n  def x = m.kind;\n}\n");
          test_case "set comprehension" `Quick (fun () ->
              pretty_prints "mod m { def x = { y in s : y > 1 }; }"
                "mod m {\n  def x = { y in s : y > 1 };\n}\n");
          test_case "fair process" `Quick (fun () ->
              pretty_prints "mod m { fair process ps(self in 1..n) = foo; }"
                "mod m {\n  fair process ps(self in 1..n) = foo;\n}\n");
          test_case "strongly fair process" `Quick (fun () ->
              pretty_prints "mod m { fair+ process ps(self in 1..n) = foo; }"
                "mod m {\n  fair+ process ps(self in 1..n) = foo;\n}\n");
          test_case "either arms print on the closing brace's line" `Quick
            (fun () ->
              pretty_prints
                "mod m { procedure foo() { either { x = 1; } or { x = 2; } } }"
                "mod m {\n\
                \  procedure foo() {\n\
                \    either {\n\
                \      x = 1;\n\
                \    } or {\n\
                \      x = 2;\n\
                \    }\n\
                \  }\n\
                 }\n");
          test_case "else if prints on one line" `Quick (fun () ->
              pretty_prints
                "mod m { procedure foo() { if (a) { break; } else if (b) { \
                 break; } else { continue; } } }"
                "mod m {\n\
                \  procedure foo() {\n\
                \    if (a) {\n\
                \      break;\n\
                \    } else if (b) {\n\
                \      break;\n\
                \    } else {\n\
                \      continue;\n\
                \    }\n\
                \  }\n\
                 }\n");
        ] );
    ]
