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
    (Sanpou.Ast.equal_program Sanpou.Ast.equal_id ast reparsed)

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
          test_case "proc simple" `Quick (fun () ->
              pretty_roundtrip "mod m { fn foo() { return (); } }");
          test_case "proc assign" `Quick (fun () ->
              pretty_roundtrip "mod m { fn foo() { x = 1; return (); } }");
          test_case "skip step" `Quick (fun () ->
              pretty_roundtrip "mod m { fn foo() { ; } }");
          test_case "atomic step" `Quick (fun () ->
              pretty_roundtrip
                "mod m { fn foo() { await lock == false, lock = true; } }");
          test_case "while body" `Quick (fun () ->
              pretty_roundtrip "mod m { fn foo() { while (true) { break; } } }");
          test_case "while wait" `Quick (fun () ->
              pretty_roundtrip "mod m { fn foo() { while (0 < x){} } }");
          test_case "if" `Quick (fun () ->
              pretty_roundtrip "mod m { fn foo() { if (x == 0) { break; } } }");
          test_case "if else" `Quick (fun () ->
              pretty_roundtrip
                "mod m { fn foo() { if (x == 0) { break; } else { continue; } } }");
          test_case "inequality" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = 1 != 2; }");
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
          test_case "map init" `Quick (fun () ->
              pretty_roundtrip "mod m { var xs = { x in 1..2: false; }; }");
          test_case "subscript" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = xs[1 + 2]; }");
          test_case "process" `Quick (fun () ->
              pretty_roundtrip "mod m { process ps = foo in 1..n; }");
          test_case "fair process" `Quick (fun () ->
              pretty_roundtrip "mod m { fair process ps = foo in 1..n; }");
          test_case "call stmt" `Quick (fun () ->
              pretty_roundtrip "mod m { fn foo() { bar(1, 2); } }");
          test_case "app expr" `Quick (fun () ->
              pretty_roundtrip "mod m { def x = foo(1, 2); }");
          test_case "var step" `Quick (fun () ->
              pretty_roundtrip "mod m { fn foo() { var x = 5; } }");
          test_case "var step expr" `Quick (fun () ->
              pretty_roundtrip
                "mod m { fn foo() { var y = 1 + 2; return (); } }");
          test_case "multiple modules" `Quick (fun () ->
              pretty_roundtrip "mod a { var x = 0; }\nmod b { var y = 1; }");
          test_case "full module" `Quick (fun () ->
              pretty_roundtrip
                {|mod rwlock {
  def readerNum = 2;
    var rcnt = 0;
    var lock = false;
  fn lockAcquire() {
    await lock == false, lock = true;
    return ();
  }
  fn reader() {
    while (true) {
      lockAcquire();
      ;
      rcnt = rcnt - 1;
    }
  }
  process readers = reader in 1..readerNum;
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
        ] );
      ( "pretty_print",
        [
          test_case "fair process" `Quick (fun () ->
              pretty_prints "mod m { fair process ps = foo in 1..n; }"
                "mod m {\n  fair process ps = foo in 1..n;\n}\n");
        ] );
    ]
