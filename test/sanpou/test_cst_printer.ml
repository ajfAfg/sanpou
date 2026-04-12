let parse input =
  input |> Lexing.from_string |> Sanpou.Parser.program Sanpou.Lexer.main

let exact_roundtrip input =
  let cst = parse input in
  let printed = Sanpou.Cst_printer.print cst in
  Alcotest.(check string) "exact roundtrip" input printed

let () =
  let open Alcotest in
  run "CstPrinter"
    [
      ( "exact_roundtrip",
        [
          test_case "const_def" `Quick (fun () ->
              exact_roundtrip "mod m { def x = 42; }");
          test_case "const_def expr" `Quick (fun () ->
              exact_roundtrip "mod m { def x = (1 + 2) * 3; }");
          test_case "fun_def" `Quick (fun () ->
              exact_roundtrip "mod m { def foo(x, y) = x + y; }");
          test_case "var_decl" `Quick (fun () ->
              exact_roundtrip "mod m { var x = 0; }");
          test_case "proc simple" `Quick (fun () ->
              exact_roundtrip "mod m { fn foo() { return (); } }");
          test_case "proc assign" `Quick (fun () ->
              exact_roundtrip "mod m { fn foo() { x = 1; return (); } }");
          test_case "empty step" `Quick (fun () ->
              exact_roundtrip "mod m { fn foo() { ; } }");
          test_case "atomic step" `Quick (fun () ->
              exact_roundtrip
                "mod m { fn foo() { await lock == false, lock = true; } }");
          test_case "while body" `Quick (fun () ->
              exact_roundtrip "mod m { fn foo() { while (true) { break; } } }");
          test_case "while wait" `Quick (fun () ->
              exact_roundtrip "mod m { fn foo() { while (0 < x){} } }");
          test_case "if" `Quick (fun () ->
              exact_roundtrip "mod m { fn foo() { if (x == 0) { break; } } }");
          test_case "inequality" `Quick (fun () ->
              exact_roundtrip "mod m { def x = 1 != 2; }");
          test_case "unary minus literal" `Quick (fun () ->
              exact_roundtrip "mod m { def x = -1; }");
          test_case "unary minus paren expr" `Quick (fun () ->
              exact_roundtrip "mod m { def x = -(1 + 2); }");
          test_case "subtract negative" `Quick (fun () ->
              exact_roundtrip "mod m { def x = y - -1; }");
          test_case "tuple empty" `Quick (fun () ->
              exact_roundtrip "mod m { def x = (); }");
          test_case "tuple single" `Quick (fun () ->
              exact_roundtrip "mod m { def x = (1,); }");
          test_case "tuple pair" `Quick (fun () ->
              exact_roundtrip "mod m { def x = (1, 2); }");
          test_case "tuple nested" `Quick (fun () ->
              exact_roundtrip "mod m { def x = ((1, 2), 3); }");
          test_case "sequence empty" `Quick (fun () ->
              exact_roundtrip "mod m { def x = []; }");
          test_case "sequence single" `Quick (fun () ->
              exact_roundtrip "mod m { def x = [1]; }");
          test_case "sequence single trailing comma" `Quick (fun () ->
              exact_roundtrip "mod m { def x = [1,]; }");
          test_case "sequence pair" `Quick (fun () ->
              exact_roundtrip "mod m { def x = [1, 2]; }");
          test_case "paren expr" `Quick (fun () ->
              exact_roundtrip "mod m { def x = (1 + 2) * 3; }");
          test_case "process" `Quick (fun () ->
              exact_roundtrip "mod m { process ps = foo in 1..n; }");
          test_case "fair process" `Quick (fun () ->
              exact_roundtrip "mod m { fair process ps = foo in 1..n; }");
          test_case "call stmt" `Quick (fun () ->
              exact_roundtrip "mod m { fn foo() { bar(1, 2); } }");
          test_case "app expr" `Quick (fun () ->
              exact_roundtrip "mod m { def x = foo(1, 2); }");
          test_case "var step" `Quick (fun () ->
              exact_roundtrip "mod m { fn foo() { var x = 5; } }");
          test_case "var step expr" `Quick (fun () ->
              exact_roundtrip "mod m { fn foo() { var y = 1 + 2; return (); } }");
          test_case "multiple modules" `Quick (fun () ->
              exact_roundtrip "mod a { var x = 0; }\nmod b { var y = 1; }");
          test_case "full module" `Quick (fun () ->
              exact_roundtrip
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
      ( "whitespace_preservation",
        [
          test_case "extra spaces" `Quick (fun () ->
              exact_roundtrip "mod  m  {  def  x  =  42 ;  }");
          test_case "await wide spaces" `Quick (fun () ->
              exact_roundtrip "mod m { fn f() { await          x == 0; } }");
          test_case "tabs" `Quick (fun () ->
              exact_roundtrip "mod m {\tdef x = 1;\t}");
          test_case "newlines" `Quick (fun () ->
              exact_roundtrip "mod m {\n  def x = 1;\n}");
          test_case "multiple blank lines" `Quick (fun () ->
              exact_roundtrip "mod m {\n\n  def x = 1;\n\n  var y = 2;\n\n}");
        ] );
      ( "comment_preservation",
        [
          test_case "line comment before def" `Quick (fun () ->
              exact_roundtrip "mod m {\n// a comment\ndef x = 1; }");
          test_case "comment between items" `Quick (fun () ->
              exact_roundtrip
                "mod m {\n  def x = 1;\n  // between\n  var y = 2;\n}");
          test_case "comment before module" `Quick (fun () ->
              exact_roundtrip "// header\nmod m { def x = 1; }");
        ] );
      ( "pretty_print",
        [
          test_case "fair process" `Quick (fun () ->
              let printed =
                parse "mod m { fair process ps = foo in 1..n; }"
                |> Sanpou.Cst_printer.print_pretty
              in
              Alcotest.(check string)
                "pretty print" "mod m {\n  fair process ps = foo in 1..n;\n}\n"
                printed);
        ] );
    ]
