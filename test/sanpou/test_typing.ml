let parse input =
  input |> Lexing.from_string |> Sanpou.Parser.program Sanpou.Lexer.main

let check_ok input =
  let cst = parse input in
  Sanpou.Typing.check cst

let check_fails input =
  let cst = parse input in
  try
    Sanpou.Typing.check cst;
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
          test_case "function def" `Quick (fun () ->
              check_ok "mod m { def f(x) = x + 1; }");
          test_case "function app in const" `Quick (fun () ->
              check_ok "mod m { def f(x) = x + 1; def y = f(3); }");
          test_case "var decl int" `Quick (fun () ->
              check_ok "mod m { let x = 0; }");
          test_case "var decl bool" `Quick (fun () ->
              check_ok "mod m { let x = false; }");
          test_case "simple proc" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  let x = 0;\n\
                \  fn foo() { x = 1; return (); }\n\
                \  process ps = foo in 1..2;\n\
                \  }");
          test_case "proc with await" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  let lock = false;\n\
                \  fn foo() {\n\
                \    await lock == false, lock = true;\n\
                \    return ();\n\
                \  }\n\
                \  process ps = foo in 1..2;\n\
                \  }");
          test_case "proc with while and break" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  let x = 0;\n\
                \  fn foo() {\n\
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
                \  let lock = false;\n\
                \  fn acquire() {\n\
                \    await lock == false, lock = true;\n\
                \    return ();\n\
                \  }\n\
                \  fn main() {\n\
                \    acquire();\n\
                \    return ();\n\
                \  }\n\
                \  process ps = main in 1..2;\n\
                \  }");
          test_case "local let" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  let g = 0;\n\
                \  fn foo() {\n\
                \    let x = 5;\n\
                \    g = x;\n\
                \    return ();\n\
                \  }\n\
                \  process ps = foo in 1..2;\n\
                \  }");
          test_case "tuple return" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  fn foo() { return (1, 2); }\n\
                \  process ps = foo in 1..2;\n\
                \  }");
          test_case "while wait" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  let x = 0;\n\
                \  fn foo() {\n\
                \    while (0 < x) {}\n\
                \    return ();\n\
                \  }\n\
                \  process ps = foo in 1..2;\n\
                \  }");
          test_case "empty step" `Quick (fun () ->
              check_ok
                "mod m {\n\
                \  fn foo() { ; return (); }\n\
                \  process ps = foo in 1..2;\n\
                \  }");
          test_case "full rwlock" `Quick (fun () ->
              check_ok
                {|mod rwlock {
  def readerNum = 2;
  def writerNum = 2;
  def foo(x) = x + 1;
  let rcnt = 0;
  let wcnt = 0;
  let lock = false;
  fn lockAcquire() {
    await lock == false, lock = true;
    return ();
  }
  fn lockRelease() {
    lock = false;
    return ();
  }
  fn rwlockReadAcquire() {
    while (true) {
      while (0 < wcnt) {}
      rcnt = rcnt + 1;
      if (wcnt == 0) { break; }
      rcnt = rcnt - 1;
    }
    return ();
  }
  fn rwlockReadRelease() {
    rcnt = rcnt - 1;
    return ();
  }
  fn rwlockWriteAcquire() {
    wcnt = wcnt + 1;
    while (0 < rcnt) {}
    lockAcquire();
    return ();
  }
  fn rwlockWriteRelease() {
    lockRelease();
    wcnt = wcnt - 1;
    return ();
  }
  fn reader() {
    while (true) {
      rwlockReadAcquire();
      ;
      rwlockReadRelease();
    }
  }
  fn writer() {
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
                \  let x = 0;\n\
                \  fn foo() { x = true; return (); }\n\
                \  process ps = foo in 1..2;\n\
                \  }");
          test_case "assign to const" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  def x = 0;\n\
                \  fn foo() { x = 1; return (); }\n\
                \  process ps = foo in 1..2;\n\
                \  }");
          test_case "unbound in proc" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  fn foo() { y = 1; return (); }\n\
                \  process ps = foo in 1..2;\n\
                \  }");
          test_case "process range not int" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  fn foo() { return (); }\n\
                \  process ps = foo in true..2;\n\
                \  }");
          test_case "await non-bool" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  fn foo() { await 42; return (); }\n\
                \  process ps = foo in 1..2;\n\
                \  }");
          test_case "while non-bool" `Quick (fun () ->
              check_fails
                "mod m {\n\
                \  fn foo() { while (42) { ; } return (); }\n\
                \  process ps = foo in 1..2;\n\
                \  }");
          test_case "fn arity mismatch" `Quick (fun () ->
              check_fails "mod m { def f(x) = x + 1; def y = f(1, 2); }");
        ] );
    ]
