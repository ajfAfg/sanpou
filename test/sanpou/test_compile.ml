let parse input =
  input |> Lexing.from_string |> Sanpou.Parser.program Sanpou.Lexer.main

let full_example =
  {|
mod rwlock {
  def readerNum = 2;
  def writerNum = 2;

  def foo(x) = x + 1;

  let rcnt = 0;
  let wcnt = 0;
  let lock = false;

  fn lockAcquire() {
    await lock == false,
    lock = true;
    return ();
  }

  fn lockRelease() {
    lock = false;
    return ();
  }

  fn rwlockReadAcquire() {
    while (true) {
      while (0 < wcnt);
      rcnt = rcnt + 1;
      if (wcnt == 0) {
        break;
      }
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
    while (0 < rcnt);
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
      rwlockWriteAcquire();
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
}
|}

let compile cst =
  cst |> Sanpou.Lower.compile_to_ir |> List.map Sanpou.Emit_tla.generate_module

let has_in s tla =
  Alcotest.(check bool)
    s true
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
          Alcotest.test_case "simple proc" `Quick (fun () ->
              let cst =
                parse
                  "mod foo {\n\
                   let x = 0;\n\
                   fn foo() { x = 1; return (); }\n\
                   process ps = foo in 1..2;\n\
                   }\n"
              in
              let modules = compile cst in
              let tla = Tla.Tla_printer.render (List.hd modules) in
              let has s = has_in s tla in
              has "---- MODULE foo ----";
              has "EXTENDS TLC, Sequences, Integers";
              has "VARIABLES pc, x, stack";
              has "x = 0";
              has "Spec == Init /\\ [][Next]_vars";
              has "====");
          Alcotest.test_case "full example compiles" `Quick (fun () ->
              let cst = parse full_example in
              let modules = compile cst in
              let tla = Tla.Tla_printer.render (List.hd modules) in
              let has s = has_in s tla in
              has "---- MODULE rwlock ----";
              has "readerNum == 2";
              has "writerNum == 2";
              has "foo(x) == (x + 1)";
              has "VARIABLES pc, rcnt, wcnt, lock, stack";
              has "ProcSet ==";
              has "lockAcquire(self) ==";
              has "reader(self) ==";
              has "writer(self) ==";
              has "Spec == Init /\\ [][Next]_vars";
              has "====");
          Alcotest.test_case "multiple modules" `Quick (fun () ->
              let cst =
                parse
                  "mod a {\n\
                   let x = 0;\n\
                   fn f() { x = 1; return (); }\n\
                   process ps = f in 1..2;\n\
                   }\n\
                   mod b {\n\
                   let y = 0;\n\
                   fn g() { y = 2; return (); }\n\
                   process qs = g in 1..3;\n\
                   }\n"
              in
              let modules = compile cst in
              Alcotest.(check int) "two modules" 2 (List.length modules);
              let tla_a = Tla.Tla_printer.render (List.nth modules 0) in
              let tla_b = Tla.Tla_printer.render (List.nth modules 1) in
              has_in "---- MODULE a ----" tla_a;
              has_in "VARIABLES pc, x, stack" tla_a;
              has_in "---- MODULE b ----" tla_b;
              has_in "VARIABLES pc, y, stack" tla_b);
          Alcotest.test_case "local variable" `Quick (fun () ->
              let cst =
                parse
                  "mod m {\n\
                   let g = 0;\n\
                   fn foo() { let x = 5; g = x; return (); }\n\
                   process ps = foo in 1..2;\n\
                   }\n"
              in
              let modules = compile cst in
              let tla = Tla.Tla_printer.render (List.hd modules) in
              let has s = has_in s tla in
              has "VARIABLES pc, g, x__1, stack";
              has "x__1 = [self \\in ProcSet |-> 0]";
              has "x__1' = [x__1 EXCEPT ![self] = 5]");
          Alcotest.test_case "shadowing module var" `Quick (fun () ->
              let cst =
                parse
                  "mod m {\n\
                   let x = 0;\n\
                   fn foo() { let x = 42; return (); }\n\
                   process ps = foo in 1..2;\n\
                   }\n"
              in
              let modules = compile cst in
              let tla = Tla.Tla_printer.render (List.hd modules) in
              let has s = has_in s tla in
              has "VARIABLES pc, x, x__1, stack";
              has "x__1' = [x__1 EXCEPT ![self] = 42]");
          Alcotest.test_case "nested shadowing" `Quick (fun () ->
              let cst =
                parse
                  "mod m {\n\
                   let g = 0;\n\
                   fn foo() {\n\
                   let x = 1;\n\
                   while (true) {\n\
                   let x = 2;\n\
                   g = x;\n\
                   break;\n\
                   }\n\
                   g = x;\n\
                   return ();\n\
                   }\n\
                   process ps = foo in 1..2;\n\
                   }\n"
              in
              let modules = compile cst in
              let tla = Tla.Tla_printer.render (List.hd modules) in
              let has s = has_in s tla in
              has "VARIABLES pc, g, x__1, x__2, stack";
              has "x__1' = [x__1 EXCEPT ![self] = 1]";
              has "x__2' = [x__2 EXCEPT ![self] = 2]");
        ] );
    ]
