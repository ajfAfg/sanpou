let parse input =
  input |> Lexing.from_string |> Sanpou.Parser.program Sanpou.Lexer.main

let full_example =
  {|
mod rwlock {
  def readerNum = 2;
  def writerNum = 2;

  def foo(x) = x + 1;

  var rcnt = 0;
  var wcnt = 0;
  var lock = false;

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
      while (0 < wcnt) {}
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
  Sanpou.Typing.check cst;
  cst |> Sanpou.Alpha_convert.transform |> Sanpou.Linearize.linearize
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
          Alcotest.test_case "simple proc" `Quick (fun () ->
              let cst =
                parse
                  "mod foo {\n\
                   var x = 0;\n\
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
              has "__process_ps_wrapper__(self) ==";
              has "x = 0";
              has "Spec == Init /\\ [][Next]_vars";
              has "====");
          Alcotest.test_case "top-level return uses process wrapper" `Quick
            (fun () ->
              let cst =
                parse
                  "mod foo {\n\
                   fn foo() { return (); }\n\
                   process ps = foo in 1..1;\n\
                   }\n"
              in
              let tla = compile cst |> List.hd |> Tla.Tla_printer.render in
              let has s = has_in s tla in
              has "__process_ps_wrapper__(self) ==";
              has "__w_ps_entry__(self) ==";
              has "__w_ps_discard__(self) ==";
              has "pc = [self \\in ProcSet |-> \"__w_ps_entry__\"]");
          Alcotest.test_case "fair process adds weak fairness" `Quick (fun () ->
              let cst =
                parse
                  "mod foo {\n\
                   var x = 0;\n\
                   fn foo() { x = 1 - x; }\n\
                   fair process ps = foo in 1..2;\n\
                   }\n"
              in
              let modules = compile cst in
              let tla = Tla.Tla_printer.render (List.hd modules) in
              let has s = has_in s tla in
              has "WF_vars(__process_ps_wrapper__(self))";
              has "WF_vars(foo(self))";
              has "====");
          Alcotest.test_case "fair process covers helper procedures" `Quick
            (fun () ->
              let cst =
                parse
                  "mod foo {\n\
                   var x = 0;\n\
                   fn helper() { x = 1; return (); }\n\
                   fn main() { helper(); return (); }\n\
                   fair process ps = main in 1..2;\n\
                   }\n"
              in
              let tla = compile cst |> List.hd |> Tla.Tla_printer.render in
              let has s = has_in s tla in
              has "WF_vars(__process_ps_wrapper__(self))";
              has "WF_vars(main(self))";
              has "WF_vars(helper(self))";
              has "return_pc |->");
          Alcotest.test_case "plain process omits weak fairness" `Quick
            (fun () ->
              let cst =
                parse
                  "mod foo {\n\
                   var x = 0;\n\
                   fn foo() { x = 1 - x; }\n\
                   process ps = foo in 1..2;\n\
                   }\n"
              in
              let modules = compile cst in
              let tla = Tla.Tla_printer.render (List.hd modules) in
              let has_not s =
                Alcotest.(check bool)
                  s false
                  (try
                     let _ = Str.search_forward (Str.regexp_string s) tla 0 in
                     true
                   with Not_found -> false)
              in
              has_not "WF_vars(foo(self))");
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
              has "__process_readers_wrapper__(self) ==";
              has "reader(self) ==";
              has "writer(self) ==";
              has "Spec == Init /\\ [][Next]_vars";
              has "====");
          Alcotest.test_case "sequence builtins compile to tla sequences" `Quick
            (fun () ->
              let cst =
                parse
                  "mod seqs {\n\
                   def xs = [1, 2];\n\
                   def y = head(xs);\n\
                   def ys = tail(xs);\n\
                   def zs = append(xs, 3);\n\
                   def ws = concat(xs, [4, 5]);\n\
                   fn main() { return (); }\n\
                   process ps = main in 1..1;\n\
                   }\n"
              in
              let tla = compile cst |> List.hd |> Tla.Tla_printer.render in
              let has s = has_in s tla in
              has "xs == << 1, 2 >>";
              has "y == Head(xs)";
              has "ys == Tail(xs)";
              has "zs == Append(xs, 3)";
              has "ws == xs \\o << 4, 5 >>");
          Alcotest.test_case "inequality compiles to tla not-equals" `Quick
            (fun () ->
              let cst =
                parse
                  "mod neq {\n\
                   def differs = [1] != [2];\n\
                   fn main() { return (); }\n\
                   process ps = main in 1..1;\n\
                   }\n"
              in
              let tla = compile cst |> List.hd |> Tla.Tla_printer.render in
              has_in "differs == (<< 1 >> /= << 2 >>)" tla);
          Alcotest.test_case "or compiles to tla disjunction" `Quick (fun () ->
              let cst =
                parse
                  "mod ormod {\n\
                   def either = true || false;\n\
                   fn main() { return (); }\n\
                   process ps = main in 1..1;\n\
                   }\n"
              in
              let tla = compile cst |> List.hd |> Tla.Tla_printer.render in
              has_in "either == (TRUE \\/ FALSE)" tla);
          Alcotest.test_case "map init and indexed update compile" `Quick
            (fun () ->
              let cst =
                parse
                  "mod maps {\n\
                   var xs = { i in 1..2: 0; };\n\
                   fn main() { xs[1] = self; return (); }\n\
                   process ps = main in 1..2;\n\
                   }\n"
              in
              let tla = compile cst |> List.hd |> Tla.Tla_printer.render in
              has_in "xs = [i \\in 1..2 |-> 0]" tla;
              has_in "xs' = [xs EXCEPT ![1] = self]" tla);
          Alcotest.test_case "unary minus compiles" `Quick (fun () ->
              let cst =
                parse
                  "mod neg {\n\
                   def literal = -1;\n\
                   def expr = -(1 + 2);\n\
                   fn main() { return (); }\n\
                   process ps = main in 1..1;\n\
                   }\n"
              in
              let tla = compile cst |> List.hd |> Tla.Tla_printer.render in
              has_in "literal == -1" tla;
              has_in "expr == (0 - (1 + 2))" tla);
          Alcotest.test_case "multiple modules" `Quick (fun () ->
              let cst =
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
                   var g = 0;\n\
                   fn foo() { var x = 5; g = x; return (); }\n\
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
                   var x = 0;\n\
                   fn foo() { var x = 42; return (); }\n\
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
              let modules = compile cst in
              let tla = Tla.Tla_printer.render (List.hd modules) in
              let has s = has_in s tla in
              has "VARIABLES pc, g, x__1, x__2, stack";
              has "x__1' = [x__1 EXCEPT ![self] = 1]";
              has "x__2' = [x__2 EXCEPT ![self] = 2]");
          Alcotest.test_case "procedure parameters become local state" `Quick
            (fun () ->
              let cst =
                parse
                  "mod params {\n\
                   fn callee(idx) {\n\
                   var seen = idx;\n\
                   return ();\n\
                   }\n\
                   fn caller() { callee(self); return (); }\n\
                   process ps = caller in 1..2;\n\
                   }\n"
              in
              let tla = compile cst |> List.hd |> Tla.Tla_printer.render in
              let has s = has_in s tla in
              has "VARIABLES pc, idx__1, seen__2, stack";
              has "idx__1 = [self \\in ProcSet |-> 0]";
              has "idx__1' = [idx__1 EXCEPT ![self] = self]";
              has "seen__2' = [seen__2 EXCEPT ![self] = idx__1[self]]";
              has_not_in "![idx]" tla);
        ] );
    ]
