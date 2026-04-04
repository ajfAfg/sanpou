open Sanpou.Cst

let n = ""
let cl0 = { items = []; commas = [] }
let cl1 x = { items = [ x ]; commas = [] }
let cl2 x y = { items = [ x; y ]; commas = [ n ] }
let cl3 x y z = { items = [ x; y; z ]; commas = [ n; n ] }
let intlit v = IntLit { t = n; value = v }
let boollit v = BoolLit { t = n; value = v }
let var s = Var { t = n; name = s }
let binop op l r = BinOp { lhs = l; op_t = n; op; rhs = r }
let app f args = App { name_t = n; name = f; lp = n; args; rp = n }
let tuple elems tc = Tuple { lp = n; elems; trailing_comma = tc; rp = n }
let paren e = Paren { lp = n; inner = e; rp = n }
let assign x e = Assign { name_t = n; name = x; eq_t = n; value = e }
let call f args = Call { name_t = n; name = f; lp = n; args; rp = n }
let return_ e = Return { t = n; value = e }
let break_ = Break { t = n }
let await_ e = Await { t = n; cond = e }

let simple_step stmts =
  SimpleStep { loc = { line = 0; col = 0 }; stmts; semi_t = n }

let empty_step = EmptyStep { loc = { line = 0; col = 0 }; semi_t = n }
let block_step s = BlockStep { loc = { line = 0; col = 0 }; stmt = s }

let while_ cond body =
  While { while_t = n; lp = n; cond; rp = n; lb = n; body; rb = n }

let if_ cond body = If { if_t = n; lp = n; cond; rp = n; lb = n; body; rb = n }

let const_def x e =
  ConstDef { def_t = n; name_t = n; name = x; eq_t = n; value = e; semi_t = n }

let fun_def f ps e =
  FunDef
    {
      def_t = n;
      name_t = n;
      name = f;
      lp = n;
      params = ps;
      rp = n;
      eq_t = n;
      body_expr = e;
      semi_t = n;
    }

let var_decl x e =
  VarDecl { let_t = n; name_t = n; name = x; eq_t = n; value = e; semi_t = n }

let proc_def f ps body =
  ProcDef
    {
      fn_t = n;
      name_t = n;
      name = f;
      lp = n;
      params = ps;
      rp = n;
      lb = n;
      body;
      rb = n;
    }

let process_ nm pr lo hi =
  Process
    {
      process_t = n;
      name_t = n;
      name = nm;
      eq_t = n;
      proc_t = n;
      proc = pr;
      in_t = n;
      lo;
      dotdot_t = n;
      hi;
      semi_t = n;
    }

let param s = (n, s)

let let_step x e =
  LetStep
    {
      loc = { line = 0; col = 0 };
      let_t = n;
      name_t = n;
      name = x;
      eq_t = n;
      value = e;
      semi_t = n;
    }

let parse input =
  input |> Lexing.from_string |> Sanpou.Parser.program Sanpou.Lexer.main

let parse_items input =
  match (parse ("mod test { " ^ input ^ " }")).modules with
  | [ { items; _ } ] -> items
  | _ -> failwith "expected single module"

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

let () =
  let open Alcotest in
  run "Parse"
    [
      ( "const_def",
        [
          Alcotest.test_case "simple" `Quick (fun () ->
              let actual = parse_items "def x = 42;" in
              Alcotest.(check (list (testable pp_item equal_item)))
                "parse"
                [ const_def "x" (intlit 42) ]
                actual);
          Alcotest.test_case "expr" `Quick (fun () ->
              let actual = parse_items "def x = 1 + 2;" in
              Alcotest.(check (list (testable pp_item equal_item)))
                "parse"
                [ const_def "x" (binop Plus (intlit 1) (intlit 2)) ]
                actual);
        ] );
      ( "fun_def",
        [
          Alcotest.test_case "simple" `Quick (fun () ->
              let actual = parse_items "def foo(x) = x + 1;" in
              Alcotest.(check (list (testable pp_item equal_item)))
                "parse"
                [
                  fun_def "foo"
                    (cl1 (param "x"))
                    (binop Plus (var "x") (intlit 1));
                ]
                actual);
        ] );
      ( "var_decl",
        [
          Alcotest.test_case "int" `Quick (fun () ->
              let actual = parse_items "let x = 0;" in
              Alcotest.(check (list (testable pp_item equal_item)))
                "parse"
                [ var_decl "x" (intlit 0) ]
                actual);
          Alcotest.test_case "bool" `Quick (fun () ->
              let actual = parse_items "let b = false;" in
              Alcotest.(check (list (testable pp_item equal_item)))
                "parse"
                [ var_decl "b" (boollit false) ]
                actual);
        ] );
      ( "proc_def",
        [
          Alcotest.test_case "simple return" `Quick (fun () ->
              let actual = parse_items "fn foo() { return (); }" in
              Alcotest.(check (list (testable pp_item equal_item)))
                "parse"
                [
                  proc_def "foo" cl0
                    [ simple_step (cl1 (return_ (tuple cl0 None))) ];
                ]
                actual);
          Alcotest.test_case "assign and return" `Quick (fun () ->
              let actual = parse_items "fn foo() { x = 1; return (); }" in
              Alcotest.(check (list (testable pp_item equal_item)))
                "parse"
                [
                  proc_def "foo" cl0
                    [
                      simple_step (cl1 (assign "x" (intlit 1)));
                      simple_step (cl1 (return_ (tuple cl0 None)));
                    ];
                ]
                actual);
          Alcotest.test_case "skip step" `Quick (fun () ->
              let actual = parse_items "fn foo() { ; }" in
              Alcotest.(check (list (testable pp_item equal_item)))
                "parse"
                [ proc_def "foo" cl0 [ empty_step ] ]
                actual);
          Alcotest.test_case "return value" `Quick (fun () ->
              let actual = parse_items "fn foo() { return 42; }" in
              Alcotest.(check (list (testable pp_item equal_item)))
                "parse"
                [
                  proc_def "foo" cl0 [ simple_step (cl1 (return_ (intlit 42))) ];
                ]
                actual);
          Alcotest.test_case "return tuple" `Quick (fun () ->
              let actual = parse_items "fn foo() { return (1, 2); }" in
              Alcotest.(check (list (testable pp_item equal_item)))
                "parse"
                [
                  proc_def "foo" cl0
                    [
                      simple_step
                        (cl1 (return_ (tuple (cl2 (intlit 1) (intlit 2)) None)));
                    ];
                ]
                actual);
        ] );
      ( "atomic_step",
        [
          Alcotest.test_case "await with comma" `Quick (fun () ->
              let actual =
                parse_items
                  "fn foo() { await lock == false, lock = true; return (); }"
              in
              Alcotest.(check (list (testable pp_item equal_item)))
                "parse"
                [
                  proc_def "foo" cl0
                    [
                      simple_step
                        (cl2
                           (await_ (binop Eq (var "lock") (boollit false)))
                           (assign "lock" (boollit true)));
                      simple_step (cl1 (return_ (tuple cl0 None)));
                    ];
                ]
                actual);
        ] );
      ( "while",
        [
          Alcotest.test_case "while with body" `Quick (fun () ->
              let actual = parse_items "fn f() { while (true) { break; } }" in
              Alcotest.(check (list (testable pp_item equal_item)))
                "parse"
                [
                  proc_def "f" cl0
                    [
                      block_step
                        (while_ (boollit true) [ simple_step (cl1 break_) ]);
                    ];
                ]
                actual);
          Alcotest.test_case "while wait" `Quick (fun () ->
              let actual = parse_items "fn f() { while (0 < x) {} }" in
              Alcotest.(check (list (testable pp_item equal_item)))
                "parse"
                [
                  proc_def "f" cl0
                    [ block_step (while_ (binop Lt (intlit 0) (var "x")) []) ];
                ]
                actual);
        ] );
      ( "tuple",
        [
          Alcotest.test_case "empty" `Quick (fun () ->
              let actual = parse_items "def x = ();" in
              Alcotest.(check (list (testable pp_item equal_item)))
                "parse"
                [ const_def "x" (tuple cl0 None) ]
                actual);
          Alcotest.test_case "single" `Quick (fun () ->
              let actual = parse_items "def x = (1,);" in
              Alcotest.(check (list (testable pp_item equal_item)))
                "parse"
                [ const_def "x" (tuple (cl1 (intlit 1)) (Some n)) ]
                actual);
          Alcotest.test_case "pair" `Quick (fun () ->
              let actual = parse_items "def x = (1, 2);" in
              Alcotest.(check (list (testable pp_item equal_item)))
                "parse"
                [ const_def "x" (tuple (cl2 (intlit 1) (intlit 2)) None) ]
                actual);
          Alcotest.test_case "triple" `Quick (fun () ->
              let actual = parse_items "def x = (1, 2, 3);" in
              Alcotest.(check (list (testable pp_item equal_item)))
                "parse"
                [
                  const_def "x"
                    (tuple (cl3 (intlit 1) (intlit 2) (intlit 3)) None);
                ]
                actual);
          Alcotest.test_case "nested" `Quick (fun () ->
              let actual = parse_items "def x = ((1, 2), 3);" in
              Alcotest.(check (list (testable pp_item equal_item)))
                "parse"
                [
                  const_def "x"
                    (tuple
                       (cl2 (tuple (cl2 (intlit 1) (intlit 2)) None) (intlit 3))
                       None);
                ]
                actual);
        ] );
      ( "process",
        [
          Alcotest.test_case "simple" `Quick (fun () ->
              let actual = parse_items "process ps = foo in 1..n;" in
              Alcotest.(check (list (testable pp_item equal_item)))
                "parse"
                [ process_ "ps" "foo" (intlit 1) (var "n") ]
                actual);
        ] );
      ( "full_example",
        [
          Alcotest.test_case "parses without error" `Quick (fun () ->
              let _ast = parse full_example in
              ());
        ] );
      ( "let_step",
        [
          Alcotest.test_case "simple" `Quick (fun () ->
              let actual = parse_items "fn foo() { let x = 5; return (); }" in
              Alcotest.(check (list (testable pp_item equal_item)))
                "parse"
                [
                  proc_def "foo" cl0
                    [
                      let_step "x" (intlit 5);
                      simple_step (cl1 (return_ (tuple cl0 None)));
                    ];
                ]
                actual);
          Alcotest.test_case "with expression" `Quick (fun () ->
              let actual =
                parse_items "fn foo() { let y = 1 + 2; return (); }"
              in
              Alcotest.(check (list (testable pp_item equal_item)))
                "parse"
                [
                  proc_def "foo" cl0
                    [
                      let_step "y" (binop Plus (intlit 1) (intlit 2));
                      simple_step (cl1 (return_ (tuple cl0 None)));
                    ];
                ]
                actual);
        ] );
    ]
