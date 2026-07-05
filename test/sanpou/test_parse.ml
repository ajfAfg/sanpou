(* Parser cases where the AST shape is not obvious from the surface syntax:
   step atomicity, tuple/paren disambiguation, trailing commas, builtin
   resolution, and unary-minus grammar. Plain "construct X parses" coverage
   lives in the pretty-print roundtrip corpus (test_ast_printer.ml). *)

open Sanpou.Generic_ast

(* Trailing-comma markers are not represented in the AST; the [tc] parameters
   below are kept so call sites read like the surface syntax. *)
let n = ""
let loc0 = { line = 0; col = 0 }
let node desc = { desc; loc = loc0 }
let cl0 = []
let cl1 x = [ x ]
let cl2 x y = [ x; y ]
let intlit v = node (IntLit v)
let boollit v = node (BoolLit v)
let var s = node (Var s)
let unop op r = node (UnOp (op, r))
let binop op l r = node (BinOp (op, l, r))
let app f args = node (App (f, args))
let builtin_ b args = node (Builtin (b, args))
let tuple elems _tc = node (Tuple elems)
let sequence elems _tc = node (Sequence elems)
let paren e = e
let assign x e = node (Assign (VarTarget x, e))
let return_ e = node (Return e)
let await_ e = node (Await e)
let simple_step stmts = node (SimpleStep stmts)
let empty_step = node EmptyStep
let const_def x e = node (ConstDef { name = x; value = e })
let fun_def f ps e = node (FunDef { name = f; params = ps; body_expr = e })
let proc_def f ps body = node (ProcDef { name = f; params = ps; body })
let param s = s
let var_step x e = node (VarStep (x, e))

let parse input =
  input |> Lexing.from_string |> Sanpou.Parser.program Sanpou.Lexer.main

let parse_items input =
  match parse ("mod test { " ^ input ^ " }") with
  | [ { items; _ } ] -> items
  | _ -> failwith "expected single module"

let items_t =
  Alcotest.(
    list (testable (pp_item pp_id pp_id) (equal_item equal_id equal_id)))

let check_items input expected =
  Alcotest.check items_t "parse" expected (parse_items input)

let () =
  let open Alcotest in
  run "Parse"
    [
      ( "step_atomicity",
        [
          test_case "statements split into separate steps" `Quick (fun () ->
              check_items "fn foo() { x = 1; return (); }"
                [
                  proc_def "foo" cl0
                    [
                      simple_step (cl1 (assign "x" (intlit 1)));
                      simple_step (cl1 (return_ (tuple cl0 None)));
                    ];
                ]);
          test_case "await with comma merges into one step" `Quick (fun () ->
              check_items
                "fn foo() { await lock == false, lock = true; return (); }"
                [
                  proc_def "foo" cl0
                    [
                      simple_step
                        (cl2
                           (await_ (binop Eq (var "lock") (boollit false)))
                           (assign "lock" (boollit true)));
                      simple_step (cl1 (return_ (tuple cl0 None)));
                    ];
                ]);
          test_case "bare semicolon is an empty step" `Quick (fun () ->
              check_items "fn foo() { ; }" [ proc_def "foo" cl0 [ empty_step ] ]);
        ] );
      ( "tuple_vs_paren",
        [
          test_case "empty" `Quick (fun () ->
              check_items "def x = ();" [ const_def "x" (tuple cl0 None) ]);
          test_case "single needs trailing comma" `Quick (fun () ->
              check_items "def x = (1,);"
                [ const_def "x" (tuple (cl1 (intlit 1)) (Some n)) ]);
          test_case "pair" `Quick (fun () ->
              check_items "def x = (1, 2);"
                [ const_def "x" (tuple (cl2 (intlit 1) (intlit 2)) None) ]);
        ] );
      ( "sequence",
        [
          test_case "empty" `Quick (fun () ->
              check_items "def x = [];" [ const_def "x" (sequence cl0 None) ]);
          test_case "single trailing comma" `Quick (fun () ->
              check_items "def x = [1,];"
                [ const_def "x" (sequence (cl1 (intlit 1)) (Some n)) ]);
        ] );
      ( "builtin",
        [
          test_case "application resolves to builtin" `Quick (fun () ->
              check_items "def x = head(xs);"
                [
                  const_def "x" (builtin_ Sanpou.Builtin.Head (cl1 (var "xs")));
                ]);
          test_case "non-builtin stays an app" `Quick (fun () ->
              check_items "def x = first(xs);"
                [ const_def "x" (app "first" (cl1 (var "xs"))) ]);
        ] );
      ( "unary_minus",
        [
          test_case "literal" `Quick (fun () ->
              check_items "def x = -1;" [ const_def "x" (unop Neg (intlit 1)) ]);
          test_case "paren expr" `Quick (fun () ->
              check_items "def x = -(1 + 2);"
                [
                  const_def "x"
                    (unop Neg (paren (binop Plus (intlit 1) (intlit 2))));
                ]);
          test_case "subtract negative" `Quick (fun () ->
              check_items "def x = y - -1;"
                [ const_def "x" (binop Minus (var "y") (unop Neg (intlit 1))) ]);
        ] );
      ( "definitions",
        [
          test_case "fun_def with params" `Quick (fun () ->
              check_items "def foo(x) = x + 1;"
                [
                  fun_def "foo"
                    (cl1 (param "x"))
                    (binop Plus (var "x") (intlit 1));
                ]);
          test_case "var step" `Quick (fun () ->
              check_items "fn foo() { var x = 5; return (); }"
                [
                  proc_def "foo" cl0
                    [
                      var_step "x" (intlit 5);
                      simple_step (cl1 (return_ (tuple cl0 None)));
                    ];
                ]);
          test_case "let is rejected" `Quick (fun () ->
              try
                let _ = parse_items "let x = 0;" in
                Alcotest.fail "Expected parse error"
              with _ -> ());
        ] );
    ]
