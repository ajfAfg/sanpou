open Sanpou.Generic_ast

let loc0 = { line = 0; col = 0 }
let node desc = { desc; loc = loc0 }
let ident = Sanpou.Resolved_ast.ident

(* ----- resolved-tree builders (input) ----- *)

let intlit v = node (IntLit v)
let var s = node (Var (ident s))
let binop op l r = node (BinOp (op, l, r))

let proc_app f args : Sanpou.Resolved_ast.expr =
  node (App (Sanpou.Resolved_ast.Proc f, args))

let fun_app f args : Sanpou.Resolved_ast.expr =
  node (App (Sanpou.Resolved_ast.Fun f, args))

let assign x e = node (Assign (VarTarget (ident x), e))
let assign_idx x i e = node (Assign (SubscriptTarget (ident x, i), e))
let await_ e = node (Await e)
let return_ e = node (Return e)
let call_ f args = node (Call (f, args))
let simple_step stmts : Sanpou.Resolved_ast.step = node (SimpleStep stmts)
let var_step x e : Sanpou.Resolved_ast.step = node (VarStep (ident x, e))

let while_block cond body : Sanpou.Resolved_ast.step =
  node (BlockStep (While { cond; body }))

let if_block cond body : Sanpou.Resolved_ast.step =
  node (BlockStep (If { cond; body; else_body = None }))

let map_init lo hi value = node (MapInit { binder = ident "i"; lo; hi; value })
let if_expr c t f = node (IfExpr (c, t, f))

let forall_ lo hi body =
  node (Quant { quant = Forall; binder = ident "i"; lo; hi; body })

let make_proc name body : Sanpou.Resolved_ast.item =
  node (ProcDef { name; params = []; body })

let make_const name value : Sanpou.Resolved_ast.item =
  node (ConstDef { name; value })

let make_var name value : Sanpou.Resolved_ast.item =
  node (VarDecl { name; init = InitValue value })

let make_process name proc : Sanpou.Resolved_ast.item =
  node (Process { name; proc; fair = false; lo = intlit 1; hi = intlit 1 })

let make_module name items : Sanpou.Resolved_ast.module_def =
  { mod_name = name; items; mod_loc = loc0 }

(* ----- normalized-tree builders (expected) ----- *)

let n_intlit v : Sanpou.Normalized_ast.expr = node (IntLit v)
let n_var s : Sanpou.Normalized_ast.expr = node (Var (ident s))
let n_binop op l r : Sanpou.Normalized_ast.expr = node (BinOp (op, l, r))

let n_fun_app f args : Sanpou.Normalized_ast.expr =
  node (App (Sanpou.Normalized_ast.Fun f, args))

let n_assign x e : Sanpou.Normalized_ast.simple_stmt =
  node (Assign (VarTarget (ident x), e))

let n_assign_idx x i e : Sanpou.Normalized_ast.simple_stmt =
  node (Assign (SubscriptTarget (ident x, i), e))

let n_await e : Sanpou.Normalized_ast.simple_stmt = node (Await e)
let n_return e : Sanpou.Normalized_ast.simple_stmt = node (Return e)
let n_call f args : Sanpou.Normalized_ast.simple_stmt = node (Call (f, args))

let n_simple stmts : Sanpou.Normalized_ast.step =
  node (Sanpou.Normalized_ast.SimpleStep stmts)

let n_var_step x e : Sanpou.Normalized_ast.step =
  node (Sanpou.Normalized_ast.VarStep (ident x, e))

let n_call_bind t f args : Sanpou.Normalized_ast.step =
  node (Sanpou.Normalized_ast.CallBindStep { bind = ident t; callee = f; args })

let n_while pre cond body : Sanpou.Normalized_ast.step =
  node (Sanpou.Normalized_ast.BlockStep (While { pre; cond; body }))

let n_if cond body : Sanpou.Normalized_ast.step =
  node (Sanpou.Normalized_ast.BlockStep (If { cond; body; else_body = None }))

let n_if_expr c t f : Sanpou.Normalized_ast.expr = node (IfExpr (c, t, f))

let n_forall lo hi body : Sanpou.Normalized_ast.expr =
  node (Quant { quant = Forall; binder = ident "i"; lo; hi; body })

(* ----- harness ----- *)

let body_testable =
  Alcotest.testable Sanpou.Normalized_ast.pp_body
    Sanpou.Normalized_ast.equal_body

let expr_testable =
  Alcotest.testable Sanpou.Normalized_ast.pp_expr
    Sanpou.Normalized_ast.equal_expr

(* Normalize a single-module program with one procedure [foo] wrapping
   [body] and return foo's normalized body. *)
let normalize_proc_body ?(extra_items = []) body =
  let m = make_module "m" (extra_items @ [ make_proc "foo" body ]) in
  let normalized = List.hd (Sanpou.Normalize_calls.normalize [ m ]) in
  let proc =
    List.find
      (fun (p : Sanpou.Normalized_ast.proc_def) -> p.name = "foo")
      normalized.procs
  in
  proc.body

let check_body msg expected actual =
  Alcotest.check body_testable msg expected actual

let () =
  let open Alcotest in
  run "Normalize_calls"
    [
      ( "pure",
        [
          test_case "pure statements pass through" `Quick (fun () ->
              let body =
                [
                  var_step "x" (intlit 1);
                  simple_step [ assign "x" (binop Plus (var "x") (intlit 2)) ];
                  simple_step [ await_ (fun_app "f" [ var "x" ]) ];
                ]
              in
              check_body "unchanged"
                [
                  n_var_step "x" (n_intlit 1);
                  n_simple
                    [ n_assign "x" (n_binop Plus (n_var "x") (n_intlit 2)) ];
                  n_simple [ n_await (n_fun_app "f" [ n_var "x" ]) ];
                ]
                (normalize_proc_body body));
        ] );
      ( "hoisting",
        [
          test_case "call in assignment value" `Quick (fun () ->
              let body =
                [
                  simple_step
                    [ assign "x" (binop Plus (proc_app "foo" []) (intlit 1)) ];
                ]
              in
              check_body "hoisted"
                [
                  n_call_bind "callRet__1" "foo" [];
                  n_simple
                    [
                      n_assign "x"
                        (n_binop Plus (n_var "callRet__1") (n_intlit 1));
                    ];
                ]
                (normalize_proc_body body));
          test_case "nested calls hoist innermost first" `Quick (fun () ->
              let body =
                [
                  simple_step [ return_ (proc_app "foo" [ proc_app "foo" [] ]) ];
                ]
              in
              check_body "in evaluation order"
                [
                  n_call_bind "callRet__1" "foo" [];
                  n_call_bind "callRet__2" "foo" [ n_var "callRet__1" ];
                  n_simple [ n_return (n_var "callRet__2") ];
                ]
                (normalize_proc_body body));
          test_case "target index hoists before value" `Quick (fun () ->
              let body =
                [
                  simple_step
                    [ assign_idx "x" (proc_app "foo" []) (proc_app "foo" []) ];
                ]
              in
              check_body "index temp first"
                [
                  n_call_bind "callRet__1" "foo" [];
                  n_call_bind "callRet__2" "foo" [];
                  n_simple
                    [
                      n_assign_idx "x" (n_var "callRet__1") (n_var "callRet__2");
                    ];
                ]
                (normalize_proc_body body));
          test_case "hoists precede the whole merged step" `Quick (fun () ->
              let body =
                [
                  simple_step
                    [ assign "x" (intlit 1); assign "y" (proc_app "foo" []) ];
                ]
              in
              check_body "one merged step remains"
                [
                  n_call_bind "callRet__1" "foo" [];
                  n_simple
                    [
                      n_assign "x" (n_intlit 1);
                      n_assign "y" (n_var "callRet__1");
                    ];
                ]
                (normalize_proc_body body));
          test_case "var step binding a call" `Quick (fun () ->
              let body = [ var_step "x" (proc_app "foo" []) ] in
              check_body "temp then var"
                [
                  n_call_bind "callRet__1" "foo" [];
                  n_var_step "x" (n_var "callRet__1");
                ]
                (normalize_proc_body body));
          test_case "call statement args hoisted" `Quick (fun () ->
              let body =
                [ simple_step [ call_ "foo" [ proc_app "foo" [] ] ] ]
              in
              check_body "call stays a statement"
                [
                  n_call_bind "callRet__1" "foo" [];
                  n_simple [ n_call "foo" [ n_var "callRet__1" ] ];
                ]
                (normalize_proc_body body));
        ] );
      ( "control_flow",
        [
          test_case "while condition calls go into pre" `Quick (fun () ->
              let body =
                [
                  while_block
                    (binop Lt (proc_app "foo" []) (intlit 3))
                    [ simple_step [ assign "x" (intlit 1) ] ];
                ]
              in
              check_body "pre holds the call"
                [
                  n_while
                    [ n_call_bind "callRet__1" "foo" [] ]
                    (n_binop Lt (n_var "callRet__1") (n_intlit 3))
                    [ n_simple [ n_assign "x" (n_intlit 1) ] ];
                ]
                (normalize_proc_body body));
          test_case "if condition calls become preceding steps" `Quick
            (fun () ->
              let body =
                [
                  if_block
                    (binop Lt (proc_app "foo" []) (intlit 3))
                    [ simple_step [ assign "x" (intlit 1) ] ];
                ]
              in
              check_body "sibling steps before if"
                [
                  n_call_bind "callRet__1" "foo" [];
                  n_if
                    (n_binop Lt (n_var "callRet__1") (n_intlit 3))
                    [ n_simple [ n_assign "x" (n_intlit 1) ] ];
                ]
                (normalize_proc_body body));
          test_case "map-init bounds may call, in order" `Quick (fun () ->
              let body =
                [
                  var_step "m"
                    (map_init (proc_app "foo" []) (proc_app "foo" []) (intlit 0));
                ]
              in
              match normalize_proc_body body with
              | [ s1; s2; _ ] ->
                  check body_testable "lo then hi"
                    [
                      n_call_bind "callRet__1" "foo" [];
                      n_call_bind "callRet__2" "foo" [];
                    ]
                    [ s1; s2 ]
              | _ -> fail "expected two hoisted steps and the var step");
        ] );
      ( "errors",
        [
          test_case "call in if-expression condition is hoisted" `Quick
            (fun () ->
              let body =
                [
                  var_step "y"
                    (if_expr
                       (binop Lt (proc_app "foo" []) (intlit 3))
                       (intlit 1) (intlit 2));
                ]
              in
              check_body "condition call hoisted before the step"
                [
                  n_call_bind "callRet__1" "foo" [];
                  n_var_step "y"
                    (n_if_expr
                       (n_binop Lt (n_var "callRet__1") (n_intlit 3))
                       (n_intlit 1) (n_intlit 2));
                ]
                (normalize_proc_body body));
          test_case "call in quantifier bounds is hoisted" `Quick (fun () ->
              let body =
                [
                  var_step "y"
                    (forall_ (proc_app "foo" []) (intlit 3)
                       (binop Lt (var "i") (intlit 4)));
                ]
              in
              check_body "bound call hoisted before the step"
                [
                  n_call_bind "callRet__1" "foo" [];
                  n_var_step "y"
                    (n_forall (n_var "callRet__1") (n_intlit 3)
                       (n_binop Lt (n_var "i") (n_intlit 4)));
                ]
                (normalize_proc_body body));
          test_case "call in quantifier body rejected" `Quick (fun () ->
              let body =
                [
                  var_step "y"
                    (forall_ (intlit 1) (intlit 3)
                       (binop Lt (proc_app "foo" []) (intlit 4)));
                ]
              in
              check_raises "located error"
                (Sanpou.Normalize_calls.Error
                   ( "procedure calls are not allowed inside a quantifier body",
                     loc0 ))
                (fun () -> ignore (normalize_proc_body body)));
          test_case "call in if-expression then branch rejected" `Quick
            (fun () ->
              let body =
                [
                  var_step "y"
                    (if_expr (var "b") (proc_app "foo" []) (intlit 2));
                ]
              in
              check_raises "located error"
                (Sanpou.Normalize_calls.Error
                   ( "procedure calls are not allowed in the then branch of \
                      an if-expression",
                     loc0 ))
                (fun () -> ignore (normalize_proc_body body)));
          test_case "call in if-expression else branch rejected" `Quick
            (fun () ->
              let body =
                [
                  var_step "y"
                    (if_expr (var "b") (intlit 1) (proc_app "foo" []));
                ]
              in
              check_raises "located error"
                (Sanpou.Normalize_calls.Error
                   ( "procedure calls are not allowed in the else branch of \
                      an if-expression",
                     loc0 ))
                (fun () -> ignore (normalize_proc_body body)));
          test_case "call in map-init value rejected" `Quick (fun () ->
              let body =
                [
                  var_step "m"
                    (map_init (intlit 1) (intlit 3) (proc_app "foo" []));
                ]
              in
              check_raises "located error"
                (Sanpou.Normalize_calls.Error
                   ( "procedure calls are not allowed inside a map initializer",
                     loc0 ))
                (fun () -> ignore (normalize_proc_body body)));
          test_case "call in const def rejected" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_const "c" (proc_app "foo" []);
                    make_proc "foo" [ simple_step [ return_ (intlit 0) ] ];
                  ]
              in
              check_raises "located error"
                (Sanpou.Normalize_calls.Error
                   ( "procedure calls are not allowed in module-level \
                      expressions",
                     loc0 ))
                (fun () -> ignore (Sanpou.Normalize_calls.normalize [ m ])));
        ] );
      ( "modules",
        [
          test_case "items are partitioned" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_const "c" (intlit 5);
                    make_var "x" (intlit 0);
                    make_proc "foo" [ simple_step [ return_ (intlit 0) ] ];
                    make_process "ps" "foo";
                  ]
              in
              let normalized =
                List.hd (Sanpou.Normalize_calls.normalize [ m ])
              in
              check string "name" "m" normalized.name;
              check int "consts" 1 (List.length normalized.const_defs);
              check int "vars" 1 (List.length normalized.var_decls);
              check int "procs" 1 (List.length normalized.procs);
              check int "processes" 1 (List.length normalized.processes);
              check expr_testable "const value" (n_intlit 5)
                (snd (List.hd normalized.const_defs)));
          test_case "temp counter restarts per module" `Quick (fun () ->
              let proc_with_call =
                make_proc "foo" [ var_step "x" (proc_app "foo" []) ]
              in
              let m1 = make_module "m1" [ proc_with_call ] in
              let m2 = make_module "m2" [ proc_with_call ] in
              match Sanpou.Normalize_calls.normalize [ m1; m2 ] with
              | [ n1; n2 ] ->
                  let first_bind (n : Sanpou.Normalized_ast.module_def) =
                    match (List.hd n.procs).body with
                    | { desc = CallBindStep { bind; _ }; _ } :: _ -> bind.name
                    | _ -> fail "expected a CallBindStep"
                  in
                  check string "m1 temp" "callRet__1" (first_bind n1);
                  check string "m2 temp" "callRet__1" (first_bind n2)
              | _ -> fail "expected two modules");
        ] );
    ]
