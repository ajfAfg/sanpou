open Sanpou.Ast

let loc0 = { line = 0; col = 0 }
let node desc = { desc; loc = loc0 }
let cl1 x = [ x ]
let intlit v = node (IntLit v)
let boollit v = node (BoolLit v)
let var s = node (Var s)
let binop op l r = node (BinOp (op, l, r))
let assign x e = node (Assign (VarTarget x, e))
let return_ e = node (Return e)
let await_ e = node (Await e)
let simple_step stmts = node (SimpleStep stmts)
let var_step name value = node (VarStep (name, value))
let while_block cond body = node (BlockStep (While { cond; body }))

let if_block cond body =
  node (BlockStep (If { cond; body; else_body = None }))

let make_proc name body = node (ProcDef { name; params = []; body })

(* TLA names of renamed variables that become state variables (i.e. not
   MapInit binders), in declaration order *)
let local_tla_names (am : Sanpou.Alpha_convert.alpha_module) =
  List.filter_map
    (fun (r : Sanpou.Alpha_convert.rename) ->
      match r.kind with
      | Sanpou.Alpha_convert.BinderVar -> None
      | _ -> Some r.tla_name)
    am.renames

let make_module items = { mod_name = "m"; items; mod_loc = loc0 }
let make_program modules = modules

let transform_one input =
  let result = Sanpou.Alpha_convert.transform input in
  List.hd result

let get_proc_body (am : Sanpou.Alpha_convert.alpha_module) proc_name =
  let item =
    List.find
      (fun (item : item) ->
        match item.desc with
        | ProcDef { name; _ } -> name = proc_name
        | _ -> false)
      am.ast.items
  in
  match item.desc with ProcDef { body; _ } -> body | _ -> assert false

let extract_var_name (e : expr) =
  match e.desc with Var name -> name | _ -> failwith "expected Var"

let extract_assign_name (stmt : simple_stmt) =
  match stmt.desc with
  | Assign (VarTarget name, _) | Assign (SubscriptTarget (name, _), _) -> name
  | _ -> failwith "expected Assign"

let extract_assign_value (stmt : simple_stmt) =
  match stmt.desc with
  | Assign (_, value) -> value
  | _ -> failwith "expected Assign"

let extract_var_step_name (step : step) =
  match step.desc with
  | VarStep (name, _) -> name
  | _ -> failwith "expected VarStep"

let extract_var_step_value (step : step) =
  match step.desc with
  | VarStep (_, value) -> value
  | _ -> failwith "expected VarStep"

let extract_simple_stmts (step : step) =
  match step.desc with
  | SimpleStep stmts -> stmts
  | _ -> failwith "expected SimpleStep"

let () =
  let open Alcotest in
  run "Alpha_convert"
    [
      ( "rename",
        [
          test_case "var step renamed" `Quick (fun () ->
              let prog =
                make_program
                  [
                    make_module [ make_proc "foo" [ var_step "x" (intlit 5) ] ];
                  ]
              in
              let am = transform_one prog in
              let body = get_proc_body am "foo" in
              check string "renamed" "x__1"
                (extract_var_step_name (List.hd body)));
          test_case "var value unchanged" `Quick (fun () ->
              let prog =
                make_program
                  [
                    make_module [ make_proc "foo" [ var_step "x" (intlit 42) ] ];
                  ]
              in
              let am = transform_one prog in
              let body = get_proc_body am "foo" in
              check bool "value" true
                (equal_expr (intlit 42) (extract_var_step_value (List.hd body))));
          test_case "usage after var uses new name" `Quick (fun () ->
              let prog =
                make_program
                  [
                    make_module
                      [
                        make_proc "foo"
                          [
                            var_step "x" (intlit 1);
                            simple_step (cl1 (assign "g" (var "x")));
                          ];
                      ];
                  ]
              in
              let am = transform_one prog in
              let body = get_proc_body am "foo" in
              let stmts = extract_simple_stmts (List.nth body 1) in
              let value = extract_assign_value (List.hd stmts) in
              check string "var renamed" "x__1" (extract_var_name value));
          test_case "multiple vars get different names" `Quick (fun () ->
              let prog =
                make_program
                  [
                    make_module
                      [
                        make_proc "foo"
                          [ var_step "x" (intlit 1); var_step "y" (intlit 2) ];
                      ];
                  ]
              in
              let am = transform_one prog in
              let body = get_proc_body am "foo" in
              check string "first" "x__1"
                (extract_var_step_name (List.nth body 0));
              check string "second" "y__2"
                (extract_var_step_name (List.nth body 1)));
          test_case "same name shadowed in while" `Quick (fun () ->
              let prog =
                make_program
                  [
                    make_module
                      [
                        make_proc "foo"
                          [
                            var_step "x" (intlit 1);
                            while_block (boollit true)
                              [ var_step "x" (intlit 2) ];
                          ];
                      ];
                  ]
              in
              let am = transform_one prog in
              let body = get_proc_body am "foo" in
              check string "outer" "x__1"
                (extract_var_step_name (List.nth body 0));
              match (List.nth body 1).desc with
              | BlockStep (While { body = while_body; _ }) ->
                  check string "inner" "x__2"
                    (extract_var_step_name (List.hd while_body))
              | _ -> fail "expected while");
        ] );
      ( "local_vars",
        [
          test_case "collects local vars" `Quick (fun () ->
              let prog =
                make_program
                  [
                    make_module
                      [
                        make_proc "foo"
                          [ var_step "x" (intlit 1); var_step "y" (intlit 2) ];
                      ];
                  ]
              in
              let am = transform_one prog in
              check (list string) "locals" [ "x__1"; "y__2" ]
                (local_tla_names am);
              check (list string) "originals" [ "x"; "y" ]
                (List.map
                   (fun (r : Sanpou.Alpha_convert.rename) -> r.original)
                   am.renames);
              check (list string) "owning proc" [ "foo"; "foo" ]
                (List.map
                   (fun (r : Sanpou.Alpha_convert.rename) -> r.proc)
                   am.renames));
          test_case "no var steps no locals" `Quick (fun () ->
              let prog =
                make_program
                  [
                    make_module
                      [
                        make_proc "foo"
                          [ simple_step (cl1 (return_ (intlit 0))) ];
                      ];
                  ]
              in
              let am = transform_one prog in
              check (list string) "empty" [] (local_tla_names am));
          test_case "nested var in while collected" `Quick (fun () ->
              let prog =
                make_program
                  [
                    make_module
                      [
                        make_proc "foo"
                          [
                            var_step "x" (intlit 1);
                            while_block (boollit true)
                              [ var_step "y" (intlit 2) ];
                          ];
                      ];
                  ]
              in
              let am = transform_one prog in
              check (list string) "locals" [ "x__1"; "y__2" ]
                (local_tla_names am));
          test_case "nested var in if collected" `Quick (fun () ->
              let prog =
                make_program
                  [
                    make_module
                      [
                        make_proc "foo"
                          [
                            if_block (boollit true) [ var_step "z" (intlit 3) ];
                          ];
                      ];
                  ]
              in
              let am = transform_one prog in
              check (list string) "locals" [ "z__1" ] (local_tla_names am));
        ] );
      ( "expressions",
        [
          test_case "global var not renamed" `Quick (fun () ->
              let prog =
                make_program
                  [
                    make_module
                      [
                        make_proc "foo"
                          [ simple_step (cl1 (assign "g" (var "g"))) ];
                      ];
                  ]
              in
              let am = transform_one prog in
              let body = get_proc_body am "foo" in
              let stmts = extract_simple_stmts (List.hd body) in
              check string "assign target" "g"
                (extract_assign_name (List.hd stmts));
              check string "var ref" "g"
                (extract_var_name (extract_assign_value (List.hd stmts))));
          test_case "var value references earlier var" `Quick (fun () ->
              let prog =
                make_program
                  [
                    make_module
                      [
                        make_proc "foo"
                          [ var_step "x" (intlit 1); var_step "y" (var "x") ];
                      ];
                  ]
              in
              let am = transform_one prog in
              let body = get_proc_body am "foo" in
              check string "y value uses x__1" "x__1"
                (extract_var_name (extract_var_step_value (List.nth body 1))));
          test_case "binop subexprs renamed" `Quick (fun () ->
              let prog =
                make_program
                  [
                    make_module
                      [
                        make_proc "foo"
                          [
                            var_step "x" (intlit 1);
                            simple_step
                              (cl1
                                 (assign "g" (binop Plus (var "x") (intlit 2))));
                          ];
                      ];
                  ]
              in
              let am = transform_one prog in
              let body = get_proc_body am "foo" in
              let stmts = extract_simple_stmts (List.nth body 1) in
              let value = extract_assign_value (List.hd stmts) in
              match value.desc with
              | BinOp (_, lhs, _) ->
                  check string "lhs renamed" "x__1" (extract_var_name lhs)
              | _ -> fail "expected BinOp");
          test_case "await cond renamed" `Quick (fun () ->
              let prog =
                make_program
                  [
                    make_module
                      [
                        make_proc "foo"
                          [
                            var_step "x" (intlit 1);
                            simple_step (cl1 (await_ (var "x")));
                          ];
                      ];
                  ]
              in
              let am = transform_one prog in
              let body = get_proc_body am "foo" in
              let stmts = extract_simple_stmts (List.nth body 1) in
              match (List.hd stmts).desc with
              | Await cond -> check string "cond" "x__1" (extract_var_name cond)
              | _ -> fail "expected Await");
        ] );
      ( "non_proc_items",
        [
          test_case "const def unchanged" `Quick (fun () ->
              let const = node (ConstDef { name = "c"; value = intlit 1 }) in
              let prog = make_program [ make_module [ const ] ] in
              let am = transform_one prog in
              match (List.hd am.ast.items).desc with
              | ConstDef { name; value } ->
                  check string "name" "c" name;
                  check bool "value" true (equal_expr (intlit 1) value)
              | _ -> fail "expected ConstDef");
          test_case "var decl unchanged" `Quick (fun () ->
              let vd = node (VarDecl { name = "v"; value = intlit 0 }) in
              let prog = make_program [ make_module [ vd ] ] in
              let am = transform_one prog in
              match (List.hd am.ast.items).desc with
              | VarDecl { name; _ } -> check string "name" "v" name
              | _ -> fail "expected VarDecl");
        ] );
      ( "multiple_modules",
        [
          test_case "counters independent per module" `Quick (fun () ->
              let prog =
                make_program
                  [
                    make_module [ make_proc "foo" [ var_step "x" (intlit 1) ] ];
                    make_module [ make_proc "bar" [ var_step "x" (intlit 2) ] ];
                  ]
              in
              let result = Sanpou.Alpha_convert.transform prog in
              let am1 = List.nth result 0 in
              let am2 = List.nth result 1 in
              let body1 = get_proc_body am1 "foo" in
              let body2 = get_proc_body am2 "bar" in
              check string "mod1" "x__1" (extract_var_step_name (List.hd body1));
              check string "mod2" "x__1" (extract_var_step_name (List.hd body2)));
        ] );
    ]
