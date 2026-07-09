open Sanpou.Generic_ast

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
let if_block cond body = node (BlockStep (If { cond; body; else_body = None }))
let make_proc name body = node (ProcDef { name; params = []; body })
let make_module items = { mod_name = "m"; items; mod_loc = loc0 }
let make_program modules = modules
let transform_one input = List.hd (Sanpou.Alpha_convert.transform input)

let get_proc_body (m : Sanpou.Resolved_ast.module_def) proc_name =
  let item =
    List.find
      (fun (item : Sanpou.Resolved_ast.item) ->
        match item.desc with
        | ProcDef { name; _ } -> name = proc_name
        | _ -> false)
      m.items
  in
  match item.desc with ProcDef { body; _ } -> body | _ -> assert false

(* Renamed [var] bindings of a body, in pre-order (the old rename table,
   read back from the tree). *)
let rec var_step_idents (steps : Sanpou.Resolved_ast.body) :
    Sanpou.Resolved_ast.ident list =
  List.concat_map
    (fun (step : Sanpou.Resolved_ast.step) ->
      match step.desc with
      | VarStep (i, _) -> [ i ]
      | BlockStep (While { body; _ }) -> var_step_idents body
      | BlockStep (If { body; else_body; _ }) -> (
          var_step_idents body
          @ match else_body with Some b -> var_step_idents b | None -> [])
      | BlockStep (Either arms) -> List.concat_map var_step_idents arms
      | WithStep _ | SimpleStep _ | EmptyStep -> [])
    steps

let local_names m proc_name =
  List.map
    (fun (i : Sanpou.Resolved_ast.ident) -> i.name)
    (var_step_idents (get_proc_body m proc_name))

let extract_var_name (e : Sanpou.Resolved_ast.expr) =
  match e.desc with Var i -> i.name | _ -> failwith "expected Var"

let extract_assign_name (stmt : Sanpou.Resolved_ast.simple_stmt) =
  match stmt.desc with
  | Assign (VarTarget i, _) | Assign (PathTarget (i, _), _) -> i.name
  | _ -> failwith "expected Assign"

let extract_assign_value (stmt : Sanpou.Resolved_ast.simple_stmt) =
  match stmt.desc with
  | Assign (_, value) -> value
  | _ -> failwith "expected Assign"

let extract_var_step_name (step : Sanpou.Resolved_ast.step) =
  match step.desc with
  | VarStep (i, _) -> i.name
  | _ -> failwith "expected VarStep"

let extract_var_step_value (step : Sanpou.Resolved_ast.step) =
  match step.desc with
  | VarStep (_, value) -> value
  | _ -> failwith "expected VarStep"

let extract_simple_stmts (step : Sanpou.Resolved_ast.step) =
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
              let m = transform_one prog in
              let body = get_proc_body m "foo" in
              check string "renamed" "x__1"
                (extract_var_step_name (List.hd body)));
          test_case "original name preserved" `Quick (fun () ->
              let prog =
                make_program
                  [
                    make_module [ make_proc "foo" [ var_step "x" (intlit 5) ] ];
                  ]
              in
              let m = transform_one prog in
              let body = get_proc_body m "foo" in
              match (List.hd body).desc with
              | VarStep (i, _) -> check string "original" "x" i.original
              | _ -> fail "expected VarStep");
          test_case "var value unchanged" `Quick (fun () ->
              let prog =
                make_program
                  [
                    make_module [ make_proc "foo" [ var_step "x" (intlit 42) ] ];
                  ]
              in
              let m = transform_one prog in
              let body = get_proc_body m "foo" in
              check bool "value" true
                (Sanpou.Resolved_ast.equal_expr (intlit 42)
                   (extract_var_step_value (List.hd body))));
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
              let m = transform_one prog in
              let body = get_proc_body m "foo" in
              let stmts = extract_simple_stmts (List.nth body 1) in
              let value = extract_assign_value (List.hd stmts) in
              check string "var renamed" "x__1" (extract_var_name value));
          test_case "reference keeps original for display" `Quick (fun () ->
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
              let m = transform_one prog in
              let body = get_proc_body m "foo" in
              let stmts = extract_simple_stmts (List.nth body 1) in
              match (extract_assign_value (List.hd stmts)).desc with
              | Var i -> check string "original" "x" i.original
              | _ -> fail "expected Var");
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
              let m = transform_one prog in
              let body = get_proc_body m "foo" in
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
              let m = transform_one prog in
              let body = get_proc_body m "foo" in
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
              let m = transform_one prog in
              let idents = var_step_idents (get_proc_body m "foo") in
              check (list string) "locals" [ "x__1"; "y__2" ]
                (List.map
                   (fun (i : Sanpou.Resolved_ast.ident) -> i.name)
                   idents);
              check (list string) "originals" [ "x"; "y" ]
                (List.map
                   (fun (i : Sanpou.Resolved_ast.ident) -> i.original)
                   idents));
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
              let m = transform_one prog in
              check (list string) "empty" [] (local_names m "foo"));
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
              let m = transform_one prog in
              check (list string) "locals" [ "x__1"; "y__2" ]
                (local_names m "foo"));
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
              let m = transform_one prog in
              check (list string) "locals" [ "z__1" ] (local_names m "foo"));
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
              let m = transform_one prog in
              let body = get_proc_body m "foo" in
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
              let m = transform_one prog in
              let body = get_proc_body m "foo" in
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
              let m = transform_one prog in
              let body = get_proc_body m "foo" in
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
              let m = transform_one prog in
              let body = get_proc_body m "foo" in
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
              let m = transform_one prog in
              match (List.hd m.items).desc with
              | ConstDef { name; value } ->
                  check string "name" "c" name;
                  check bool "value" true
                    (Sanpou.Resolved_ast.equal_expr (intlit 1) value)
              | _ -> fail "expected ConstDef");
          test_case "var decl unchanged" `Quick (fun () ->
              let vd =
                node (VarDecl { name = "v"; init = InitValue (intlit 0) })
              in
              let prog = make_program [ make_module [ vd ] ] in
              let m = transform_one prog in
              match (List.hd m.items).desc with
              | VarDecl { name; _ } -> check string "name" "v" name
              | _ -> fail "expected VarDecl");
        ] );
      ( "callee_resolution",
        [
          (* Resolution is lexical and sequential: definitions shadow the
             builtins from their point onward. *)
          test_case "unshadowed builtin name resolves to the builtin" `Quick
            (fun () ->
              let const =
                node
                  (ConstDef
                     { name = "c"; value = node (App ("head", [ var "xs" ])) })
              in
              let m = transform_one (make_program [ make_module [ const ] ]) in
              match (List.hd m.items).desc with
              | ConstDef
                  { value = { desc = Builtin (Sanpou.Builtin.Head, _); _ }; _ }
                ->
                  ()
              | _ -> fail "expected the builtin Head");
          test_case "preceding def shadows the builtin" `Quick (fun () ->
              let fd =
                node
                  (FunDef
                     { name = "head"; params = [ "x" ]; body_expr = var "x" })
              in
              let const =
                node
                  (ConstDef
                     { name = "c"; value = node (App ("head", [ intlit 1 ])) })
              in
              let m =
                transform_one (make_program [ make_module [ fd; const ] ])
              in
              match (List.nth m.items 1).desc with
              | ConstDef
                  {
                    value =
                      { desc = App (Sanpou.Resolved_ast.Fun "head", _); _ };
                    _;
                  } ->
                  ()
              | _ -> fail "expected the user function");
          test_case "use before the shadowing def is the builtin" `Quick
            (fun () ->
              let const =
                node
                  (ConstDef
                     { name = "c"; value = node (App ("head", [ var "xs" ])) })
              in
              let fd =
                node
                  (FunDef
                     { name = "head"; params = [ "x" ]; body_expr = var "x" })
              in
              let m =
                transform_one (make_program [ make_module [ const; fd ] ])
              in
              match (List.hd m.items).desc with
              | ConstDef
                  { value = { desc = Builtin (Sanpou.Builtin.Head, _); _ }; _ }
                ->
                  ()
              | _ -> fail "expected the builtin Head");
          test_case "procedure sees itself for self-recursion" `Quick (fun () ->
              let body =
                [ simple_step [ return_ (node (App ("f", [ intlit 0 ]))) ] ]
              in
              let m =
                transform_one
                  (make_program [ make_module [ make_proc "f" body ] ])
              in
              match get_proc_body m "f" with
              | [
               {
                 desc =
                   SimpleStep
                     [
                       {
                         desc =
                           Return
                             { desc = App (Sanpou.Resolved_ast.Proc "f", _); _ };
                         _;
                       };
                     ];
                 _;
               };
              ] ->
                  ()
              | _ -> fail "expected a Proc self-call");
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
              let m1 = List.nth result 0 in
              let m2 = List.nth result 1 in
              let body1 = get_proc_body m1 "foo" in
              let body2 = get_proc_body m2 "bar" in
              check string "mod1" "x__1" (extract_var_step_name (List.hd body1));
              check string "mod2" "x__1" (extract_var_step_name (List.hd body2)));
        ] );
    ]
