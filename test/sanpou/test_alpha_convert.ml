open Sanpou.Cst

let n = ""
let loc0 = { line = 0; col = 0 }
let cl0 = { items = []; commas = [] }
let cl1 x = { items = [ x ]; commas = [] }
let intlit v = IntLit { t = n; value = v }
let boollit v = BoolLit { t = n; value = v }
let var s = Var { t = n; name = s }
let binop op l r = BinOp { lhs = l; op_t = n; op; rhs = r }
let assign x e = Assign { name_t = n; name = x; eq_t = n; value = e }
let return_ e = Return { t = n; value = e }
let await_ e = Await { t = n; cond = e }
let simple_step stmts = SimpleStep { loc = loc0; stmts; semi_t = n }

let var_step name value =
  VarStep
    { loc = loc0; var_t = n; name_t = n; name; eq_t = n; value; semi_t = n }

let while_block cond body =
  BlockStep
    {
      loc = loc0;
      stmt = While { while_t = n; lp = n; cond; rp = n; lb = n; body; rb = n };
    }

let if_block cond body =
  BlockStep
    {
      loc = loc0;
      stmt = If { if_t = n; lp = n; cond; rp = n; lb = n; body; rb = n };
    }

let make_proc name body =
  ProcDef
    {
      fn_t = n;
      name_t = n;
      name;
      lp = n;
      params = cl0;
      rp = n;
      lb = n;
      body;
      rb = n;
    }

let make_module items =
  { mod_t = n; name_t = n; mod_name = "m"; lb = n; items; rb = n }

let make_program modules = { modules; eof_t = n }

let transform_one input =
  let result = Sanpou.Alpha_convert.transform input in
  List.hd result

let get_proc_body (am : Sanpou.Alpha_convert.alpha_module) proc_name =
  let item =
    List.find
      (fun (item : item) ->
        match item with ProcDef { name; _ } -> name = proc_name | _ -> false)
      am.cst.items
  in
  match item with ProcDef { body; _ } -> body | _ -> assert false

let extract_var_name = function
  | Var { name; _ } -> name
  | _ -> failwith "expected Var"

let extract_assign_name = function
  | Assign { name; _ } -> name
  | _ -> failwith "expected Assign"

let extract_assign_value = function
  | Assign { value; _ } -> value
  | _ -> failwith "expected Assign"

let extract_var_step_name = function
  | VarStep { name; _ } -> name
  | _ -> failwith "expected VarStep"

let extract_var_step_value = function
  | VarStep { value; _ } -> value
  | _ -> failwith "expected VarStep"

let extract_simple_stmts = function
  | SimpleStep { stmts; _ } -> stmts.items
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
              match List.nth body 1 with
              | BlockStep { stmt = While { body = while_body; _ }; _ } ->
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
              check (list string) "locals" [ "x__1"; "y__2" ] am.local_vars);
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
              check (list string) "empty" [] am.local_vars);
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
              check (list string) "locals" [ "x__1"; "y__2" ] am.local_vars);
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
              check (list string) "locals" [ "z__1" ] am.local_vars);
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
              match value with
              | BinOp { lhs; _ } ->
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
              match List.hd stmts with
              | Await { cond; _ } ->
                  check string "cond" "x__1" (extract_var_name cond)
              | _ -> fail "expected Await");
        ] );
      ( "non_proc_items",
        [
          test_case "const def unchanged" `Quick (fun () ->
              let const =
                ConstDef
                  {
                    def_t = n;
                    name_t = n;
                    name = "c";
                    eq_t = n;
                    value = intlit 1;
                    semi_t = n;
                  }
              in
              let prog = make_program [ make_module [ const ] ] in
              let am = transform_one prog in
              match List.hd am.cst.items with
              | ConstDef { name; value; _ } ->
                  check string "name" "c" name;
                  check bool "value" true (equal_expr (intlit 1) value)
              | _ -> fail "expected ConstDef");
          test_case "var decl unchanged" `Quick (fun () ->
              let vd =
                VarDecl
                  {
                    var_t = n;
                    name_t = n;
                    name = "v";
                    eq_t = n;
                    value = intlit 0;
                    semi_t = n;
                  }
              in
              let prog = make_program [ make_module [ vd ] ] in
              let am = transform_one prog in
              match List.hd am.cst.items with
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
