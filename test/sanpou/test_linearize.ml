open Sanpou.Ast
open Sanpou.Ir

let loc0 = { line = 0; col = 0 }
let node desc = { desc; loc = loc0 }
let cl0 = []
let cl1 x = [ x ]
let intlit v = node (IntLit v)
let boollit v = node (BoolLit v)
let var s = node (Var (Sanpou.Resolved_ast.ident s))
let app f args = node (App (f, args))
let assign x e = node (Assign (VarTarget (Sanpou.Resolved_ast.ident x), e))
let call_ f = node (Call (f, []))
let return_ e = node (Return e)
let break_ = node Break
let await_ e = node (Await e)
let tuple0 = node (Tuple [])
let simple_step stmts = node (SimpleStep stmts)
let empty_step = node EmptyStep
let var_step name value = node (VarStep (Sanpou.Resolved_ast.ident name, value))
let while_block cond body = node (BlockStep (While { cond; body }))
let if_block cond body = node (BlockStep (If { cond; body; else_body = None }))
let make_proc name body = node (ProcDef { name; params = []; body })
let make_var name value = node (VarDecl { name; value })
let make_const name value = node (ConstDef { name; value })
let make_fundef name params body_expr = node (FunDef { name; params; body_expr })

let make_process ?(fair = false) name proc lo hi =
  node (Process { name; proc; fair; lo; hi })

let make_module name items = { mod_name = name; items; mod_loc = loc0 }

let linearize_one m =
  let result = Sanpou.Linearize.linearize [ m ] in
  List.hd result

let find_action ir label =
  let all_actions = List.concat_map (fun (p : proc_ir) -> p.actions) ir.procs in
  List.find (fun (a : action) -> a.label = label) all_actions

let find_proc ir name =
  List.find (fun (p : proc_ir) -> p.proc_name = name) ir.procs

let () =
  let open Alcotest in
  run "Linearize"
    [
      ( "simple",
        [
          test_case "return generates one action" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_proc "foo" [ simple_step (cl1 (return_ tuple0)) ];
                    make_process "ps" "foo" (intlit 1) (intlit 2);
                  ]
              in
              let ir = linearize_one m in
              let proc = find_proc ir "foo" in
              check int "one action" 1 (List.length proc.actions);
              let a = List.hd proc.actions in
              check string "label" "L1" a.label;
              (match a.stack_op with
              | StackReturn _ -> ()
              | _ -> fail "expected StackReturn");
              match a.pc_dest with
              | PcReturn -> ()
              | _ -> fail "expected PcReturn");
          test_case "assign generates action" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_var "x" (intlit 0);
                    make_proc "foo"
                      [
                        simple_step (cl1 (assign "x" (intlit 1)));
                        simple_step (cl1 (return_ tuple0));
                      ];
                    make_process "ps" "foo" (intlit 1) (intlit 2);
                  ]
              in
              let ir = linearize_one m in
              let proc = find_proc ir "foo" in
              check int "two actions" 2 (List.length proc.actions);
              let entry = find_action ir proc.entry_label in
              check int "one assignment" 1 (List.length entry.assignments);
              let name =
                match List.hd entry.assignments with
                | AssignVar (name, _) -> name
                | AssignIndex (name, _, _) -> name
              in
              check string "assign x" "x" name);
          test_case "empty step" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_proc "foo" [ empty_step ];
                    make_process "ps" "foo" (intlit 1) (intlit 2);
                  ]
              in
              let ir = linearize_one m in
              let proc = find_proc ir "foo" in
              check int "one action" 1 (List.length proc.actions);
              let a = List.hd proc.actions in
              match a.pc_dest with
              | PcNext "Done" -> ()
              | _ -> fail "expected PcNext Done");
          test_case "fair process preserved in ir" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_proc "foo" [ empty_step ];
                    make_process ~fair:true "ps" "foo" (intlit 1) (intlit 2);
                  ]
              in
              let ir = linearize_one m in
              match ir.processes with
              | [ p ] -> Alcotest.(check bool) "fair" true p.fair
              | _ -> fail "expected one process");
        ] );
      ( "control_flow",
        [
          test_case "while generates branch" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_proc "foo"
                      [
                        while_block (boollit true)
                          [ simple_step (cl1 (return_ tuple0)) ];
                      ];
                    make_process "ps" "foo" (intlit 1) (intlit 2);
                  ]
              in
              let ir = linearize_one m in
              let proc = find_proc ir "foo" in
              let entry = find_action ir proc.entry_label in
              match entry.pc_dest with
              | PcBranch (_, _, "Done") -> ()
              | _ -> fail "expected PcBranch with Done as false branch");
          test_case "if generates branch" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_proc "foo"
                      [
                        if_block (boollit true)
                          [ simple_step (cl1 (return_ tuple0)) ];
                      ];
                    make_process "ps" "foo" (intlit 1) (intlit 2);
                  ]
              in
              let ir = linearize_one m in
              let proc = find_proc ir "foo" in
              let entry = find_action ir proc.entry_label in
              match entry.pc_dest with
              | PcBranch (_, _, "Done") -> ()
              | _ -> fail "expected PcBranch with Done as false branch");
          test_case "break targets after loop" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_proc "foo"
                      [
                        while_block (boollit true) [ simple_step (cl1 break_) ];
                        simple_step (cl1 (return_ tuple0));
                      ];
                    make_process "ps" "foo" (intlit 1) (intlit 2);
                  ]
              in
              let ir = linearize_one m in
              let foo = find_proc ir "foo" in
              let return_action =
                List.find
                  (fun (a : action) ->
                    match a.stack_op with StackReturn _ -> true | _ -> false)
                  foo.actions
              in
              let break_action =
                List.find
                  (fun (a : action) ->
                    a.assignments = [] && a.guard = None
                    && a.stack_op = StackNone
                    &&
                    match a.pc_dest with
                    | PcNext l -> l = return_action.label
                    | _ -> false)
                  foo.actions
              in
              match break_action.pc_dest with
              | PcNext l -> check string "break to return" return_action.label l
              | _ -> fail "expected PcNext");
        ] );
      ( "call",
        [
          test_case "call generates push and pop" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_proc "bar" [ simple_step (cl1 (return_ tuple0)) ];
                    make_proc "foo"
                      [
                        simple_step (cl1 (call_ "bar"));
                        simple_step (cl1 (return_ tuple0));
                      ];
                    make_process "ps" "foo" (intlit 1) (intlit 2);
                  ]
              in
              let ir = linearize_one m in
              let foo = find_proc ir "foo" in
              let push_action =
                List.find
                  (fun (a : action) ->
                    match a.stack_op with StackPush _ -> true | _ -> false)
                  foo.actions
              in
              (match push_action.stack_op with
              | StackPush ("bar", _, []) -> ()
              | _ -> fail "expected StackPush bar");
              let pop_action =
                List.find
                  (fun (a : action) ->
                    match a.stack_op with StackDiscard -> true | _ -> false)
                  foo.actions
              in
              check bool "pop has no assignments" true
                (pop_action.assignments = []));
          test_case "call jumps into the callee" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_proc "bar" [ simple_step (cl1 (return_ tuple0)) ];
                    make_proc "foo"
                      [
                        simple_step (cl1 (call_ "bar"));
                        simple_step (cl1 (return_ tuple0));
                      ];
                    make_process "ps" "foo" (intlit 1) (intlit 2);
                  ]
              in
              let ir = linearize_one m in
              let foo = find_proc ir "foo" in
              let push_action =
                List.find
                  (fun (a : action) ->
                    match a.stack_op with StackPush _ -> true | _ -> false)
                  foo.actions
              in
              match push_action.pc_dest with
              | PcCall callee -> check string "callee" "bar" callee
              | _ -> fail "expected PcCall");
          test_case "call expression captures return value" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_proc "bar" [ simple_step (cl1 (return_ (intlit 42))) ];
                    make_proc "foo"
                      [
                        var_step "x" (app "bar" cl0);
                        simple_step (cl1 (return_ (var "x")));
                      ];
                    make_process "ps" "foo" (intlit 1) (intlit 1);
                  ]
              in
              let ir = linearize_one m in
              check bool "temp local" true
                (List.mem "callRet__1" ir.local_var_decls);
              let foo = find_proc ir "foo" in
              let capture_action =
                List.find
                  (fun (a : action) ->
                    match a.stack_op with
                    | StackPopAssign _ -> true
                    | _ -> false)
                  foo.actions
              in
              (match capture_action.stack_op with
              | StackPopAssign "callRet__1" -> ()
              | _ -> fail "expected StackPopAssign callRet__1");
              let assign_action =
                List.find
                  (fun (a : action) ->
                    match a.assignments with
                    | [ AssignVar ("x", { desc = Var { name = "callRet__1"; _ }; _ }) ] ->
                        true
                    | _ -> false)
                  foo.actions
              in
              check bool "assigns captured return" true
                (assign_action.stack_op = StackNone));
        ] );
      ( "var_step",
        [
          test_case "var becomes assignment action" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_proc "foo"
                      [
                        var_step "x" (intlit 42);
                        simple_step (cl1 (return_ tuple0));
                      ];
                    make_process "ps" "foo" (intlit 1) (intlit 2);
                  ]
              in
              let ir = linearize_one m in
              let proc = find_proc ir "foo" in
              let entry = find_action ir proc.entry_label in
              check int "one assignment" 1 (List.length entry.assignments);
              let name, value =
                match List.hd entry.assignments with
                | AssignVar (name, value) -> (name, value)
                | AssignIndex _ -> failwith "expected AssignVar"
              in
              check string "name" "x" name;
              check bool "value" true (Sanpou.Resolved_ast.equal_expr (intlit 42) value));
        ] );
      ( "module_items",
        [
          test_case "const defs collected" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_const "c" (intlit 5);
                    make_proc "foo" [ simple_step (cl1 (return_ tuple0)) ];
                    make_process "ps" "foo" (intlit 1) (intlit 2);
                  ]
              in
              let ir = linearize_one m in
              check int "one const" 1 (List.length ir.const_defs);
              let name, _ = List.hd ir.const_defs in
              check string "name" "c" name);
          test_case "var decls collected" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_var "x" (intlit 0);
                    make_proc "foo" [ simple_step (cl1 (return_ tuple0)) ];
                    make_process "ps" "foo" (intlit 1) (intlit 2);
                  ]
              in
              let ir = linearize_one m in
              check int "one var" 1 (List.length ir.var_decls);
              let name, _ = List.hd ir.var_decls in
              check string "name" "x" name);
          test_case "fun defs collected" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_fundef "f" [ "x" ] (var "x");
                    make_proc "foo" [ simple_step (cl1 (return_ tuple0)) ];
                    make_process "ps" "foo" (intlit 1) (intlit 2);
                  ]
              in
              let ir = linearize_one m in
              check int "one fun" 1 (List.length ir.fun_defs);
              let name, params, _ = List.hd ir.fun_defs in
              check string "name" "f" name;
              check (list string) "params" [ "x" ] params);
          test_case "processes collected" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_proc "foo" [ simple_step (cl1 (return_ tuple0)) ];
                    make_process "ps" "foo" (intlit 1) (intlit 2);
                  ]
              in
              let ir = linearize_one m in
              check int "one process" 1 (List.length ir.processes);
              let p = List.hd ir.processes in
              check string "name" "ps" p.name;
              check string "proc" "foo" p.proc);
          test_case "local var decls collected from bodies" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_proc "foo"
                      [
                        var_step "x__1" (intlit 1);
                        var_step "y__2" (intlit 2);
                        simple_step (cl1 (return_ tuple0));
                      ];
                    make_process "ps" "foo" (intlit 1) (intlit 2);
                  ]
              in
              let ir = linearize_one m in
              check (list string) "locals" [ "x__1"; "y__2" ] ir.local_var_decls);
          test_case "module name" `Quick (fun () ->
              let m =
                make_module "mymod"
                  [
                    make_proc "foo" [ simple_step (cl1 (return_ tuple0)) ];
                    make_process "ps" "foo" (intlit 1) (intlit 2);
                  ]
              in
              let ir = linearize_one m in
              check string "name" "mymod" ir.name);
        ] );
      ( "labels",
        [
          test_case "labels are sequential" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_proc "foo"
                      [
                        simple_step (cl1 (assign "x" (intlit 1)));
                        simple_step (cl1 (assign "x" (intlit 2)));
                        simple_step (cl1 (return_ tuple0));
                      ];
                    make_process "ps" "foo" (intlit 1) (intlit 2);
                  ]
              in
              let ir = linearize_one m in
              let proc = find_proc ir "foo" in
              let labels =
                List.map (fun (a : action) -> a.label) proc.actions
              in
              check bool "has L1" true (List.mem "L1" labels);
              check bool "has L2" true (List.mem "L2" labels);
              check bool "has L3" true (List.mem "L3" labels));
          test_case "entry label is first step" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_proc "foo"
                      [
                        simple_step (cl1 (assign "x" (intlit 1)));
                        simple_step (cl1 (return_ tuple0));
                      ];
                    make_process "ps" "foo" (intlit 1) (intlit 2);
                  ]
              in
              let ir = linearize_one m in
              let proc = find_proc ir "foo" in
              let entry = find_action ir proc.entry_label in
              check int "entry has assign" 1 (List.length entry.assignments));
        ] );
      ( "await",
        [
          test_case "await sets guard" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_var "x" (intlit 0);
                    make_proc "foo"
                      [
                        simple_step (cl1 (await_ (boollit true)));
                        simple_step (cl1 (return_ tuple0));
                      ];
                    make_process "ps" "foo" (intlit 1) (intlit 2);
                  ]
              in
              let ir = linearize_one m in
              let proc = find_proc ir "foo" in
              let entry = find_action ir proc.entry_label in
              match entry.guard with
              | Some g -> check bool "guard" true (Sanpou.Resolved_ast.equal_expr (boollit true) g)
              | None -> fail "expected guard");
        ] );
    ]
