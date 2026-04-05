open Sanpou.Cst
open Sanpou.Ir

let n = ""
let loc0 = { line = 0; col = 0 }
let cl0 = { items = []; commas = [] }
let cl1 x = { items = [ x ]; commas = [] }
let intlit v = IntLit { t = n; value = v }
let boollit v = BoolLit { t = n; value = v }
let var s = Var { t = n; name = s }
let assign x e = Assign { name_t = n; name = x; eq_t = n; value = e }
let call_ f = Call { name_t = n; name = f; lp = n; args = cl0; rp = n }
let return_ e = Return { t = n; value = e }
let break_ = Break { t = n }
let await_ e = Await { t = n; cond = e }
let tuple0 = Tuple { lp = n; elems = cl0; trailing_comma = None; rp = n }
let simple_step stmts = SimpleStep { loc = loc0; stmts; semi_t = n }
let empty_step = EmptyStep { loc = loc0; semi_t = n }

let let_step name value =
  LetStep
    { loc = loc0; let_t = n; name_t = n; name; eq_t = n; value; semi_t = n }

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

let make_var name value =
  VarDecl { let_t = n; name_t = n; name; eq_t = n; value; semi_t = n }

let make_const name value =
  ConstDef { def_t = n; name_t = n; name; eq_t = n; value; semi_t = n }

let make_fundef name params body_expr =
  FunDef
    {
      def_t = n;
      name_t = n;
      name;
      lp = n;
      params = { items = List.map (fun p -> (n, p)) params; commas = [] };
      rp = n;
      eq_t = n;
      body_expr;
      semi_t = n;
    }

let make_process ?(fair = false) name proc lo hi =
  Process
    {
      fair_t = (if fair then Some n else None);
      process_t = n;
      name_t = n;
      name;
      eq_t = n;
      proc_t = n;
      proc;
      in_t = n;
      lo;
      dotdot_t = n;
      hi;
      semi_t = n;
    }

let make_module name items =
  { mod_t = n; name_t = n; mod_name = name; lb = n; items; rb = n }

let make_alpha_module cst local_vars : Sanpou.Alpha_convert.alpha_module =
  { cst; local_vars }

let linearize_one am =
  let result = Sanpou.Linearize.linearize [ am ] in
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
              let ir = linearize_one (make_alpha_module m []) in
              let proc = find_proc ir "foo" in
              check int "one action" 1 (List.length proc.actions);
              let a = List.hd proc.actions in
              check string "label" "L1" a.label;
              (match a.stack_op with
              | StackReturn _ -> ()
              | _ -> fail "expected StackReturn");
              match a.pc_dest with
              | PcNext "__return__" -> ()
              | _ -> fail "expected PcNext __return__");
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
              let ir = linearize_one (make_alpha_module m []) in
              let proc = find_proc ir "foo" in
              check int "two actions" 2 (List.length proc.actions);
              let entry = find_action ir proc.entry_label in
              check int "one assignment" 1 (List.length entry.assignments);
              let name, _ = List.hd entry.assignments in
              check string "assign x" "x" name);
          test_case "empty step" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_proc "foo" [ empty_step ];
                    make_process "ps" "foo" (intlit 1) (intlit 2);
                  ]
              in
              let ir = linearize_one (make_alpha_module m []) in
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
              let ir = linearize_one (make_alpha_module m []) in
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
              let ir = linearize_one (make_alpha_module m []) in
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
              let ir = linearize_one (make_alpha_module m []) in
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
              let ir = linearize_one (make_alpha_module m []) in
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
              let ir = linearize_one (make_alpha_module m []) in
              let foo = find_proc ir "foo" in
              let push_action =
                List.find
                  (fun (a : action) ->
                    match a.stack_op with StackPush _ -> true | _ -> false)
                  foo.actions
              in
              (match push_action.stack_op with
              | StackPush ("bar", _) -> ()
              | _ -> fail "expected StackPush bar");
              let pop_action =
                List.find
                  (fun (a : action) ->
                    match a.stack_op with StackDiscard -> true | _ -> false)
                  foo.actions
              in
              check bool "pop has no assignments" true
                (pop_action.assignments = []));
          test_case "call target resolved" `Quick (fun () ->
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
              let ir = linearize_one (make_alpha_module m []) in
              let bar = find_proc ir "bar" in
              let foo = find_proc ir "foo" in
              let push_action =
                List.find
                  (fun (a : action) ->
                    match a.stack_op with StackPush _ -> true | _ -> false)
                  foo.actions
              in
              match push_action.pc_dest with
              | PcNext l -> check string "resolved" bar.entry_label l
              | _ -> fail "expected PcNext");
        ] );
      ( "let_step",
        [
          test_case "let becomes assignment action" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_proc "foo"
                      [
                        let_step "x" (intlit 42);
                        simple_step (cl1 (return_ tuple0));
                      ];
                    make_process "ps" "foo" (intlit 1) (intlit 2);
                  ]
              in
              let ir = linearize_one (make_alpha_module m [ "x" ]) in
              let proc = find_proc ir "foo" in
              let entry = find_action ir proc.entry_label in
              check int "one assignment" 1 (List.length entry.assignments);
              let name, value = List.hd entry.assignments in
              check string "name" "x" name;
              check bool "value" true (equal_expr (intlit 42) value));
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
              let ir = linearize_one (make_alpha_module m []) in
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
              let ir = linearize_one (make_alpha_module m []) in
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
              let ir = linearize_one (make_alpha_module m []) in
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
              let ir = linearize_one (make_alpha_module m []) in
              check int "one process" 1 (List.length ir.processes);
              let p = List.hd ir.processes in
              check string "name" "ps" p.name;
              check string "proc" "foo" p.proc);
          test_case "local var decls passed through" `Quick (fun () ->
              let m =
                make_module "m"
                  [
                    make_proc "foo" [ simple_step (cl1 (return_ tuple0)) ];
                    make_process "ps" "foo" (intlit 1) (intlit 2);
                  ]
              in
              let ir = linearize_one (make_alpha_module m [ "x__1"; "y__2" ]) in
              check (list string) "locals" [ "x__1"; "y__2" ] ir.local_var_decls);
          test_case "module name" `Quick (fun () ->
              let m =
                make_module "mymod"
                  [
                    make_proc "foo" [ simple_step (cl1 (return_ tuple0)) ];
                    make_process "ps" "foo" (intlit 1) (intlit 2);
                  ]
              in
              let ir = linearize_one (make_alpha_module m []) in
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
              let ir = linearize_one (make_alpha_module m []) in
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
              let ir = linearize_one (make_alpha_module m []) in
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
              let ir = linearize_one (make_alpha_module m []) in
              let proc = find_proc ir "foo" in
              let entry = find_action ir proc.entry_label in
              match entry.guard with
              | Some g -> check bool "guard" true (equal_expr (boollit true) g)
              | None -> fail "expected guard");
        ] );
    ]
