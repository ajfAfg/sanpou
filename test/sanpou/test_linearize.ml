open Sanpou.Generic_ast
open Sanpou.Ir

let loc0 = { line = 0; col = 0 }
let node desc = { desc; loc = loc0 }
let ident = Sanpou.Resolved_ast.ident
let cl1 x = [ x ]
let intlit v : Sanpou.Normalized_ast.expr = node (IntLit v)
let boollit v : Sanpou.Normalized_ast.expr = node (BoolLit v)
let var s : Sanpou.Normalized_ast.expr = node (Var (ident s))

let assign x e : Sanpou.Normalized_ast.simple_stmt =
  node (Assign (VarTarget (ident x), e))

let call_ f : Sanpou.Normalized_ast.simple_stmt = node (Call (f, []))
let return_ e : Sanpou.Normalized_ast.simple_stmt = node (Return e)
let break_ : Sanpou.Normalized_ast.simple_stmt = node Break
let await_ e : Sanpou.Normalized_ast.simple_stmt = node (Await e)
let tuple0 : Sanpou.Normalized_ast.expr = node (Tuple [])

let simple_step stmts : Sanpou.Normalized_ast.step =
  node (Sanpou.Normalized_ast.SimpleStep stmts)

let empty_step : Sanpou.Normalized_ast.step =
  node Sanpou.Normalized_ast.EmptyStep

let var_step name value : Sanpou.Normalized_ast.step =
  node (Sanpou.Normalized_ast.VarStep (ident name, value))

let call_bind t f args : Sanpou.Normalized_ast.step =
  node (Sanpou.Normalized_ast.CallBindStep { bind = ident t; callee = f; args })

let while_block ?(pre = []) cond body : Sanpou.Normalized_ast.step =
  node (Sanpou.Normalized_ast.BlockStep (While { pre; cond; body }))

let if_block cond body : Sanpou.Normalized_ast.step =
  node (Sanpou.Normalized_ast.BlockStep (If { cond; body; else_body = None }))

let either_block arms : Sanpou.Normalized_ast.step =
  node (Sanpou.Normalized_ast.BlockStep (Either arms))

let make_proc name body : Sanpou.Normalized_ast.proc_def =
  { name; params = []; body; loc = loc0 }

let make_process ?(fairness = Unfair) name proc lo hi :
    Sanpou.Normalized_ast.process_def =
  { name; proc; fairness; domain = node (Range (lo, hi)); loc = loc0 }

let make_module ?(const_defs = []) ?(fun_defs = []) ?(var_decls = [])
    ?(processes = []) name procs : Sanpou.Normalized_ast.module_def =
  {
    name;
    atoms = [];
    const_defs;
    prop_defs = [];
    fun_defs;
    var_decls = List.map (fun (n, v) -> (n, InitValue v)) var_decls;
    procs;
    processes;
  }

let default_process = make_process "ps" "foo" (intlit 1) (intlit 2)

let linearize_one m =
  let result = Sanpou.Linearize.linearize [ m ] in
  List.hd result

(* Most tests inspect plain actions; unwrap the node layer. *)
let plain_actions (nodes : action_node list) : action list =
  List.filter_map (function Action a -> Some a | Choice _ -> None) nodes

let find_action ir label =
  let all_actions =
    List.concat_map (fun (p : proc_ir) -> plain_actions p.actions) ir.procs
  in
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
                make_module "m" ~processes:[ default_process ]
                  [ make_proc "foo" [ simple_step (cl1 (return_ tuple0)) ] ]
              in
              let ir = linearize_one m in
              let proc = find_proc ir "foo" in
              check int "one action" 1 (List.length proc.actions);
              let a = List.hd (plain_actions proc.actions) in
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
                  ~var_decls:[ ("x", intlit 0) ]
                  ~processes:[ default_process ]
                  [
                    make_proc "foo"
                      [
                        simple_step (cl1 (assign "x" (intlit 1)));
                        simple_step (cl1 (return_ tuple0));
                      ];
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
                | AssignPath (name, _, _) -> name
              in
              check string "assign x" "x" name);
          test_case "empty step" `Quick (fun () ->
              let m =
                make_module "m" ~processes:[ default_process ]
                  [ make_proc "foo" [ empty_step ] ]
              in
              let ir = linearize_one m in
              let proc = find_proc ir "foo" in
              check int "one action" 1 (List.length proc.actions);
              let a = List.hd (plain_actions proc.actions) in
              match a.pc_dest with
              | PcNext "Done" -> ()
              | _ -> fail "expected PcNext Done");
          test_case "fair process preserved in ir" `Quick (fun () ->
              let m =
                make_module "m"
                  ~processes:
                    [
                      make_process ~fairness:WeakFair "ps" "foo" (intlit 1)
                        (intlit 2);
                    ]
                  [ make_proc "foo" [ empty_step ] ]
              in
              let ir = linearize_one m in
              match ir.processes with
              | [ p ] ->
                  Alcotest.(check bool) "fair" true (p.fairness = WeakFair)
              | _ -> fail "expected one process");
        ] );
      ( "either",
        [
          test_case "either becomes a choice of arm entry actions" `Quick
            (fun () ->
              let m =
                make_module "m" ~processes:[ default_process ]
                  [
                    make_proc "foo"
                      [
                        either_block
                          [
                            [ simple_step (cl1 (assign "x" (intlit 1))) ];
                            [
                              simple_step
                                (cl1 (await_ (boollit false)))
                            ];
                          ];
                        simple_step (cl1 (return_ tuple0));
                      ];
                  ]
              in
              let ir = linearize_one m in
              let foo = find_proc ir "foo" in
              let arms =
                List.find_map
                  (function
                    | Choice { arms; _ } -> Some arms | Action _ -> None)
                  foo.actions
                |> Option.get
              in
              check int "two arms" 2 (List.length arms);
              let first = List.nth arms 0 in
              let second = List.nth arms 1 in
              check bool "first arm assigns" true
                (first.assignments
                = [ AssignVar ("x", (intlit 1 : Sanpou.Normalized_ast.expr)) ]);
              check bool "second arm is guarded" true (second.guard <> None);
              (* both arms rejoin at the step after the either *)
              let dest = function
                | PcNext l -> l
                | _ -> Alcotest.fail "expected PcNext"
              in
              check string "arms rejoin" (dest first.pc_dest)
                (dest second.pc_dest));
          test_case "the choice is the entry of its step sequence" `Quick
            (fun () ->
              let m =
                make_module "m" ~processes:[ default_process ]
                  [
                    make_proc "foo"
                      [
                        either_block
                          [
                            [ simple_step (cl1 (assign "x" (intlit 1))) ];
                            [ simple_step (cl1 (assign "x" (intlit 2))) ];
                          ];
                      ];
                  ]
              in
              let ir = linearize_one m in
              let foo = find_proc ir "foo" in
              match List.hd foo.actions with
              | Choice { label; _ } ->
                  check string "entry is the choice" foo.entry_label label
              | Action _ -> fail "expected the choice first");
        ] );
      ( "control_flow",
        [
          test_case "while generates branch" `Quick (fun () ->
              let m =
                make_module "m" ~processes:[ default_process ]
                  [
                    make_proc "foo"
                      [
                        while_block (boollit true)
                          [ simple_step (cl1 (return_ tuple0)) ];
                      ];
                  ]
              in
              let ir = linearize_one m in
              let proc = find_proc ir "foo" in
              let entry = find_action ir proc.entry_label in
              match entry.pc_dest with
              | PcBranch (_, _, "Done") -> ()
              | _ -> fail "expected PcBranch with Done as false branch");
          test_case "while pre runs before every check" `Quick (fun () ->
              let m =
                make_module "m" ~processes:[ default_process ]
                  [
                    make_proc "bar" [ simple_step (cl1 (return_ (intlit 0))) ];
                    make_proc "foo"
                      [
                        while_block
                          ~pre:[ call_bind "callRet__1" "bar" [] ]
                          (var "callRet__1")
                          [ simple_step (cl1 (assign "x" (intlit 1))) ];
                        simple_step (cl1 (return_ tuple0));
                      ];
                  ]
              in
              let ir = linearize_one m in
              let foo = find_proc ir "foo" in
              (* entry is the hoisted call, not the check *)
              let entry = find_action ir foo.entry_label in
              (match entry.stack_op with
              | StackPush ("bar", _, []) -> ()
              | _ -> fail "expected the pre call at loop entry");
              (* the pop leads to the check *)
              let pop =
                List.find
                  (fun (a : action) ->
                    match a.stack_op with
                    | StackPopAssign "callRet__1" -> true
                    | _ -> false)
                  (plain_actions foo.actions)
              in
              let check_label =
                match pop.pc_dest with
                | PcNext l -> l
                | _ -> fail "expected PcNext"
              in
              let check_action = find_action ir check_label in
              match check_action.pc_dest with
              | PcBranch (_, body_entry, _) ->
                  (* the body's back edge re-enters the pre, not the check *)
                  let body_action = find_action ir body_entry in
                  let back_edge =
                    match body_action.pc_dest with
                    | PcNext l -> l
                    | _ -> fail "expected PcNext"
                  in
                  check string "continue re-enters pre" foo.entry_label
                    back_edge
              | _ -> fail "expected the check after the pre");
          test_case "if generates branch" `Quick (fun () ->
              let m =
                make_module "m" ~processes:[ default_process ]
                  [
                    make_proc "foo"
                      [
                        if_block (boollit true)
                          [ simple_step (cl1 (return_ tuple0)) ];
                      ];
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
                make_module "m" ~processes:[ default_process ]
                  [
                    make_proc "foo"
                      [
                        while_block (boollit true) [ simple_step (cl1 break_) ];
                        simple_step (cl1 (return_ tuple0));
                      ];
                  ]
              in
              let ir = linearize_one m in
              let foo = find_proc ir "foo" in
              let return_action =
                List.find
                  (fun (a : action) ->
                    match a.stack_op with StackReturn _ -> true | _ -> false)
                  (plain_actions foo.actions)
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
                  (plain_actions foo.actions)
              in
              match break_action.pc_dest with
              | PcNext l -> check string "break to return" return_action.label l
              | _ -> fail "expected PcNext");
        ] );
      ( "call",
        [
          test_case "call generates push and pop" `Quick (fun () ->
              let m =
                make_module "m" ~processes:[ default_process ]
                  [
                    make_proc "bar" [ simple_step (cl1 (return_ tuple0)) ];
                    make_proc "foo"
                      [
                        simple_step (cl1 (call_ "bar"));
                        simple_step (cl1 (return_ tuple0));
                      ];
                  ]
              in
              let ir = linearize_one m in
              let foo = find_proc ir "foo" in
              let push_action =
                List.find
                  (fun (a : action) ->
                    match a.stack_op with StackPush _ -> true | _ -> false)
                  (plain_actions foo.actions)
              in
              (match push_action.stack_op with
              | StackPush ("bar", _, []) -> ()
              | _ -> fail "expected StackPush bar");
              let pop_action =
                List.find
                  (fun (a : action) ->
                    match a.stack_op with StackDiscard -> true | _ -> false)
                  (plain_actions foo.actions)
              in
              check bool "pop has no assignments" true
                (pop_action.assignments = []));
          test_case "call jumps into the callee" `Quick (fun () ->
              let m =
                make_module "m" ~processes:[ default_process ]
                  [
                    make_proc "bar" [ simple_step (cl1 (return_ tuple0)) ];
                    make_proc "foo"
                      [
                        simple_step (cl1 (call_ "bar"));
                        simple_step (cl1 (return_ tuple0));
                      ];
                  ]
              in
              let ir = linearize_one m in
              let foo = find_proc ir "foo" in
              let push_action =
                List.find
                  (fun (a : action) ->
                    match a.stack_op with StackPush _ -> true | _ -> false)
                  (plain_actions foo.actions)
              in
              match push_action.pc_dest with
              | PcCall callee -> check string "callee" "bar" callee
              | _ -> fail "expected PcCall");
          test_case "call-bind captures the return value" `Quick (fun () ->
              let m =
                make_module "m"
                  ~processes:[ make_process "ps" "foo" (intlit 1) (intlit 1) ]
                  [
                    make_proc "bar" [ simple_step (cl1 (return_ (intlit 42))) ];
                    make_proc "foo"
                      [
                        call_bind "callRet__1" "bar" [];
                        var_step "x" (var "callRet__1");
                        simple_step (cl1 (return_ (var "x")));
                      ];
                  ]
              in
              let ir = linearize_one m in
              check bool "temp local" true
                (List.mem "callRet__1" ir.local_var_decls);
              (match
                 List.find_opt
                   (fun (v : var_info) -> v.tla_name = "callRet__1")
                   ir.var_infos
               with
              | Some v -> check bool "callret kind" true (v.kind = CallRet "bar")
              | None -> fail "expected var_info for the temp");
              let foo = find_proc ir "foo" in
              let capture_action =
                List.find
                  (fun (a : action) ->
                    match a.stack_op with
                    | StackPopAssign _ -> true
                    | _ -> false)
                  (plain_actions foo.actions)
              in
              (match capture_action.stack_op with
              | StackPopAssign "callRet__1" -> ()
              | _ -> fail "expected StackPopAssign callRet__1");
              let assign_action =
                List.find
                  (fun (a : action) ->
                    match a.assignments with
                    | [
                     AssignVar
                       ("x", { desc = Var { name = "callRet__1"; _ }; _ });
                    ] ->
                        true
                    | _ -> false)
                  (plain_actions foo.actions)
              in
              check bool "assigns captured return" true
                (assign_action.stack_op = StackNone));
        ] );
      ( "var_step",
        [
          test_case "var becomes assignment action" `Quick (fun () ->
              let m =
                make_module "m" ~processes:[ default_process ]
                  [
                    make_proc "foo"
                      [
                        var_step "x" (intlit 42);
                        simple_step (cl1 (return_ tuple0));
                      ];
                  ]
              in
              let ir = linearize_one m in
              let proc = find_proc ir "foo" in
              let entry = find_action ir proc.entry_label in
              check int "one assignment" 1 (List.length entry.assignments);
              let name, value =
                match List.hd entry.assignments with
                | AssignVar (name, value) -> (name, value)
                | AssignPath _ -> failwith "expected AssignVar"
              in
              check string "name" "x" name;
              check bool "value" true
                (Sanpou.Normalized_ast.equal_expr (intlit 42) value));
        ] );
      ( "module_items",
        [
          test_case "module items pass through" `Quick (fun () ->
              let m =
                make_module "mymod"
                  ~const_defs:[ ("c", intlit 5) ]
                  ~fun_defs:[ ("f", [ "x" ], var "x") ]
                  ~var_decls:[ ("x", intlit 0) ]
                  ~processes:[ default_process ]
                  [ make_proc "foo" [ simple_step (cl1 (return_ tuple0)) ] ]
              in
              let ir = linearize_one m in
              check string "name" "mymod" ir.name;
              check int "one const" 1 (List.length ir.const_defs);
              check int "one fun" 1 (List.length ir.fun_defs);
              check int "one var" 1 (List.length ir.var_decls);
              check int "one process" 1 (List.length ir.processes);
              let p = List.hd ir.processes in
              check string "process name" "ps" p.name;
              check string "process proc" "foo" p.proc);
          test_case "local var decls collected from bodies" `Quick (fun () ->
              let m =
                make_module "m" ~processes:[ default_process ]
                  [
                    make_proc "foo"
                      [
                        var_step "x__1" (intlit 1);
                        var_step "y__2" (intlit 2);
                        simple_step (cl1 (return_ tuple0));
                      ];
                  ]
              in
              let ir = linearize_one m in
              check (list string) "locals" [ "x__1"; "y__2" ] ir.local_var_decls);
        ] );
      ( "labels",
        [
          test_case "labels are sequential" `Quick (fun () ->
              let m =
                make_module "m" ~processes:[ default_process ]
                  [
                    make_proc "foo"
                      [
                        simple_step (cl1 (assign "x" (intlit 1)));
                        simple_step (cl1 (assign "x" (intlit 2)));
                        simple_step (cl1 (return_ tuple0));
                      ];
                  ]
              in
              let ir = linearize_one m in
              let proc = find_proc ir "foo" in
              let labels = List.map node_label proc.actions in
              check bool "has L1" true (List.mem "L1" labels);
              check bool "has L2" true (List.mem "L2" labels);
              check bool "has L3" true (List.mem "L3" labels));
          test_case "entry label is first step" `Quick (fun () ->
              let m =
                make_module "m" ~processes:[ default_process ]
                  [
                    make_proc "foo"
                      [
                        simple_step (cl1 (assign "x" (intlit 1)));
                        simple_step (cl1 (return_ tuple0));
                      ];
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
                  ~var_decls:[ ("x", intlit 0) ]
                  ~processes:[ default_process ]
                  [
                    make_proc "foo"
                      [
                        simple_step (cl1 (await_ (boollit true)));
                        simple_step (cl1 (return_ tuple0));
                      ];
                  ]
              in
              let ir = linearize_one m in
              let proc = find_proc ir "foo" in
              let entry = find_action ir proc.entry_label in
              match entry.guard with
              | Some g ->
                  check bool "guard" true
                    (Sanpou.Normalized_ast.equal_expr (boollit true) g)
              | None -> fail "expected guard");
        ] );
    ]
