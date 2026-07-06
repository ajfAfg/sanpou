open Generic_ast

(* ===== Call normalization: resolved AST → normalized AST =====

   Hoists every procedure call out of expression position into a
   [CallBindStep] binding a fresh [callRet__N] temporary. Hoisted steps are
   emitted in evaluation order (left to right; an assignment target's index
   before the assigned value), immediately before the statement they were
   lifted from — except a while condition's calls, which go into the loop's
   [pre] so they re-run on every iteration.

   Procedure calls need an enclosing procedure frame, so in module-level
   expressions (constant/function/variable definitions, process ranges) and
   under a map-initializer binder they are rejected with a located
   [Error]. *)

exception Error of string * Generic_ast.loc

let error message loc = raise (Error (message, loc))

type state = { temp_counter : int ref }

let fresh_temp st : Resolved_ast.ident =
  incr st.temp_counter;
  Resolved_ast.ident ("callRet__" ^ string_of_int !(st.temp_counter))

(* ===== Expressions ===== *)

(* Returns the hoisted call steps in evaluation order and the residual pure
   expression. *)
let rec normalize_expr st (e : Resolved_ast.expr) :
    Normalized_ast.step list * Normalized_ast.expr =
  let at desc : Normalized_ast.expr = { desc; loc = e.loc } in
  match e.desc with
  | IntLit v -> ([], at (IntLit v))
  | BoolLit b -> ([], at (BoolLit b))
  | Var i -> ([], at (Var i))
  | Self -> ([], at Self)
  | UnOp (op, rhs) ->
      let steps, rhs = normalize_expr st rhs in
      (steps, at (UnOp (op, rhs)))
  | BinOp (op, lhs, rhs) ->
      let lhs_steps, lhs = normalize_expr st lhs in
      let rhs_steps, rhs = normalize_expr st rhs in
      (lhs_steps @ rhs_steps, at (BinOp (op, lhs, rhs)))
  | App (Resolved_ast.Proc callee, args) ->
      let steps, args = normalize_expr_list st args in
      let bind = fresh_temp st in
      ( steps
        @ [
            {
              desc = Normalized_ast.CallBindStep { bind; callee; args };
              loc = e.loc;
            };
          ],
        at (Var bind) )
  | App (Resolved_ast.Fun name, args) ->
      let steps, args = normalize_expr_list st args in
      (steps, at (App (Normalized_ast.Fun name, args)))
  | Builtin (b, args) ->
      let steps, args = normalize_expr_list st args in
      (steps, at (Builtin (b, args)))
  | Subscript (lhs, index) ->
      let lhs_steps, lhs = normalize_expr st lhs in
      let index_steps, index = normalize_expr st index in
      (lhs_steps @ index_steps, at (Subscript (lhs, index)))
  | Range (lo, hi) ->
      let lo_steps, lo = normalize_expr st lo in
      let hi_steps, hi = normalize_expr st hi in
      (lo_steps @ hi_steps, at (Range (lo, hi)))
  | MapInit { binder; domain; value } ->
      let domain_steps, domain = normalize_expr st domain in
      let value_steps, value = normalize_expr st value in
      (match value_steps with
      | [] -> ()
      | first :: _ ->
          (* the call would be hoisted out of the binder's scope and run
             once instead of per element *)
          error "procedure calls are not allowed inside a map initializer"
            first.loc);
      (domain_steps, at (MapInit { binder; domain; value }))
  | Tuple elems ->
      let steps, elems = normalize_expr_list st elems in
      (steps, at (Tuple elems))
  | Sequence elems ->
      let steps, elems = normalize_expr_list st elems in
      (steps, at (Sequence elems))
  | SetLit elems ->
      let steps, elems = normalize_expr_list st elems in
      (steps, at (SetLit elems))
  | SetComp { binder; domain; pred } ->
      let domain_steps, domain = normalize_expr st domain in
      let pred_steps, pred = normalize_expr st pred in
      (match pred_steps with
      | [] -> ()
      | first :: _ ->
          (* the call would be hoisted out of the binder's scope and run
             once instead of per element *)
          error "procedure calls are not allowed inside a set comprehension"
            first.loc);
      (domain_steps, at (SetComp { binder; domain; pred }))
  | Quant { quant; binder; domain; body } ->
      let domain_steps, domain = normalize_expr st domain in
      let body_steps, body = normalize_expr st body in
      (match body_steps with
      | [] -> ()
      | first :: _ ->
          (* the call would be hoisted out of the binder's scope and run
             once instead of per element *)
          error "procedure calls are not allowed inside a quantifier body"
            first.loc);
      (domain_steps, at (Quant { quant; binder; domain; body }))
  | IfExpr (cond, then_e, else_e) ->
      let cond_steps, cond = normalize_expr st cond in
      (* Hoisting a call out of a branch would run it unconditionally,
         changing which calls the program performs; only the condition is
         evaluated on every path, so only its calls may be hoisted. Note
         that hoisting weakens the if-expression's atomicity guarantee:
         the call completes in earlier actions, so other processes can
         interleave between it and the step containing the if expression.
         The test and the update are one atomic action only when the
         condition is call-free (see "If expressions and atomicity" in the
         README). *)
      let branch_call_free label e =
        match normalize_expr st e with
        | [], e -> e
        | first :: _, _ ->
            error
              ("procedure calls are not allowed in the " ^ label
             ^ " branch of an if-expression")
              first.loc
      in
      let then_e = branch_call_free "then" then_e in
      let else_e = branch_call_free "else" else_e in
      (cond_steps, at (IfExpr (cond, then_e, else_e)))

and normalize_expr_list st (exprs : Resolved_ast.expr list) :
    Normalized_ast.step list * Normalized_ast.expr list =
  let steps_rev, exprs_rev =
    List.fold_left
      (fun (steps_acc, exprs_acc) expr ->
        let steps, expr = normalize_expr st expr in
        (List.rev_append steps steps_acc, expr :: exprs_acc))
      ([], []) exprs
  in
  (List.rev steps_rev, List.rev exprs_rev)

(* Module-level expressions have no enclosing procedure frame, so a hoist
   is an error rather than a rewrite. *)
let call_free_expr st (e : Resolved_ast.expr) : Normalized_ast.expr =
  match normalize_expr st e with
  | [], e -> e
  | first :: _, _ ->
      error "procedure calls are not allowed in module-level expressions"
        first.loc

(* ===== Statements ===== *)

let normalize_simple_stmt st (stmt : Resolved_ast.simple_stmt) :
    Normalized_ast.step list * Normalized_ast.simple_stmt =
  let at desc : Normalized_ast.simple_stmt = { desc; loc = stmt.loc } in
  match stmt.desc with
  | Assign (target, value) ->
      let target_steps, target =
        match target with
        | VarTarget i -> ([], VarTarget i)
        | SubscriptTarget (i, indices) ->
            let steps, indices = normalize_expr_list st indices in
            (steps, SubscriptTarget (i, indices))
      in
      let value_steps, value = normalize_expr st value in
      (target_steps @ value_steps, at (Assign (target, value)))
  | Call (name, args) ->
      let steps, args = normalize_expr_list st args in
      (steps, at (Call (name, args)))
  | Return value ->
      let steps, value = normalize_expr st value in
      (steps, at (Return value))
  | Break -> ([], at Break)
  | Continue -> ([], at Continue)
  | Await cond ->
      let steps, cond = normalize_expr st cond in
      (steps, at (Await cond))
  | Assert cond ->
      let steps, cond = normalize_expr st cond in
      (steps, at (Assert cond))

(* ===== Steps ===== *)

let rec normalize_step st (step : Resolved_ast.step) : Normalized_ast.step list
    =
  let at desc : Normalized_ast.step = { desc; loc = step.loc } in
  match step.desc with
  | EmptyStep -> [ at Normalized_ast.EmptyStep ]
  | VarStep (i, value) ->
      let steps, value = normalize_expr st value in
      steps @ [ at (Normalized_ast.VarStep (i, value)) ]
  | SimpleStep stmts ->
      let steps_rev, stmts_rev =
        List.fold_left
          (fun (steps_acc, stmts_acc) stmt ->
            let steps, stmt = normalize_simple_stmt st stmt in
            (List.rev_append steps steps_acc, stmt :: stmts_acc))
          ([], []) stmts
      in
      List.rev steps_rev
      @ [ at (Normalized_ast.SimpleStep (List.rev stmts_rev)) ]
  | WithStep { binder; domain; stmts } ->
      let domain_steps, domain = normalize_expr st domain in
      (* The whole body runs in one action under the binder, so a call —
         which spans several actions — cannot appear in it, neither as a
         statement nor inside an expression. *)
      let stmts =
        List.map
          (fun (stmt : Resolved_ast.simple_stmt) ->
            match stmt.desc with
            | Call _ ->
                error "procedure calls are not allowed inside a with statement"
                  stmt.loc
            | _ -> (
                match normalize_simple_stmt st stmt with
                | [], stmt -> stmt
                | first :: _, _ ->
                    error
                      "procedure calls are not allowed inside a with statement"
                      first.loc))
          stmts
      in
      domain_steps
      @ [ at (Normalized_ast.WithStep { binder; domain; stmts }) ]
  | BlockStep (While { cond; body }) ->
      let pre, cond = normalize_expr st cond in
      [
        at
          (Normalized_ast.BlockStep
             (While { pre; cond; body = normalize_body st body }));
      ]
  | BlockStep (If { cond; body; else_body }) ->
      let steps, cond = normalize_expr st cond in
      steps
      @ [
          at
            (Normalized_ast.BlockStep
               (If
                  {
                    cond;
                    body = normalize_body st body;
                    else_body = Option.map (normalize_body st) else_body;
                  }));
        ]
  | BlockStep (Either arms) ->
      [
        at
          (Normalized_ast.BlockStep
             (Either (List.map (normalize_body st) arms)));
      ]

and normalize_body st (steps : Resolved_ast.body) : Normalized_ast.body =
  List.concat_map (normalize_step st) steps

(* ===== Modules ===== *)

let normalize_module (m : Resolved_ast.module_def) : Normalized_ast.module_def =
  let st = { temp_counter = ref 0 } in
  let partition f = List.filter_map f m.items in
  {
    name = m.mod_name;
    const_defs =
      partition (fun (item : Resolved_ast.item) ->
          match item.desc with
          | ConstDef { name; value } -> Some (name, call_free_expr st value)
          | _ -> None);
    prop_defs =
      partition (fun (item : Resolved_ast.item) ->
          match item.desc with
          | PropDef { name; value } -> Some (name, call_free_expr st value)
          | _ -> None);
    fun_defs =
      partition (fun (item : Resolved_ast.item) ->
          match item.desc with
          | FunDef { name; params; body_expr } ->
              Some (name, params, call_free_expr st body_expr)
          | _ -> None);
    var_decls =
      partition (fun (item : Resolved_ast.item) ->
          match item.desc with
          | VarDecl { name; init } ->
              let init =
                match init with
                | InitValue value -> InitValue (call_free_expr st value)
                | InitIn domain -> InitIn (call_free_expr st domain)
              in
              Some (name, init)
          | _ -> None);
    procs =
      partition (fun (item : Resolved_ast.item) ->
          match item.desc with
          | ProcDef { name; params; body } ->
              Some
                ({ name; params; body = normalize_body st body; loc = item.loc }
                  : Normalized_ast.proc_def)
          | _ -> None);
    processes =
      partition (fun (item : Resolved_ast.item) ->
          match item.desc with
          | Process { name; proc; fairness; domain } ->
              Some
                ({
                   name;
                   proc;
                   fairness;
                   domain = call_free_expr st domain;
                   loc = item.loc;
                 }
                  : Normalized_ast.process_def)
          | _ -> None);
  }

(* ===== Public API ===== *)

let normalize (prog : Resolved_ast.program) : Normalized_ast.program =
  List.map normalize_module prog
