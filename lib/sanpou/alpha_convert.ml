open Generic_ast

(* ===== Alpha conversion: surface AST → resolved AST =====

   Every procedure-local binding (parameter, [var], MapInit binder in a
   procedure body) gets a unique TLA-safe name; each variable position in
   the result carries a [Resolved_ast.ident] pairing that name with the
   source name for display, so no rename table needs to travel with the
   tree. Module-level expressions are not renamed and convert verbatim.

   Applied callees are resolved at the same time, lexically: the module's
   definitions seen so far shadow the builtins, which form the outermost
   scope. Resolution is sequential — a definition is visible only after its
   own item, matching the type checker's environment — so a name resolves
   to [Proc]/[Fun] when a preceding module-level definition binds it, to a
   [Builtin] node otherwise when a builtin has that name, and to [Fun] as a
   last resort (the type checker has already rejected unbound callees). *)

type state = {
  var_name_counter : int ref;
  defs : (id * Resolved_ast.callee) list;
      (* module-level definitions seen so far; non-callables map to [Fun]
         (applying one is a type error caught before this pass) *)
  rename_binders : bool;
      (* binders in module-level expressions keep their source name *)
}

let fresh st name : Resolved_ast.ident =
  if not st.rename_binders then Resolved_ast.ident name
  else begin
    incr st.var_name_counter;
    {
      name = name ^ "__" ^ string_of_int !(st.var_name_counter);
      original = name;
    }
  end

(* [env] maps a source name to its renamed ident; names bound outside the
   procedure (globals, constants, functions) pass through unrenamed. *)
let resolve env name : Resolved_ast.ident =
  match List.assoc_opt name env with
  | Some renamed -> renamed
  | None -> Resolved_ast.ident name

(* ===== Expression alpha conversion ===== *)

let rec alpha_expr env st (e : Surface_ast.expr) : Resolved_ast.expr =
  let desc : (Resolved_ast.ident, Resolved_ast.callee) expr_desc =
    match e.desc with
    | IntLit v -> IntLit v
    | BoolLit b -> BoolLit b
    | StrLit s -> StrLit s
    | Self -> Self
    | Var name -> Var (resolve env name)
    | UnOp (op, rhs) -> UnOp (op, alpha_expr env st rhs)
    | BinOp (op, lhs, rhs) ->
        BinOp (op, alpha_expr env st lhs, alpha_expr env st rhs)
    | App (name, args) -> (
        let args = List.map (alpha_expr env st) args in
        match List.assoc_opt name st.defs with
        | Some callee -> App (callee, args)
        | None -> (
            match Builtin.of_name name with
            | Some b -> Builtin (b, args)
            | None -> App (Resolved_ast.Fun name, args)))
    | Builtin (b, args) -> Builtin (b, List.map (alpha_expr env st) args)
    | Subscript (lhs, index) ->
        Subscript (alpha_expr env st lhs, alpha_expr env st index)
    | Range (lo, hi) -> Range (alpha_expr env st lo, alpha_expr env st hi)
    | MapInit { binder; domain; value } ->
        let binder' = fresh st binder in
        let env' = (binder, binder') :: env in
        MapInit
          {
            binder = binder';
            domain = alpha_expr env st domain;
            value = alpha_expr env' st value;
          }
    | Tuple elems -> Tuple (List.map (alpha_expr env st) elems)
    | Sequence elems -> Sequence (List.map (alpha_expr env st) elems)
    | SetLit elems -> SetLit (List.map (alpha_expr env st) elems)
    | SetComp { binder; domain; pred } ->
        let binder' = fresh st binder in
        let env' = (binder, binder') :: env in
        SetComp
          {
            binder = binder';
            domain = alpha_expr env st domain;
            pred = alpha_expr env' st pred;
          }
    | IfExpr (cond, then_e, else_e) ->
        IfExpr
          ( alpha_expr env st cond,
            alpha_expr env st then_e,
            alpha_expr env st else_e )
    | Quant { quant; binder; domain; body } ->
        let binder' = fresh st binder in
        let env' = (binder, binder') :: env in
        Quant
          {
            quant;
            binder = binder';
            domain = alpha_expr env st domain;
            body = alpha_expr env' st body;
          }
  in
  { desc; loc = e.loc }

let alpha_assign_target env st :
    Surface_ast.assign_target -> Resolved_ast.assign_target = function
  | VarTarget name -> VarTarget (resolve env name)
  | SubscriptTarget (name, indices) ->
      SubscriptTarget (resolve env name, List.map (alpha_expr env st) indices)

(* ===== Step and body alpha conversion ===== *)

let alpha_simple_stmt env st (stmt : Surface_ast.simple_stmt) :
    Resolved_ast.simple_stmt =
  let desc : (Resolved_ast.ident, Resolved_ast.callee) simple_stmt_desc =
    match stmt.desc with
    | Assign (target, value) ->
        Assign (alpha_assign_target env st target, alpha_expr env st value)
    | Call (name, args) -> Call (name, List.map (alpha_expr env st) args)
    | Return value -> Return (alpha_expr env st value)
    | Break -> Break
    | Continue -> Continue
    | Await cond -> Await (alpha_expr env st cond)
    | Assert cond -> Assert (alpha_expr env st cond)
  in
  { desc; loc = stmt.loc }

let rec alpha_step st env (step : Surface_ast.step) : Resolved_ast.step =
  let desc : (Resolved_ast.ident, Resolved_ast.callee) step_desc =
    match step.desc with
    | SimpleStep stmts -> SimpleStep (List.map (alpha_simple_stmt env st) stmts)
    | EmptyStep -> EmptyStep
    | BlockStep stmt -> BlockStep (alpha_block_stmt st env stmt)
    | WithStep { binder; domain; stmts } ->
        let binder' = fresh st binder in
        let env' = (binder, binder') :: env in
        WithStep
          {
            binder = binder';
            domain = alpha_expr env st domain;
            stmts = List.map (alpha_simple_stmt env' st) stmts;
          }
    | VarStep _ -> failwith "VarStep should be handled in alpha_body"
  in
  { desc; loc = step.loc }

and alpha_block_stmt st env :
    Surface_ast.block_stmt ->
    (Resolved_ast.ident, Resolved_ast.callee) block_stmt = function
  | While { cond; body } ->
      While { cond = alpha_expr env st cond; body = alpha_body st env body }
  | If { cond; body; else_body } ->
      If
        {
          cond = alpha_expr env st cond;
          body = alpha_body st env body;
          else_body = Option.map (alpha_body st env) else_body;
        }
  | Either arms -> Either (List.map (alpha_body st env) arms)

and alpha_body st env (steps : Surface_ast.body) : Resolved_ast.body =
  match steps with
  | [] -> []
  | { desc = VarStep (name, value); loc } :: rest ->
      let renamed = fresh st name in
      let alpha_value = alpha_expr env st value in
      let new_env = (name, renamed) :: env in
      { desc = VarStep (renamed, alpha_value); loc }
      :: alpha_body st new_env rest
  | step :: rest -> alpha_step st env step :: alpha_body st env rest

(* ===== Module transformation ===== *)

let transform_module (m : Surface_ast.module_def) : Resolved_ast.module_def =
  let counter = ref 0 in
  (* Module-level expressions are never renamed; their names display as
     themselves. *)
  let module_st defs =
    { var_name_counter = counter; defs; rename_binders = false }
  in
  let items_rev, _ =
    List.fold_left
      (fun (acc, defs) (item : Surface_ast.item) ->
        let mst = module_st defs in
        let plain = alpha_expr [] mst in
        let desc, defs =
          match item.desc with
          | ConstDef { name; value } ->
              ( ConstDef { name; value = plain value },
                (name, Resolved_ast.Fun name) :: defs )
          | PropDef { name; value } ->
              ( PropDef { name; value = plain value },
                (name, Resolved_ast.Fun name) :: defs )
          | FunDef { name; params; body_expr } ->
              (* The body sees the preceding definitions, not the function
                 itself: defs are not recursive, matching the type
                 checker. *)
              ( FunDef { name; params; body_expr = plain body_expr },
                (name, Resolved_ast.Fun name) :: defs )
          | VarDecl { name; init } ->
              let init =
                match init with
                | InitValue value -> InitValue (plain value)
                | InitIn domain -> InitIn (plain domain)
              in
              (VarDecl { name; init }, (name, Resolved_ast.Fun name) :: defs)
          | Process { name; proc; fairness; domain } ->
              ( Process { name; proc; fairness; domain = plain domain },
                defs )
          | ProcDef { name; params; body } ->
              (* The procedure sees itself (self-recursion), again matching
                 the type checker's environment. *)
              let defs = (name, Resolved_ast.Proc name) :: defs in
              let pst =
                { var_name_counter = counter; defs; rename_binders = true }
              in
              let env_rev, params_rev =
                List.fold_left
                  (fun (env_acc, params_acc) param ->
                    let renamed = fresh pst param in
                    ((param, renamed) :: env_acc, renamed :: params_acc))
                  ([], []) params
              in
              let env = List.rev env_rev in
              let params = List.rev params_rev in
              (ProcDef { name; params; body = alpha_body pst env body }, defs)
        in
        ({ desc; loc = item.loc } :: acc, defs))
      ([], []) m.items
  in
  { mod_name = m.mod_name; items = List.rev items_rev; mod_loc = m.mod_loc }

(* ===== Public API ===== *)

let transform (prog : Surface_ast.program) : Resolved_ast.program =
  List.map transform_module prog
