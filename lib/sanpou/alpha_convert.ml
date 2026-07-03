open Ast

(* ===== Alpha conversion: surface AST → resolved AST =====

   Every procedure-local binding (parameter, [var], MapInit binder in a
   procedure body) gets a unique TLA-safe name; each variable position in
   the result carries a [Resolved_ast.ident] pairing that name with the
   source name for display, so no rename table needs to travel with the
   tree. Module-level expressions are not renamed and convert verbatim. *)

type state = { var_name_counter : int ref }

let create_state () = { var_name_counter = ref 0 }

let fresh st name : Resolved_ast.ident =
  incr st.var_name_counter;
  { name = name ^ "__" ^ string_of_int !(st.var_name_counter); original = name }

(* [env] maps a source name to its renamed ident; names bound outside the
   procedure (globals, constants, functions) pass through unrenamed. *)
let resolve env name : Resolved_ast.ident =
  match List.assoc_opt name env with
  | Some renamed -> renamed
  | None -> Resolved_ast.ident name

(* ===== Expression alpha conversion ===== *)

let rec alpha_expr env st (e : id expr) : Resolved_ast.expr =
  let desc : Resolved_ast.ident expr_desc =
    match e.desc with
    | IntLit v -> IntLit v
    | BoolLit b -> BoolLit b
    | Self -> Self
    | Var name -> Var (resolve env name)
    | UnOp (op, rhs) -> UnOp (op, alpha_expr env st rhs)
    | BinOp (op, lhs, rhs) ->
        BinOp (op, alpha_expr env st lhs, alpha_expr env st rhs)
    | App (name, args) -> App (name, List.map (alpha_expr env st) args)
    | Builtin (b, args) -> Builtin (b, List.map (alpha_expr env st) args)
    | Subscript (lhs, index) ->
        Subscript (alpha_expr env st lhs, alpha_expr env st index)
    | MapInit { binder; lo; hi; value } ->
        let binder' = fresh st binder in
        let env' = (binder, binder') :: env in
        MapInit
          {
            binder = binder';
            lo = alpha_expr env st lo;
            hi = alpha_expr env st hi;
            value = alpha_expr env' st value;
          }
    | Tuple elems -> Tuple (List.map (alpha_expr env st) elems)
    | Sequence elems -> Sequence (List.map (alpha_expr env st) elems)
  in
  { desc; loc = e.loc }

let alpha_assign_target env st :
    id assign_target -> Resolved_ast.assign_target = function
  | VarTarget name -> VarTarget (resolve env name)
  | SubscriptTarget (name, index) ->
      SubscriptTarget (resolve env name, alpha_expr env st index)

(* ===== Step and body alpha conversion ===== *)

let rec alpha_step st env (step : id step) : Resolved_ast.step =
  let desc : Resolved_ast.ident step_desc =
    match step.desc with
    | SimpleStep stmts ->
        let alpha_simple_stmt (stmt : id simple_stmt) : Resolved_ast.simple_stmt
            =
          let desc : Resolved_ast.ident simple_stmt_desc =
            match stmt.desc with
            | Assign (target, value) ->
                Assign
                  (alpha_assign_target env st target, alpha_expr env st value)
            | Call (name, args) ->
                Call (name, List.map (alpha_expr env st) args)
            | Return value -> Return (alpha_expr env st value)
            | Break -> Break
            | Continue -> Continue
            | Await cond -> Await (alpha_expr env st cond)
          in
          { desc; loc = stmt.loc }
        in
        SimpleStep (List.map alpha_simple_stmt stmts)
    | EmptyStep -> EmptyStep
    | BlockStep stmt -> BlockStep (alpha_block_stmt st env stmt)
    | VarStep _ -> failwith "VarStep should be handled in alpha_body"
  in
  { desc; loc = step.loc }

and alpha_block_stmt st env : id block_stmt -> Resolved_ast.ident block_stmt =
  function
  | While { cond; body } ->
      While { cond = alpha_expr env st cond; body = alpha_body st env body }
  | If { cond; body; else_body } ->
      If
        {
          cond = alpha_expr env st cond;
          body = alpha_body st env body;
          else_body = Option.map (alpha_body st env) else_body;
        }

and alpha_body st env (steps : id body) : Resolved_ast.body =
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

let transform_module (m : id module_def) : Resolved_ast.module_def =
  let st = create_state () in
  (* Module-level expressions are never renamed; their names display as
     themselves. *)
  let plain = Ast.map_expr Resolved_ast.ident in
  let items =
    List.map
      (fun (item : id item) ->
        let desc : Resolved_ast.ident item_desc =
          match item.desc with
          | ConstDef { name; value } -> ConstDef { name; value = plain value }
          | FunDef { name; params; body_expr } ->
              FunDef { name; params; body_expr = plain body_expr }
          | VarDecl { name; value } -> VarDecl { name; value = plain value }
          | Process { name; proc; fair; lo; hi } ->
              Process { name; proc; fair; lo = plain lo; hi = plain hi }
          | ProcDef { name; params; body } ->
              let env_rev, params_rev =
                List.fold_left
                  (fun (env_acc, params_acc) param ->
                    let renamed = fresh st param in
                    ((param, renamed) :: env_acc, renamed :: params_acc))
                  ([], []) params
              in
              let env = List.rev env_rev in
              let params = List.rev params_rev in
              ProcDef { name; params; body = alpha_body st env body }
        in
        { desc; loc = item.loc })
      m.items
  in
  { mod_name = m.mod_name; items; mod_loc = m.mod_loc }

(* ===== Public API ===== *)

let transform (prog : id program) : Resolved_ast.program =
  List.map transform_module prog
