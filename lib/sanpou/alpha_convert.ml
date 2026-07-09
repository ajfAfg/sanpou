open Generic_ast

(* ===== Alpha conversion: surface AST → resolved AST =====

   Every procedure-local binding (parameter, [var], MapInit binder in a
   procedure body) gets a unique TLA-safe name; each variable position in
   the result carries a [Resolved_ast.ident] pairing that name with the
   source name for display, so no rename table needs to travel with the
   tree.

   Module-level binders (fun-def parameters, quantifier/comprehension/map
   binders in module-level expressions) keep their source name — they are
   user-visible in the emitted spec — unless keeping it would emit invalid
   TLA+: TLA+ allows no shadowing at all, so a bound identifier or operator
   parameter that reuses any module-level name (all of which end up as
   top-level TLA+ symbols, VARIABLES included, regardless of source order)
   or an enclosing binder's name is renamed with the same [__N] scheme.

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
      (* procedure bodies rename every binder; module-level expressions
         rename only on collision *)
  module_names : id list;
      (* every module-level name of the enclosing module, position
         independent: they all become top-level TLA+ symbols *)
}

(* A fresh [__N] name that collides with no module-level name. *)
let rename st name : Resolved_ast.ident =
  let rec next () =
    incr st.var_name_counter;
    let candidate = name ^ "__" ^ string_of_int !(st.var_name_counter) in
    if List.mem candidate st.module_names then next ()
    else { Resolved_ast.name = candidate; original = name }
  in
  next ()

(* [env] holds the bindings in scope at the binder site: its domain is
   exactly the enclosing binder (and, in procedures, local) names. Binders
   named like an emitter-generated TLA+ name (pc, Len, ...) collide just
   like module-level names do — TLA+ allows no shadowing — so they are
   renamed too (declarations of those names are rejected by Check_scope; a
   binder is fine once renamed). *)
let fresh st env name : Resolved_ast.ident =
  if
    st.rename_binders
    || List.mem name st.module_names
    || Reserved_names.is_reserved name
    || List.mem_assoc name env
  then rename st name
  else Resolved_ast.ident name

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
    | AtomLit a -> AtomLit a
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
    | Field (record, label) -> Field (alpha_expr env st record, label)
    | Record fields ->
        Record (List.map (fun (l, e) -> (l, alpha_expr env st e)) fields)
    | Range (lo, hi) -> Range (alpha_expr env st lo, alpha_expr env st hi)
    | MapInit { binder; domain; value } ->
        let binder' = fresh st env binder in
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
        let binder' = fresh st env binder in
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
        let binder' = fresh st env binder in
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

let alpha_accessor env st : Surface_ast.accessor -> Resolved_ast.accessor =
  function
  | AccIndex e -> AccIndex (alpha_expr env st e)
  | AccField f -> AccField f

let alpha_assign_target env st :
    Surface_ast.assign_target -> Resolved_ast.assign_target = function
  | VarTarget name -> VarTarget (resolve env name)
  | PathTarget (name, path) ->
      PathTarget (resolve env name, List.map (alpha_accessor env st) path)

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
        let binder' = fresh st env binder in
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
      let renamed = fresh st env name in
      let alpha_value = alpha_expr env st value in
      let new_env = (name, renamed) :: env in
      { desc = VarStep (renamed, alpha_value); loc }
      :: alpha_body st new_env rest
  | step :: rest -> alpha_step st env step :: alpha_body st env rest

(* ===== Module transformation ===== *)

let transform_module (m : Surface_ast.module_def) : Resolved_ast.module_def =
  let counter = ref 0 in
  (* All module-level names, collected up front: TLA+ scopes them over the
     whole module (VARIABLES and defs alike), so a binder collides with a
     name declared *after* it in the source just the same. Duplicates are
     kept — their multiplicity drives the shadow renaming below. *)
  let declared_names =
    List.concat_map
      (fun (item : Surface_ast.item) ->
        match item.desc with
        | ConstDef { name; _ }
        | PropDef { name; _ }
        | FunDef { name; _ }
        | VarDecl { name; _ }
        | ProcDef { name; _ }
        | Process { name; _ } ->
            [ name ])
      m.items
  in
  (* Every atom literal used in the module claims its text as a TLA+ name
     (the model value's identity), so atom texts join the collision set:
     a declaration or binder of the same name renames apart, never the
     atom. Deduplicated, so [total_count] still counts declarations plus
     at most one atom claim. *)
  let atom_texts =
    let texts = ref [] in
    let rec walk (e : Surface_ast.expr) =
      match e.desc with
      | AtomLit a -> if not (List.mem a !texts) then texts := a :: !texts
      | IntLit _ | BoolLit _ | StrLit _ | Var _ | Self -> ()
      | UnOp (_, e1) -> walk e1
      | BinOp (_, a, b) | Range (a, b) | Subscript (a, b) ->
          walk a;
          walk b
      | Field (r, _) -> walk r
      | Record fields -> List.iter (fun (_, e) -> walk e) fields
      | App (_, args)
      | Builtin (_, args)
      | Tuple args
      | Sequence args
      | SetLit args ->
          List.iter walk args
      | MapInit { domain; value; _ } ->
          walk domain;
          walk value
      | SetComp { domain; pred; _ } ->
          walk domain;
          walk pred
      | IfExpr (a, b, c) ->
          walk a;
          walk b;
          walk c
      | Quant { domain; body; _ } ->
          walk domain;
          walk body
    in
    let walk_stmt (stmt : Surface_ast.simple_stmt) =
      match stmt.desc with
      | Assign (target, value) ->
          (match target with
          | VarTarget _ -> ()
          | PathTarget (_, path) ->
              List.iter
                (function AccIndex e -> walk e | AccField _ -> ())
                path);
          walk value
      | Call (_, args) -> List.iter walk args
      | Return e | Await e | Assert e -> walk e
      | Break | Continue -> ()
    in
    let rec walk_body (steps : Surface_ast.body) =
      List.iter
        (fun (step : Surface_ast.step) ->
          match step.desc with
          | SimpleStep stmts -> List.iter walk_stmt stmts
          | EmptyStep -> ()
          | VarStep (_, value) -> walk value
          | WithStep { domain; stmts; _ } ->
              walk domain;
              List.iter walk_stmt stmts
          | BlockStep (While { cond; body }) ->
              walk cond;
              walk_body body
          | BlockStep (If { cond; body; else_body }) ->
              walk cond;
              walk_body body;
              Option.iter walk_body else_body
          | BlockStep (Either arms) -> List.iter walk_body arms)
        steps
    in
    List.iter
      (fun (item : Surface_ast.item) ->
        match item.desc with
        | ConstDef { value; _ } | PropDef { value; _ } -> walk value
        | FunDef { body_expr; _ } -> walk body_expr
        | VarDecl { init = InitValue e; _ } | VarDecl { init = InitIn e; _ } ->
            walk e
        | ProcDef { body; _ } -> walk_body body
        | Process { domain; _ } -> walk domain)
      m.items;
    !texts
  in
  let module_names = declared_names @ atom_texts in
  let total_count name = List.length (List.filter (( = ) name) module_names) in
  (* Module-level binders keep their source name unless it collides (see
     [fresh]); their names display as themselves. *)
  let module_st defs =
    { var_name_counter = counter; defs; rename_binders = false; module_names }
  in
  (* Module-level declarations shadow sequentially like any other binding,
     but the emitted TLA+ namespace is flat, so shadowed declarations are
     renamed apart: every occurrence of a name except the last gets a
     [__N] name, and the last keeps the bare one — the binding visible at
     the end of the module is the one the emitted spec, traces, and the
     sidecar config refer to by its source name. (Atoms never rename:
     typing keeps their names module-wide unique.) [menv] maps each source
     name to its current ident so later references resolve to the right
     binding; [seen] counts occurrences processed so far. *)
  let declare seen name =
    let n =
      (match List.assoc_opt name seen with Some n -> n | None -> 0) + 1
    in
    let seen = (name, n) :: List.remove_assoc name seen in
    let ident =
      if n < total_count name then rename (module_st []) name
      else Resolved_ast.ident name
    in
    (seen, ident)
  in
  let items_rev, _, _, _ =
    List.fold_left
      (fun (acc, defs, menv, seen) (item : Surface_ast.item) ->
        let mst = module_st defs in
        let plain = alpha_expr menv mst in
        let desc, defs, menv, seen =
          match item.desc with
          | ConstDef { name; value } ->
              (* the value sees the preceding bindings, not the new one *)
              let value = plain value in
              let seen, id_ = declare seen name in
              ( ConstDef { name = id_.name; value },
                (name, Resolved_ast.Fun id_.name) :: defs,
                (name, id_) :: menv,
                seen )
          | PropDef { name; value } ->
              let value = plain value in
              let seen, id_ = declare seen name in
              ( PropDef { name = id_.name; value },
                (name, Resolved_ast.Fun id_.name) :: defs,
                (name, id_) :: menv,
                seen )
          | FunDef { name; params; body_expr } ->
              (* The body sees the preceding definitions, not the function
                 itself: defs are not recursive, matching the type checker.
                 Params become TLA+ operator parameters, so like any
                 module-level binder they are renamed on collision. *)
              let env_rev, params_rev =
                List.fold_left
                  (fun (env_acc, params_acc) param ->
                    let renamed = fresh (module_st defs) env_acc param in
                    ( (param, renamed) :: env_acc,
                      renamed.Resolved_ast.name :: params_acc ))
                  ([], []) params
              in
              let body_expr =
                alpha_expr (List.rev env_rev @ menv) (module_st defs) body_expr
              in
              let seen, id_ = declare seen name in
              ( FunDef
                  { name = id_.name; params = List.rev params_rev; body_expr },
                (name, Resolved_ast.Fun id_.name) :: defs,
                (name, id_) :: menv,
                seen )
          | VarDecl { name; init } ->
              let init =
                match init with
                | InitValue value -> InitValue (plain value)
                | InitIn domain -> InitIn (plain domain)
              in
              let seen, id_ = declare seen name in
              ( VarDecl { name = id_.name; init },
                (name, Resolved_ast.Fun id_.name) :: defs,
                (name, id_) :: menv,
                seen )
          | Process { name; proc; fairness; domain } ->
              let domain = plain domain in
              let seen, id_ = declare seen name in
              (* the root resolves like a callee: to the procedure binding
                 in scope at this point *)
              let proc =
                match List.assoc_opt proc defs with
                | Some (Resolved_ast.Proc n) | Some (Resolved_ast.Fun n) -> n
                | None -> proc
              in
              ( Process { name = id_.name; proc; fairness; domain },
                defs,
                menv,
                seen )
          | ProcDef { name; params; body } ->
              let seen, id_ = declare seen name in
              (* The procedure sees itself (self-recursion), again matching
                 the type checker's environment. *)
              let defs = (name, Resolved_ast.Proc id_.name) :: defs in
              let pst =
                {
                  var_name_counter = counter;
                  defs;
                  rename_binders = true;
                  module_names;
                }
              in
              let env_rev, params_rev =
                List.fold_left
                  (fun (env_acc, params_acc) param ->
                    let renamed = fresh pst env_acc param in
                    ((param, renamed) :: env_acc, renamed :: params_acc))
                  ([], []) params
              in
              let env = List.rev env_rev @ menv in
              let params = List.rev params_rev in
              ( ProcDef
                  { name = id_.name; params; body = alpha_body pst env body },
                defs,
                (name, id_) :: menv,
                seen )
        in
        ({ desc; loc = item.loc } :: acc, defs, menv, seen))
      ([], [], [], []) m.items
  in
  { mod_name = m.mod_name; items = List.rev items_rev; mod_loc = m.mod_loc }

(* ===== Public API ===== *)

let transform (prog : Surface_ast.program) : Resolved_ast.program =
  List.map transform_module prog
