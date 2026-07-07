open Generic_ast

(* ===== Types ===== *)

(* Type variables are mutable cells: unification links a variable to a type
   in place (union-find with path compression) instead of building and
   composing substitutions. A type is read through [repr]. *)
type ty =
  | TyInt
  | TyBool
  | TyString
  | TyAtom (* opaque model values; only equality/inequality *)
  | TyUnit
  | TyTuple of ty list
  | TySeq of ty
  | TySet of ty
  | TyRecord of (id * ty) list
      (* fixed-field structural record; fields kept label-sorted so identical
         field sets compare and unify regardless of order *)
  | TyMap of ty * ty
  | TyVar of tyvar ref
  | TyFun of ty list * ty

and tyvar = Unbound of int | Link of ty

(* A scheme quantifies the [Unbound] variables whose ids are listed;
   [instantiate] copies the body replacing them with fresh variables. *)
type tysc = TyScheme of int list * ty

(* ===== Errors ===== *)

type type_error =
  | Type_clash of ty * ty
  | Unbound_variable of id
  | Arity_mismatch of id * int * int
  | Not_a_function of id
  | Break_outside_loop
  | Continue_outside_loop
  | Return_type_mismatch
  | Assign_to_non_variable of id
  | Recursive_type
  | Callable_as_value of id
  | Not_a_procedure of id
  | Not_a_record of ty
  | Unknown_field of id * ty
  | Self_outside_procedure
  | Reserved_module_name of id
  | Conflicting_assignments of id
  | Non_constant_process_domain of id

exception Type_error of type_error * loc

let type_error err loc = raise (Type_error (err, loc))

(* ===== Representative ===== *)

(* Follow links to the type a chain of unifications produced, compressing
   the path on the way. Never returns [TyVar { contents = Link _ }]. *)
let rec repr = function
  | TyVar ({ contents = Link ty } as r) ->
      let t = repr ty in
      r := Link t;
      t
  | ty -> ty

(* ===== Free type variables ===== *)

let rec freevar_ty ty =
  match repr ty with
  | TyInt | TyBool | TyString | TyAtom | TyUnit -> []
  | TyVar { contents = Unbound v } -> [ v ]
  | TyVar { contents = Link _ } -> assert false (* excluded by repr *)
  | TyTuple tys -> List.concat_map freevar_ty tys
  | TySeq ty -> freevar_ty ty
  | TySet ty -> freevar_ty ty
  | TyRecord fields -> List.concat_map (fun (_, t) -> freevar_ty t) fields
  | TyMap (key_ty, value_ty) -> freevar_ty key_ty @ freevar_ty value_ty
  | TyFun (params, ret) -> List.concat_map freevar_ty params @ freevar_ty ret

let freevar_ty_set ty = List.sort_uniq compare (freevar_ty ty)

let freevar_tysc (TyScheme (bound, ty)) =
  List.filter (fun v -> not (List.mem v bound)) (freevar_ty_set ty)

let freevar_env env =
  List.concat_map (fun (_, tysc) -> freevar_tysc tysc) env
  |> List.sort_uniq compare

(* ===== Unification ===== *)

let occurs v ty = List.mem v (freevar_ty_set ty)

let rec unify loc ty1 ty2 =
  let ty1 = repr ty1 and ty2 = repr ty2 in
  match (ty1, ty2) with
  | TyVar r1, TyVar r2 when r1 == r2 -> ()
  | TyVar ({ contents = Unbound v } as r), ty
  | ty, TyVar ({ contents = Unbound v } as r) ->
      if occurs v ty then type_error Recursive_type loc;
      r := Link ty
  | TyInt, TyInt
  | TyBool, TyBool
  | TyString, TyString
  | TyAtom, TyAtom
  | TyUnit, TyUnit ->
      ()
  | TyFun (ps1, r1), TyFun (ps2, r2) ->
      if List.length ps1 <> List.length ps2 then
        type_error (Type_clash (ty1, ty2)) loc;
      List.iter2 (unify loc) ps1 ps2;
      unify loc r1 r2
  | TyTuple ts1, TyTuple ts2 ->
      if List.length ts1 <> List.length ts2 then
        type_error (Type_clash (ty1, ty2)) loc;
      List.iter2 (unify loc) ts1 ts2
  | TySeq t1, TySeq t2 -> unify loc t1 t2
  | TySet t1, TySet t2 -> unify loc t1 t2
  | TyRecord fs1, TyRecord fs2 ->
      (* Structural, no row polymorphism: identical field sets required.
         Fields are label-sorted at construction, so a positional walk
         matches labels. *)
      if List.map fst fs1 <> List.map fst fs2 then
        type_error (Type_clash (ty1, ty2)) loc;
      List.iter2 (fun (_, t1) (_, t2) -> unify loc t1 t2) fs1 fs2
  | TyMap (k1, v1), TyMap (k2, v2) ->
      unify loc k1 k2;
      unify loc v1 v2
  | t1, t2 -> type_error (Type_clash (t1, t2)) loc

(* ===== Generalization / Instantiation ===== *)

let generalize env ty =
  let fv_env = freevar_env env in
  let gen_vars =
    List.filter (fun v -> not (List.mem v fv_env)) (freevar_ty_set ty)
  in
  TyScheme (gen_vars, ty)

let instantiate fresh_tyvar (TyScheme (bound, ty)) =
  let mapping = List.map (fun v -> (v, fresh_tyvar ())) bound in
  let rec copy ty =
    match repr ty with
    | TyVar { contents = Unbound v } as t -> (
        match List.assoc_opt v mapping with Some fresh -> fresh | None -> t)
    | TyVar { contents = Link _ } -> assert false (* excluded by repr *)
    | (TyInt | TyBool | TyString | TyAtom | TyUnit) as t -> t
    | TyTuple tys -> TyTuple (List.map copy tys)
    | TySeq ty -> TySeq (copy ty)
    | TySet ty -> TySet (copy ty)
    | TyRecord fields -> TyRecord (List.map (fun (l, t) -> (l, copy t)) fields)
    | TyMap (key_ty, value_ty) -> TyMap (copy key_ty, copy value_ty)
    | TyFun (params, ret) -> TyFun (List.map copy params, copy ret)
  in
  copy ty

(* ===== Type environment ===== *)

type tyenv = (id * tysc) list

let tysc_of_ty ty = TyScheme ([], ty)

(* ===== Built-in functions ===== *)

(* Instantiated per use: each call gets fresh type variables, like a
   polymorphic function type would after [instantiate]. *)
let builtin_signature fresh_tyvar (b : Builtin.t) =
  match b with
  | Globally | Finally -> ([ TyBool ], TyBool)
  | Head ->
      let elem_ty = fresh_tyvar () in
      ([ TySeq elem_ty ], elem_ty)
  | Tail ->
      let elem_ty = fresh_tyvar () in
      ([ TySeq elem_ty ], TySeq elem_ty)
  | Append ->
      let elem_ty = fresh_tyvar () in
      ([ TySeq elem_ty; elem_ty ], TySeq elem_ty)
  | Concat ->
      let elem_ty = fresh_tyvar () in
      ([ TySeq elem_ty; TySeq elem_ty ], TySeq elem_ty)
  | Len ->
      let elem_ty = fresh_tyvar () in
      ([ TySeq elem_ty ], TyInt)
  | Union | Intersection | Difference ->
      let elem_ty = fresh_tyvar () in
      ([ TySet elem_ty; TySet elem_ty ], TySet elem_ty)
  | Cardinality ->
      let elem_ty = fresh_tyvar () in
      ([ TySet elem_ty ], TyInt)
  | Subseteq ->
      let elem_ty = fresh_tyvar () in
      ([ TySet elem_ty; TySet elem_ty ], TyBool)

(* ===== Infer expression type ===== *)

(* Operand type errors point at the offending operand's own location. *)
let ty_prim fresh_tyvar ~lhs_loc ~rhs_loc op ty1 ty2 =
  match op with
  | Plus | Minus | Mult | Div | Mod ->
      unify lhs_loc ty1 TyInt;
      unify rhs_loc ty2 TyInt;
      TyInt
  | Lt | Gt | LtEq | GtEq ->
      unify lhs_loc ty1 TyInt;
      unify rhs_loc ty2 TyInt;
      TyBool
  | And | Or ->
      unify lhs_loc ty1 TyBool;
      unify rhs_loc ty2 TyBool;
      TyBool
  | Eq | Neq ->
      let tv = fresh_tyvar () in
      unify lhs_loc ty1 tv;
      unify rhs_loc ty2 tv;
      TyBool
  | In ->
      (* [elem in set]: the right operand is a set of the left's type. *)
      unify rhs_loc ty2 (TySet ty1);
      TyBool

(* Subscripting works on sequences (int indices) and on maps (whose keys
   take the domain's element type): pick by what is already known about the
   collection, defaulting to a sequence. *)
let infer_indexed_access fresh_tyvar ~collection_loc ~index_loc collection_ty
    index_ty =
  let elem_ty = fresh_tyvar () in
  (match repr collection_ty with
  | TyMap (key_ty, _) ->
      unify index_loc index_ty key_ty;
      unify collection_loc collection_ty (TyMap (key_ty, elem_ty))
  | _ ->
      unify index_loc index_ty TyInt;
      unify collection_loc collection_ty (TySeq elem_ty));
  elem_ty

(* Record field access. Without row polymorphism the record's full type must
   be known here: an unresolved type variable cannot be a record. *)
let infer_field_access ~loc collection_ty field =
  match repr collection_ty with
  | TyRecord fields -> (
      match List.assoc_opt field fields with
      | Some ty -> ty
      | None -> type_error (Unknown_field (field, repr collection_ty)) loc)
  | ty -> type_error (Not_a_record ty) loc

let rec infer_sequence_literal fresh_tyvar env elems =
  let elem_ty = fresh_tyvar () in
  List.iter
    (fun (elem : Surface_ast.expr) ->
      unify elem.loc elem_ty (infer_expr fresh_tyvar env elem))
    elems;
  TySeq elem_ty

(* Infer the application of a value of type [fn_ty] to [args]; [loc] is the
   application site, used when the function type itself does not fit. *)
and infer_app fresh_tyvar env loc fn_ty args : ty =
  let arg_tys = List.map (infer_expr fresh_tyvar env) args in
  let ret_ty = fresh_tyvar () in
  unify loc fn_ty (TyFun (arg_tys, ret_ty));
  ret_ty

and infer_expr fresh_tyvar (env : tyenv) (e : Surface_ast.expr) : ty =
  match e.desc with
  | IntLit _ -> TyInt
  | BoolLit _ -> TyBool
  | StrLit _ -> TyString
  | AtomLit name ->
      (* The atom's text is its TLA+ name (the model value's identity in
         traces and the .cfg), so a reserved text cannot be renamed away
         like a declaration could. *)
      if
        List.mem name Emit_tla.reserved_module_names
        || Emit_tla.is_generated_action_label name
      then type_error (Reserved_module_name name) e.loc;
      TyAtom
  | Var name -> (
      match List.assoc_opt name env with
      | Some tysc -> (
          (* Functions and procedures are second-class: TLA+ operators
             cannot be passed as plain values. Nothing but a callable can
             carry a function type, so the type is the complete signal. *)
          match repr (instantiate fresh_tyvar tysc) with
          | TyFun _ -> type_error (Callable_as_value name) e.loc
          | ty -> ty)
      | None -> type_error (Unbound_variable name) e.loc)
  | Self -> (
      (* [self] is the process id, bound only inside procedure bodies. *)
      match List.assoc_opt "self" env with
      | Some tysc -> instantiate fresh_tyvar tysc
      | None -> type_error Self_outside_procedure e.loc)
  | UnOp (Neg, rhs) ->
      unify rhs.loc (infer_expr fresh_tyvar env rhs) TyInt;
      TyInt
  | UnOp (Not, rhs) ->
      unify rhs.loc (infer_expr fresh_tyvar env rhs) TyBool;
      TyBool
  | BinOp (op, lhs, rhs) ->
      let ty1 = infer_expr fresh_tyvar env lhs in
      let ty2 = infer_expr fresh_tyvar env rhs in
      ty_prim fresh_tyvar ~lhs_loc:lhs.loc ~rhs_loc:rhs.loc op ty1 ty2
  | App (name, args) ->
      (* Lexical resolution, mirroring Alpha_convert: a definition in the
         environment shadows the builtin of the same name; builtins are the
         outermost scope. *)
      let fn_ty =
        match List.assoc_opt name env with
        | Some tysc -> (
            (* Unifying a parameter or local with a function type would
               accept first-class usage that TLA+ operators cannot express,
               so only names already known to be functions are applicable. *)
            match repr (instantiate fresh_tyvar tysc) with
            | TyFun _ as fn_ty -> fn_ty
            | _ -> type_error (Not_a_function name) e.loc)
        | None -> (
            match Builtin.of_name name with
            | Some b ->
                (* Arity is checked here so the error is an Arity_mismatch
                   rather than a TyFun clash. *)
                let param_tys, ret_ty = builtin_signature fresh_tyvar b in
                if List.length param_tys <> List.length args then
                  type_error
                    (Arity_mismatch
                       (Builtin.name b, List.length param_tys, List.length args))
                    e.loc;
                TyFun (param_tys, ret_ty)
            | None -> type_error (Unbound_variable name) e.loc)
      in
      infer_app fresh_tyvar env e.loc fn_ty args
  | Builtin (b, args) ->
      (* Arity is checked here so the error is an Arity_mismatch rather than
         a TyFun clash. *)
      let param_tys, ret_ty = builtin_signature fresh_tyvar b in
      if List.length param_tys <> List.length args then
        type_error
          (Arity_mismatch
             (Builtin.name b, List.length param_tys, List.length args))
          e.loc;
      infer_app fresh_tyvar env e.loc (TyFun (param_tys, ret_ty)) args
  | Subscript (lhs, index) ->
      let lhs_ty = infer_expr fresh_tyvar env lhs in
      let index_ty = infer_expr fresh_tyvar env index in
      infer_indexed_access fresh_tyvar ~collection_loc:lhs.loc
        ~index_loc:index.loc lhs_ty index_ty
  | Field (record, label) ->
      let record_ty = infer_expr fresh_tyvar env record in
      infer_field_access ~loc:record.loc record_ty label
  | Record fields ->
      (* The AST keeps fields in source order (evaluation order for hoisted
         calls); the type sorts them so unify's positional walk matches
         labels regardless of literal order. Values are inferred in source
         order so type errors surface left to right. *)
      TyRecord
        (List.map (fun (l, e) -> (l, infer_expr fresh_tyvar env e)) fields
        |> List.sort (fun (a, _) (b, _) -> compare a b))
  | Range (lo, hi) ->
      unify lo.loc (infer_expr fresh_tyvar env lo) TyInt;
      unify hi.loc (infer_expr fresh_tyvar env hi) TyInt;
      TySet TyInt
  | MapInit { binder; domain; value } ->
      (* The key type is the domain's element type: any set works — ints,
         strings, atoms (so per-process state can be keyed by any process
         ID set). *)
      let key_ty = fresh_tyvar () in
      unify domain.loc (infer_expr fresh_tyvar env domain) (TySet key_ty);
      let binder_env = (binder, tysc_of_ty key_ty) :: env in
      TyMap (key_ty, infer_expr fresh_tyvar binder_env value)
  | Tuple elems ->
      if elems = [] then TyUnit
      else TyTuple (List.map (infer_expr fresh_tyvar env) elems)
  | Sequence elems -> infer_sequence_literal fresh_tyvar env elems
  | SetLit elems ->
      let elem_ty = fresh_tyvar () in
      List.iter
        (fun (elem : Surface_ast.expr) ->
          unify elem.loc elem_ty (infer_expr fresh_tyvar env elem))
        elems;
      TySet elem_ty
  | SetComp { binder; domain; pred } ->
      let elem_ty = fresh_tyvar () in
      unify domain.loc (infer_expr fresh_tyvar env domain) (TySet elem_ty);
      let binder_env = (binder, tysc_of_ty elem_ty) :: env in
      unify pred.loc (infer_expr fresh_tyvar binder_env pred) TyBool;
      TySet elem_ty
  | IfExpr (cond, then_e, else_e) ->
      unify cond.loc (infer_expr fresh_tyvar env cond) TyBool;
      let then_ty = infer_expr fresh_tyvar env then_e in
      unify else_e.loc then_ty (infer_expr fresh_tyvar env else_e);
      then_ty
  | Quant { binder; domain; body; _ } ->
      let elem_ty = fresh_tyvar () in
      unify domain.loc (infer_expr fresh_tyvar env domain) (TySet elem_ty);
      let binder_env = (binder, tysc_of_ty elem_ty) :: env in
      unify body.loc (infer_expr fresh_tyvar binder_env body) TyBool;
      TyBool

(* ===== Check procedure bodies ===== *)

type proc_ctx = {
  env : tyenv;
  mutable_vars : (id * ty) list;
  proc_names : id list; (* procedures visible so far, including self *)
  return_ty : ty;
  in_loop : bool;
  fresh_tyvar : unit -> ty;
}

(* Statement-level errors (assignment targets, break/continue) point at the
   statement's own location; expression errors point at the expression. *)
let check_simple_stmt (ctx : proc_ctx) (stmt : Surface_ast.simple_stmt) : unit =
  let loc = stmt.loc in
  match stmt.desc with
  | Assign (target, value) -> (
      match target with
      | VarTarget name -> (
          match List.assoc_opt name ctx.mutable_vars with
          | Some var_ty ->
              unify value.loc var_ty (infer_expr ctx.fresh_tyvar ctx.env value)
          | None -> type_error (Assign_to_non_variable name) loc)
      | PathTarget (name, path) -> (
          match List.assoc_opt name ctx.mutable_vars with
          | Some container_ty ->
              let value_ty = infer_expr ctx.fresh_tyvar ctx.env value in
              (* Walk the access path: each step peels one collection level,
                 by index for a subscript or by label for a field. *)
              let elem_ty =
                List.fold_left
                  (fun collection_ty accessor ->
                    match accessor with
                    | AccIndex index ->
                        let index_ty = infer_expr ctx.fresh_tyvar ctx.env index in
                        infer_indexed_access ctx.fresh_tyvar ~collection_loc:loc
                          ~index_loc:index.loc collection_ty index_ty
                    | AccField field ->
                        infer_field_access ~loc collection_ty field)
                  container_ty path
              in
              unify value.loc elem_ty value_ty
          | None -> type_error (Assign_to_non_variable name) loc))
  | Call (name, args) -> (
      (* A statement call discards no value only a procedure can produce
         steps for; a def function here would reach the emitter's
         procedure table and crash. *)
      if not (List.mem name ctx.proc_names) then
        type_error (Not_a_procedure name) loc;
      match List.assoc_opt name ctx.env with
      | None -> type_error (Unbound_variable name) loc
      | Some tysc ->
          let fn_ty = instantiate ctx.fresh_tyvar tysc in
          let _ = infer_app ctx.fresh_tyvar ctx.env loc fn_ty args in
          ())
  | Return value ->
      unify value.loc ctx.return_ty (infer_expr ctx.fresh_tyvar ctx.env value)
  | Break -> if not ctx.in_loop then type_error Break_outside_loop loc
  | Continue -> if not ctx.in_loop then type_error Continue_outside_loop loc
  | Await cond ->
      unify cond.loc (infer_expr ctx.fresh_tyvar ctx.env cond) TyBool
  | Assert cond ->
      unify cond.loc (infer_expr ctx.fresh_tyvar ctx.env cond) TyBool

(* One statement list (SimpleStep / WithStep) is one atomic action: its
   assignments become one conjunction of primed equations. Multiple path
   updates to a variable compose left-to-right into a single EXCEPT, but a
   whole-variable write cannot be combined with any other write to the same
   variable in the same step — there is no consistent value for the primed
   variable, and the emitter would have to drop one silently. *)
let check_no_conflicting_writes (stmts : Surface_ast.simple_stmt list) : unit =
  let write (stmt : Surface_ast.simple_stmt) =
    match stmt.desc with
    | Assign (VarTarget name, _) -> Some (name, `Whole, stmt.loc)
    | Assign (PathTarget (name, _), _) -> Some (name, `Path, stmt.loc)
    | _ -> None
  in
  let (_ : (id * [ `Whole | `Path ]) list) =
    List.fold_left
      (fun seen (name, kind, loc) ->
        if
          List.exists
            (fun (n, k) -> n = name && (k = `Whole || kind = `Whole))
            seen
        then type_error (Conflicting_assignments name) loc;
        (name, kind) :: seen)
      []
      (List.filter_map write stmts)
  in
  ()

let rec check_body (ctx : proc_ctx) (steps : Surface_ast.body) : unit =
  match steps with
  | [] -> ()
  | step :: rest -> check_body (check_step ctx step) rest

(* Returns the context for the following steps: VarStep extends it, every
   other step leaves it unchanged. *)
and check_step (ctx : proc_ctx) (step : Surface_ast.step) : proc_ctx =
  match step.desc with
  | EmptyStep -> ctx
  | SimpleStep stmts ->
      check_no_conflicting_writes stmts;
      List.iter (check_simple_stmt ctx) stmts;
      ctx
  | BlockStep (While { cond; body }) ->
      unify cond.loc (infer_expr ctx.fresh_tyvar ctx.env cond) TyBool;
      check_body { ctx with in_loop = true } body;
      ctx
  | BlockStep (If { cond; body; else_body }) ->
      unify cond.loc (infer_expr ctx.fresh_tyvar ctx.env cond) TyBool;
      check_body ctx body;
      (match else_body with
      | Some else_body -> check_body ctx else_body
      | None -> ());
      ctx
  | BlockStep (Either arms) ->
      List.iter (check_body ctx) arms;
      ctx
  | WithStep { binder; domain; stmts } ->
      let elem_ty = ctx.fresh_tyvar () in
      unify domain.loc
        (infer_expr ctx.fresh_tyvar ctx.env domain)
        (TySet elem_ty);
      (* The binder is readable but not assignable: it is absent from
         mutable_vars, so assignments to it fail as to any non-variable. *)
      let binder_ctx =
        { ctx with env = (binder, tysc_of_ty elem_ty) :: ctx.env }
      in
      check_no_conflicting_writes stmts;
      List.iter (check_simple_stmt binder_ctx) stmts;
      ctx
  | VarStep (name, value) ->
      (* Local variables are mutable, so their type is never generalized
         (value restriction): reads and assignments share one type. *)
      let val_ty = infer_expr ctx.fresh_tyvar ctx.env value in
      {
        ctx with
        env = (name, tysc_of_ty val_ty) :: ctx.env;
        mutable_vars = (name, val_ty) :: ctx.mutable_vars;
      }

(* ===== Process domain constancy =====

   PlusCal requires a process set to be a constant expression: the emitted
   spec fixes ProcSet and the domains of pc/stack at Init, so a domain that
   reads mutable state breaks TLC at runtime the moment the state changes.
   sanpou has no CONSTANT stage — literals, atoms, and defs form the constant
   fragment — so a module-level name is non-constant iff it is a [var], or a
   def whose body (transitively) mentions one. *)
let rec first_nonconst_ref (nonconst : id list) (e : Surface_ast.expr) :
    (id * loc) option =
  let first_of es = List.find_map (first_nonconst_ref nonconst) es in
  let under_binder binder e =
    first_nonconst_ref (List.filter (fun n -> n <> binder) nonconst) e
  in
  match e.desc with
  | IntLit _ | BoolLit _ | StrLit _ | AtomLit _ | Self -> None
  | Var name -> if List.mem name nonconst then Some (name, e.loc) else None
  | App (name, args) ->
      if List.mem name nonconst then Some (name, e.loc) else first_of args
  | UnOp (_, e1) -> first_nonconst_ref nonconst e1
  | BinOp (_, a, b) | Range (a, b) | Subscript (a, b) -> first_of [ a; b ]
  | Field (record, _) -> first_nonconst_ref nonconst record
  | Record fields -> first_of (List.map snd fields)
  | Builtin (_, args) | SetLit args | Tuple args | Sequence args ->
      first_of args
  | IfExpr (a, b, c) -> first_of [ a; b; c ]
  | MapInit { binder; domain; value } -> (
      match first_nonconst_ref nonconst domain with
      | Some r -> Some r
      | None -> under_binder binder value)
  | SetComp { binder; domain; pred } -> (
      match first_nonconst_ref nonconst domain with
      | Some r -> Some r
      | None -> under_binder binder pred)
  | Quant { binder; domain; body; _ } -> (
      match first_nonconst_ref nonconst domain with
      | Some r -> Some r
      | None -> under_binder binder body)

(* ===== Check a module ===== *)

let check_module (m : Surface_ast.module_def) : unit =
  let tyvar_counter = ref 0 in
  let fresh_tyvar () =
    let v = !tyvar_counter in
    incr tyvar_counter;
    TyVar (ref (Unbound v))
  in
  (* One [self] type per module: the emitter quantifies every callable
     procedure over a single ProcSet (the union of the process ID sets), so
     all processes must share one ID element type, which is also the type of
     [self] in every procedure body. *)
  let self_ty = fresh_tyvar () in
  (* Module-level names shadow sequentially like every other binding; the
     flat TLA+ namespace is reconciled by alpha-conversion, which renames
     the shadowed declarations. Names the emitter itself generates (or
     pulls in via EXTENDS) are reserved outright. *)
  let declare names loc name =
    if
      List.mem name Emit_tla.reserved_module_names
      || Emit_tla.is_generated_action_label name
    then type_error (Reserved_module_name name) loc;
    name :: names
  in
  (* [nonconst] accumulates the module-level names whose values can change
     over a behavior: vars, and defs that (transitively) read one. *)
  let nonconst_def nonconst name body =
    match first_nonconst_ref nonconst body with
    | Some _ -> name :: nonconst
    | None -> nonconst
  in
  let (_ : tyenv * id list * id list * id list) =
    List.fold_left
      (fun (env, proc_names, names, nonconst) (item : Surface_ast.item) ->
        match item.desc with
        | ConstDef { name; value } ->
            let names = declare names item.loc name in
            let ty = infer_expr fresh_tyvar env value in
            ( (name, generalize env ty) :: env,
              proc_names,
              names,
              nonconst_def nonconst name value )
        | PropDef { name; value } ->
            let names = declare names item.loc name in
            unify value.loc (infer_expr fresh_tyvar env value) TyBool;
            ( (name, tysc_of_ty TyBool) :: env,
              proc_names,
              names,
              nonconst_def nonconst name value )
        | FunDef { name; params; body_expr } ->
            let names = declare names item.loc name in
            let param_tys = List.map (fun _ -> fresh_tyvar ()) params in
            let param_env =
              List.map2
                (fun pname pty -> (pname, tysc_of_ty pty))
                params param_tys
            in
            let body_ty = infer_expr fresh_tyvar (param_env @ env) body_expr in
            let fn_ty = TyFun (param_tys, body_ty) in
            (* Params shadow module names inside the body, but the filtered
               list is only for the body check: the accumulator keeps them. *)
            let body_nonconst =
              List.filter (fun n -> not (List.mem n params)) nonconst
            in
            let nonconst =
              match first_nonconst_ref body_nonconst body_expr with
              | Some _ -> name :: nonconst
              | None -> nonconst
            in
            ((name, generalize env fn_ty) :: env, proc_names, names, nonconst)
        | VarDecl { name; init } ->
            let names = declare names item.loc name in
            let ty =
              match init with
              | InitValue value -> infer_expr fresh_tyvar env value
              | InitIn domain ->
                  let elem_ty = fresh_tyvar () in
                  unify domain.loc
                    (infer_expr fresh_tyvar env domain)
                    (TySet elem_ty);
                  elem_ty
            in
            ((name, tysc_of_ty ty) :: env, proc_names, names, name :: nonconst)
        | ProcDef { name = proc_name; params; body } ->
            let names = declare names item.loc proc_name in
            let param_tys = List.map (fun _ -> fresh_tyvar ()) params in
            let param_env =
              List.map2
                (fun pname pty -> (pname, tysc_of_ty pty))
                params param_tys
            in
            let return_ty = fresh_tyvar () in
            let fn_ty = TyFun (param_tys, return_ty) in
            let proc_env = ((proc_name, tysc_of_ty fn_ty) :: param_env) @ env in
            let proc_env = ("self", tysc_of_ty self_ty) :: proc_env in
            let mutable_vars =
              List.filter_map
                (fun (id, tysc) ->
                  match tysc with
                  | TyScheme ([], ty) ->
                      if
                        List.exists
                          (fun (item : Surface_ast.item) ->
                            match item.desc with
                            | VarDecl { name; _ } -> name = id
                            | _ -> false)
                          m.items
                      then Some (id, ty)
                      else None
                  | _ -> None)
                env
            in
            let proc_names = proc_name :: proc_names in
            let ctx =
              {
                env = proc_env;
                mutable_vars;
                proc_names;
                return_ty;
                in_loop = false;
                fresh_tyvar;
              }
            in
            check_body ctx body;
            (* Generalize with [self] in scope: [self_ty] is not part of the
               module-level env, so a procedure whose type mentions it (via a
               parameter or return value unified with [self]) would otherwise
               get it quantified — and each call site would then instantiate
               its own [self] type, breaking the one-self-type-per-module
               invariant. *)
            let gen_env = ("self", tysc_of_ty self_ty) :: env in
            ( (proc_name, generalize gen_env fn_ty) :: env,
              proc_names,
              names,
              nonconst )
        | Process { name; proc; domain; _ } ->
            let names = declare names item.loc name in
            (* The root must be a procedure: a def function here would
               reach the emitter's procedure table and crash. *)
            if not (List.mem proc proc_names) then
              type_error (Not_a_procedure proc) item.loc;
            (* The ID set's element type is [self]'s type, shared across the
               module (all processes must agree). *)
            unify domain.loc
              (infer_expr fresh_tyvar env domain)
              (TySet self_ty);
            (match first_nonconst_ref nonconst domain with
            | Some (name, loc) ->
                type_error (Non_constant_process_domain name) loc
            | None -> ());
            (env, proc_names, names, nonconst))
      ([], [], [], []) m.items
  in
  ()

(* ===== Check a program ===== *)

let check (prog : Surface_ast.program) : unit = List.iter check_module prog

(* ===== Pretty printing for error messages ===== *)

let rec string_of_ty ty =
  match repr ty with
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyString -> "string"
  | TyAtom -> "atom"
  | TyUnit -> "unit"
  | TyVar { contents = Unbound v } ->
      let c = Char.chr (Char.code 'a' + (v mod 26)) in
      Printf.sprintf "'%c" c
  | TyVar { contents = Link _ } -> assert false (* excluded by repr *)
  | TyTuple tys -> "(" ^ String.concat ", " (List.map string_of_ty tys) ^ ")"
  | TySeq ty -> "[" ^ string_of_ty ty ^ "]"
  | TySet ty -> "{" ^ string_of_ty ty ^ "}"
  | TyRecord fields ->
      "{"
      ^ String.concat ", "
          (List.map (fun (l, t) -> l ^ ": " ^ string_of_ty t) fields)
      ^ "}"
  | TyMap (key_ty, value_ty) ->
      "{" ^ string_of_ty key_ty ^ " -> " ^ string_of_ty value_ty ^ "}"
  | TyFun (params, ret) ->
      let params_s =
        match params with
        | [ p ] -> paren_fun_ty p
        | _ -> "(" ^ String.concat ", " (List.map string_of_ty params) ^ ")"
      in
      params_s ^ " -> " ^ string_of_ty ret

and paren_fun_ty ty =
  match repr ty with
  | TyFun _ as t -> "(" ^ string_of_ty t ^ ")"
  | t -> string_of_ty t

let string_of_type_error = function
  | Type_clash (t1, t2) ->
      Printf.sprintf "Type error: cannot unify %s with %s" (string_of_ty t1)
        (string_of_ty t2)
  | Unbound_variable id -> Printf.sprintf "Unbound variable: %s" id
  | Arity_mismatch (id, expected, actual) ->
      Printf.sprintf "Function %s expects %d arguments but got %d" id expected
        actual
  | Not_a_function id -> Printf.sprintf "%s is not a function" id
  | Callable_as_value id ->
      Printf.sprintf
        "%s is a function or procedure and cannot be used as a value; apply \
         it instead"
        id
  | Not_a_procedure id -> Printf.sprintf "%s is not a procedure" id
  | Not_a_record ty ->
      Printf.sprintf "%s is not a record; cannot access a field of it"
        (string_of_ty ty)
  | Unknown_field (field, ty) ->
      Printf.sprintf "record %s has no field %s" (string_of_ty ty) field
  | Self_outside_procedure -> "self can only be used inside a procedure"
  | Reserved_module_name id ->
      Printf.sprintf
        "%s is reserved: it collides with a name in the emitted TLA+ module"
        id
  | Conflicting_assignments id ->
      Printf.sprintf
        "conflicting assignments to %s in one step: a whole-variable \
         assignment cannot be combined with another assignment to the same \
         variable"
        id
  | Non_constant_process_domain id ->
      Printf.sprintf
        "a process ID set must be constant, but %s is mutable state (or \
         depends on it)"
        id
  | Break_outside_loop -> "break outside of loop"
  | Continue_outside_loop -> "continue outside of loop"
  | Return_type_mismatch -> "return type mismatch"
  | Assign_to_non_variable id ->
      Printf.sprintf "Cannot assign to %s: not a mutable variable" id
  | Recursive_type -> "Recursive type detected"
