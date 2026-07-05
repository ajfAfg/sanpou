open Generic_ast

(* ===== Types ===== *)

(* Type variables are mutable cells: unification links a variable to a type
   in place (union-find with path compression) instead of building and
   composing substitutions. A type is read through [repr]. *)
type ty =
  | TyInt
  | TyBool
  | TyUnit
  | TyTuple of ty list
  | TySeq of ty
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
  | TyInt | TyBool | TyUnit -> []
  | TyVar { contents = Unbound v } -> [ v ]
  | TyVar { contents = Link _ } -> assert false (* excluded by repr *)
  | TyTuple tys -> List.concat_map freevar_ty tys
  | TySeq ty -> freevar_ty ty
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
  | TyInt, TyInt | TyBool, TyBool | TyUnit, TyUnit -> ()
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
    | (TyInt | TyBool | TyUnit) as t -> t
    | TyTuple tys -> TyTuple (List.map copy tys)
    | TySeq ty -> TySeq (copy ty)
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

(* Subscripting works on sequences and on int-keyed maps: pick by what is
   already known about the collection, defaulting to a sequence. *)
let infer_indexed_access fresh_tyvar ~collection_loc ~index_loc collection_ty
    index_ty =
  unify index_loc index_ty TyInt;
  let elem_ty = fresh_tyvar () in
  (match repr collection_ty with
  | TyMap _ -> unify collection_loc collection_ty (TyMap (TyInt, elem_ty))
  | _ -> unify collection_loc collection_ty (TySeq elem_ty));
  elem_ty

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
  | Var name -> (
      match List.assoc_opt name env with
      | Some tysc -> instantiate fresh_tyvar tysc
      | None -> type_error (Unbound_variable name) e.loc)
  | Self -> TyInt
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
      let fn_ty =
        match List.assoc_opt name env with
        | Some tysc -> instantiate fresh_tyvar tysc
        | None -> type_error (Unbound_variable name) e.loc
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
  | MapInit { binder; lo; hi; value } ->
      unify lo.loc (infer_expr fresh_tyvar env lo) TyInt;
      unify hi.loc (infer_expr fresh_tyvar env hi) TyInt;
      let binder_env = (binder, tysc_of_ty TyInt) :: env in
      TyMap (TyInt, infer_expr fresh_tyvar binder_env value)
  | Tuple elems ->
      if elems = [] then TyUnit
      else TyTuple (List.map (infer_expr fresh_tyvar env) elems)
  | Sequence elems -> infer_sequence_literal fresh_tyvar env elems
  | IfExpr (cond, then_e, else_e) ->
      unify cond.loc (infer_expr fresh_tyvar env cond) TyBool;
      let then_ty = infer_expr fresh_tyvar env then_e in
      unify else_e.loc then_ty (infer_expr fresh_tyvar env else_e);
      then_ty
  | Quant { binder; lo; hi; body; _ } ->
      unify lo.loc (infer_expr fresh_tyvar env lo) TyInt;
      unify hi.loc (infer_expr fresh_tyvar env hi) TyInt;
      let binder_env = (binder, tysc_of_ty TyInt) :: env in
      unify body.loc (infer_expr fresh_tyvar binder_env body) TyBool;
      TyBool

(* ===== Check procedure bodies ===== *)

type proc_ctx = {
  env : tyenv;
  mutable_vars : (id * ty) list;
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
      | SubscriptTarget (name, index) -> (
          match List.assoc_opt name ctx.mutable_vars with
          | Some container_ty ->
              let index_ty = infer_expr ctx.fresh_tyvar ctx.env index in
              let value_ty = infer_expr ctx.fresh_tyvar ctx.env value in
              let elem_ty =
                infer_indexed_access ctx.fresh_tyvar ~collection_loc:loc
                  ~index_loc:index.loc container_ty index_ty
              in
              unify value.loc elem_ty value_ty
          | None -> type_error (Assign_to_non_variable name) loc))
  | Call (name, args) -> (
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
  | VarStep (name, value) ->
      (* Local variables are mutable, so their type is never generalized
         (value restriction): reads and assignments share one type. *)
      let val_ty = infer_expr ctx.fresh_tyvar ctx.env value in
      {
        ctx with
        env = (name, tysc_of_ty val_ty) :: ctx.env;
        mutable_vars = (name, val_ty) :: ctx.mutable_vars;
      }

(* ===== Check a module ===== *)

let check_module (m : Surface_ast.module_def) : unit =
  let tyvar_counter = ref 0 in
  let fresh_tyvar () =
    let v = !tyvar_counter in
    incr tyvar_counter;
    TyVar (ref (Unbound v))
  in
  let _env =
    List.fold_left
      (fun env (item : Surface_ast.item) ->
        match item.desc with
        | ConstDef { name; value } ->
            let ty = infer_expr fresh_tyvar env value in
            (name, generalize env ty) :: env
        | FunDef { name; params; body_expr } ->
            let param_tys = List.map (fun _ -> fresh_tyvar ()) params in
            let param_env =
              List.map2
                (fun pname pty -> (pname, tysc_of_ty pty))
                params param_tys
            in
            let body_ty = infer_expr fresh_tyvar (param_env @ env) body_expr in
            let fn_ty = TyFun (param_tys, body_ty) in
            (name, generalize env fn_ty) :: env
        | VarDecl { name; init } ->
            let ty =
              match init with
              | InitValue value -> infer_expr fresh_tyvar env value
              | InitRange (lo, hi) ->
                  unify lo.loc (infer_expr fresh_tyvar env lo) TyInt;
                  unify hi.loc (infer_expr fresh_tyvar env hi) TyInt;
                  TyInt
            in
            (name, tysc_of_ty ty) :: env
        | ProcDef { name = proc_name; params; body } ->
            let param_tys = List.map (fun _ -> fresh_tyvar ()) params in
            let param_env =
              List.map2
                (fun pname pty -> (pname, tysc_of_ty pty))
                params param_tys
            in
            let return_ty = fresh_tyvar () in
            let fn_ty = TyFun (param_tys, return_ty) in
            let proc_env = ((proc_name, tysc_of_ty fn_ty) :: param_env) @ env in
            let proc_env = ("self", tysc_of_ty TyInt) :: proc_env in
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
            let ctx =
              {
                env = proc_env;
                mutable_vars;
                return_ty;
                in_loop = false;
                fresh_tyvar;
              }
            in
            check_body ctx body;
            (proc_name, generalize env fn_ty) :: env
        | Process { proc; lo; hi; _ } ->
            (match List.assoc_opt proc env with
            | None -> type_error (Unbound_variable proc) item.loc
            | Some _ -> ());
            unify lo.loc (infer_expr fresh_tyvar env lo) TyInt;
            unify hi.loc (infer_expr fresh_tyvar env hi) TyInt;
            env)
      [] m.items
  in
  ()

(* ===== Check a program ===== *)

let check (prog : Surface_ast.program) : unit = List.iter check_module prog

(* ===== Pretty printing for error messages ===== *)

let rec string_of_ty ty =
  match repr ty with
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyUnit -> "unit"
  | TyVar { contents = Unbound v } ->
      let c = Char.chr (Char.code 'a' + (v mod 26)) in
      Printf.sprintf "'%c" c
  | TyVar { contents = Link _ } -> assert false (* excluded by repr *)
  | TyTuple tys -> "(" ^ String.concat ", " (List.map string_of_ty tys) ^ ")"
  | TySeq ty -> "[" ^ string_of_ty ty ^ "]"
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
  | Break_outside_loop -> "break outside of loop"
  | Continue_outside_loop -> "continue outside of loop"
  | Return_type_mismatch -> "return type mismatch"
  | Assign_to_non_variable id ->
      Printf.sprintf "Cannot assign to %s: not a mutable variable" id
  | Recursive_type -> "Recursive type detected"
