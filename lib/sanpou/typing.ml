open Cst

(* ===== Types ===== *)

type tyvar = int

type ty =
  | TyInt
  | TyBool
  | TyUnit
  | TyTuple of ty list
  | TySeq of ty
  | TyVar of tyvar
  | TyFun of ty list * ty

type tysc = TyScheme of tyvar list * ty

(* ===== Errors ===== *)

type type_error =
  | Type_clash of ty * ty
  | Unbound_variable of id
  | Arity_mismatch of id * int * int
  | Not_a_function of id
  | Break_outside_loop
  | Return_type_mismatch
  | Assign_to_non_variable of id
  | Recursive_type

exception Type_error of type_error * loc

let type_error err loc = raise (Type_error (err, loc))

(* ===== Free type variables ===== *)

let rec freevar_ty = function
  | TyInt | TyBool | TyUnit -> []
  | TyVar v -> [ v ]
  | TyTuple tys -> List.concat_map freevar_ty tys
  | TySeq ty -> freevar_ty ty
  | TyFun (params, ret) -> List.concat_map freevar_ty params @ freevar_ty ret

let freevar_ty_set ty = List.sort_uniq compare (freevar_ty ty)

let freevar_tysc (TyScheme (bound, ty)) =
  List.filter (fun v -> not (List.mem v bound)) (freevar_ty_set ty)

let freevar_env env =
  List.concat_map (fun (_, tysc) -> freevar_tysc tysc) env
  |> List.sort_uniq compare

(* ===== Substitution ===== *)

type subst = (tyvar * ty) list

let rec subst_ty (s : subst) = function
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyUnit -> TyUnit
  | TyVar v -> (
      match List.assoc_opt v s with Some t -> subst_ty s t | None -> TyVar v)
  | TyTuple tys -> TyTuple (List.map (subst_ty s) tys)
  | TySeq ty -> TySeq (subst_ty s ty)
  | TyFun (params, ret) -> TyFun (List.map (subst_ty s) params, subst_ty s ret)

let compose_subst (s1 : subst) (s2 : subst) : subst =
  let s2' = List.map (fun (v, t) -> (v, subst_ty s1 t)) s2 in
  let s1_only = List.filter (fun (v, _) -> not (List.mem_assoc v s2)) s1 in
  s2' @ s1_only

let subst_env s env =
  List.map
    (fun (name, TyScheme (bound, ty)) ->
      (name, TyScheme (bound, subst_ty s ty)))
    env

(* ===== Occurs check ===== *)

let occurs v ty = List.mem v (freevar_ty_set ty)

(* ===== Unification ===== *)

let rec unify loc (eqs : (ty * ty) list) : subst =
  match eqs with
  | [] -> []
  | (t1, t2) :: rest when t1 = t2 -> unify loc rest
  | (TyVar v, t) :: rest | (t, TyVar v) :: rest ->
      if occurs v t then type_error Recursive_type loc;
      let s = [ (v, t) ] in
      let rest' = List.map (fun (a, b) -> (subst_ty s a, subst_ty s b)) rest in
      compose_subst (unify loc rest') s
  | (TyFun (ps1, r1), TyFun (ps2, r2)) :: rest ->
      if List.length ps1 <> List.length ps2 then
        type_error (Type_clash (TyFun (ps1, r1), TyFun (ps2, r2))) loc;
      unify loc (List.combine ps1 ps2 @ [ (r1, r2) ] @ rest)
  | (TyTuple ts1, TyTuple ts2) :: rest ->
      if List.length ts1 <> List.length ts2 then
        type_error (Type_clash (TyTuple ts1, TyTuple ts2)) loc;
      unify loc (List.combine ts1 ts2 @ rest)
  | (TySeq ty1, TySeq ty2) :: rest -> unify loc ((ty1, ty2) :: rest)
  | (t1, t2) :: _ -> type_error (Type_clash (t1, t2)) loc

(* ===== Generalization / Instantiation ===== *)

let generalize env ty =
  let fv_ty = freevar_ty_set ty in
  let fv_env = freevar_env env in
  let gen_vars = List.filter (fun v -> not (List.mem v fv_env)) fv_ty in
  TyScheme (gen_vars, ty)

let instantiate fresh_tyvar (TyScheme (bound, ty)) =
  let s = List.map (fun v -> (v, fresh_tyvar ())) bound in
  subst_ty s ty

(* ===== Type environment ===== *)

type tyenv = (id * tysc) list

let tysc_of_ty ty = TyScheme ([], ty)

(* ===== Infer expression type ===== *)

let ty_prim fresh_tyvar _loc op ty1 ty2 =
  match op with
  | Plus | Minus | Mult -> ([ (ty1, TyInt); (ty2, TyInt) ], TyInt)
  | Lt | LtEq | GtEq -> ([ (ty1, TyInt); (ty2, TyInt) ], TyBool)
  | And -> ([ (ty1, TyBool); (ty2, TyBool) ], TyBool)
  | Eq | Neq ->
      let tv = fresh_tyvar () in
      ([ (ty1, tv); (ty2, tv) ], TyBool)

let rec infer_sequence_literal fresh_tyvar env elems =
  let elem_ty = fresh_tyvar () in
  let s_acc = ref [] in
  List.iter
    (fun elem ->
      let env' = subst_env !s_acc env in
      let s_elem, ty_elem = infer_expr fresh_tyvar env' elem in
      let s_combined = compose_subst s_elem !s_acc in
      let elem_ty' = subst_ty s_combined elem_ty in
      let s_unify = unify { line = 0; col = 0 } [ (elem_ty', ty_elem) ] in
      s_acc := compose_subst s_unify s_combined)
    elems;
  (!s_acc, TySeq (subst_ty !s_acc elem_ty))

and infer_expr fresh_tyvar (env : tyenv) (e : expr) : subst * ty =
  match e with
  | IntLit _ -> ([], TyInt)
  | BoolLit _ -> ([], TyBool)
  | Var { name; _ } -> (
      match List.assoc_opt name env with
      | Some tysc -> ([], instantiate fresh_tyvar tysc)
      | None -> type_error (Unbound_variable name) { line = 0; col = 0 })
  | UnOp { op = Neg; rhs; _ } ->
      let s, ty = infer_expr fresh_tyvar env rhs in
      let s2 = unify { line = 0; col = 0 } [ (ty, TyInt) ] in
      (compose_subst s2 s, TyInt)
  | BinOp { op; lhs; rhs; _ } ->
      let s1, ty1 = infer_expr fresh_tyvar env lhs in
      let env' = subst_env s1 env in
      let s2, ty2 = infer_expr fresh_tyvar env' rhs in
      let ty1' = subst_ty s2 ty1 in
      let eqs, result_ty =
        ty_prim fresh_tyvar { line = 0; col = 0 } op ty1' ty2
      in
      let s3 = unify { line = 0; col = 0 } eqs in
      (compose_subst s3 (compose_subst s2 s1), subst_ty s3 result_ty)
  | App { name = "globally" | "finally"; args; _ } -> (
      match args.items with
      | [ e ] ->
          let s, ty = infer_expr fresh_tyvar env e in
          let s2 = unify { line = 0; col = 0 } [ (ty, TyBool) ] in
          (compose_subst s2 s, TyBool)
      | _ ->
          type_error
            (Arity_mismatch ("globally/finally", 1, List.length args.items))
            { line = 0; col = 0 })
  | App { name = "head"; args; _ } -> (
      match args.items with
      | [ seq ] ->
          let s, seq_ty = infer_expr fresh_tyvar env seq in
          let elem_ty = fresh_tyvar () in
          let s2 = unify { line = 0; col = 0 } [ (seq_ty, TySeq elem_ty) ] in
          (compose_subst s2 s, subst_ty s2 elem_ty)
      | _ ->
          type_error
            (Arity_mismatch ("head", 1, List.length args.items))
            { line = 0; col = 0 })
  | App { name = "tail"; args; _ } -> (
      match args.items with
      | [ seq ] ->
          let s, seq_ty = infer_expr fresh_tyvar env seq in
          let elem_ty = fresh_tyvar () in
          let s2 = unify { line = 0; col = 0 } [ (seq_ty, TySeq elem_ty) ] in
          (compose_subst s2 s, TySeq (subst_ty s2 elem_ty))
      | _ ->
          type_error
            (Arity_mismatch ("tail", 1, List.length args.items))
            { line = 0; col = 0 })
  | App { name = "append"; args; _ } -> (
      match args.items with
      | [ seq; elem ] ->
          let s1, seq_ty = infer_expr fresh_tyvar env seq in
          let env' = subst_env s1 env in
          let s2, elem_ty = infer_expr fresh_tyvar env' elem in
          let s12 = compose_subst s2 s1 in
          let seq_ty' = subst_ty s2 seq_ty in
          let s3 = unify { line = 0; col = 0 } [ (seq_ty', TySeq elem_ty) ] in
          let s123 = compose_subst s3 s12 in
          (s123, TySeq (subst_ty s123 elem_ty))
      | _ ->
          type_error
            (Arity_mismatch ("append", 2, List.length args.items))
            { line = 0; col = 0 })
  | App { name = "concat"; args; _ } -> (
      match args.items with
      | [ lhs; rhs ] ->
          let s1, lhs_ty = infer_expr fresh_tyvar env lhs in
          let env' = subst_env s1 env in
          let s2, rhs_ty = infer_expr fresh_tyvar env' rhs in
          let s12 = compose_subst s2 s1 in
          let elem_ty = fresh_tyvar () in
          let lhs_ty' = subst_ty s2 lhs_ty in
          let s3 =
            unify { line = 0; col = 0 }
              [ (lhs_ty', TySeq elem_ty); (rhs_ty, TySeq elem_ty) ]
          in
          let s123 = compose_subst s3 s12 in
          (s123, TySeq (subst_ty s123 elem_ty))
      | _ ->
          type_error
            (Arity_mismatch ("concat", 2, List.length args.items))
            { line = 0; col = 0 })
  | App { name; args; _ } -> (
      match List.assoc_opt name env with
      | None -> type_error (Unbound_variable name) { line = 0; col = 0 }
      | Some tysc ->
          let fn_ty = instantiate fresh_tyvar tysc in
          let s_acc, arg_tys =
            List.fold_left
              (fun (s_acc, tys) arg ->
                let env' = subst_env s_acc env in
                let s, ty = infer_expr fresh_tyvar env' arg in
                (compose_subst s s_acc, tys @ [ ty ]))
              ([], []) args.items
          in
          let ret_tv = fresh_tyvar () in
          let expected_fn = TyFun (arg_tys, ret_tv) in
          let fn_ty' = subst_ty s_acc fn_ty in
          let s_u = unify { line = 0; col = 0 } [ (fn_ty', expected_fn) ] in
          (compose_subst s_u s_acc, subst_ty s_u ret_tv))
  | Tuple { elems; _ } ->
      if elems.items = [] then ([], TyUnit)
      else
        let s_acc, tys =
          List.fold_left
            (fun (s_acc, tys) elem ->
              let env' = subst_env s_acc env in
              let s, ty = infer_expr fresh_tyvar env' elem in
              (compose_subst s s_acc, tys @ [ ty ]))
            ([], []) elems.items
        in
        (s_acc, TyTuple tys)
  | Sequence { elems; _ } -> infer_sequence_literal fresh_tyvar env elems.items
  | Paren { inner; _ } -> infer_expr fresh_tyvar env inner

(* ===== Check procedure bodies ===== *)

type proc_ctx = {
  env : tyenv;
  mutable_vars : (id * ty) list;
  return_ty : ty;
  in_loop : bool;
  fresh_tyvar : unit -> ty;
}

let check_simple_stmt (ctx : proc_ctx) loc (stmt : simple_stmt) : subst =
  match stmt with
  | Assign { name; value; _ } -> (
      match List.assoc_opt name ctx.mutable_vars with
      | Some var_ty ->
          let s1, val_ty = infer_expr ctx.fresh_tyvar ctx.env value in
          let var_ty' = subst_ty s1 var_ty in
          let s2 = unify loc [ (var_ty', val_ty) ] in
          compose_subst s2 s1
      | None -> type_error (Assign_to_non_variable name) loc)
  | Call { name; args; _ } -> (
      match List.assoc_opt name ctx.env with
      | None -> type_error (Unbound_variable name) loc
      | Some tysc ->
          let fn_ty = instantiate ctx.fresh_tyvar tysc in
          let s_acc, arg_tys =
            List.fold_left
              (fun (s_acc, tys) arg ->
                let env' = subst_env s_acc ctx.env in
                let s, ty = infer_expr ctx.fresh_tyvar env' arg in
                (compose_subst s s_acc, tys @ [ ty ]))
              ([], []) args.items
          in
          let ret_tv = ctx.fresh_tyvar () in
          let expected_fn = TyFun (arg_tys, ret_tv) in
          let fn_ty' = subst_ty s_acc fn_ty in
          let s_u = unify loc [ (fn_ty', expected_fn) ] in
          compose_subst s_u s_acc)
  | Return { value; _ } ->
      let s1, val_ty = infer_expr ctx.fresh_tyvar ctx.env value in
      let ret_ty' = subst_ty s1 ctx.return_ty in
      let s2 = unify loc [ (ret_ty', val_ty) ] in
      compose_subst s2 s1
  | Break _ ->
      if not ctx.in_loop then type_error Break_outside_loop loc;
      []
  | Await { cond; _ } ->
      let s1, cond_ty = infer_expr ctx.fresh_tyvar ctx.env cond in
      let s2 = unify loc [ (cond_ty, TyBool) ] in
      compose_subst s2 s1

let apply_subst_ctx s ctx =
  {
    ctx with
    env = subst_env s ctx.env;
    mutable_vars = List.map (fun (n, t) -> (n, subst_ty s t)) ctx.mutable_vars;
    return_ty = subst_ty s ctx.return_ty;
  }

let rec check_body (ctx : proc_ctx) (steps : body) : subst =
  match steps with
  | [] -> []
  | step :: rest ->
      let s1, ctx' = check_step ctx step in
      let ctx'' = apply_subst_ctx s1 ctx' in
      let s2 = check_body ctx'' rest in
      compose_subst s2 s1

and check_step (ctx : proc_ctx) (step : step) : subst * proc_ctx =
  match step with
  | EmptyStep _ -> ([], ctx)
  | SimpleStep { stmts; loc; _ } ->
      let s =
        List.fold_left
          (fun s_acc stmt ->
            let ctx' = apply_subst_ctx s_acc ctx in
            let s = check_simple_stmt ctx' loc stmt in
            compose_subst s s_acc)
          [] stmts.items
      in
      (s, ctx)
  | BlockStep { stmt = While { cond; body; _ }; loc } ->
      let s1, cond_ty = infer_expr ctx.fresh_tyvar ctx.env cond in
      let s2 = unify loc [ (cond_ty, TyBool) ] in
      let s12 = compose_subst s2 s1 in
      let ctx' = apply_subst_ctx s12 { ctx with in_loop = true } in
      let s3 = check_body ctx' body in
      (compose_subst s3 s12, ctx)
  | BlockStep { stmt = If { cond; body; _ }; loc } ->
      let s1, cond_ty = infer_expr ctx.fresh_tyvar ctx.env cond in
      let s2 = unify loc [ (cond_ty, TyBool) ] in
      let s12 = compose_subst s2 s1 in
      let ctx' = apply_subst_ctx s12 ctx in
      let s3 = check_body ctx' body in
      (compose_subst s3 s12, ctx)
  | LetStep { name; value; _ } ->
      let s1, val_ty = infer_expr ctx.fresh_tyvar ctx.env value in
      let env' = subst_env s1 ctx.env in
      let tysc = generalize env' (subst_ty s1 val_ty) in
      let ctx' =
        {
          ctx with
          env = (name, tysc) :: env';
          mutable_vars = (name, subst_ty s1 val_ty) :: ctx.mutable_vars;
        }
      in
      (s1, ctx')

(* ===== Check a module ===== *)

let check_module (m : module_def) : unit =
  let tyvar_counter = ref 0 in
  let fresh_tyvar () =
    let v = !tyvar_counter in
    incr tyvar_counter;
    TyVar v
  in
  let _env =
    List.fold_left
      (fun env (item : item) ->
        match item with
        | ConstDef { name; value; _ } ->
            let s1, ty = infer_expr fresh_tyvar env value in
            let env' = subst_env s1 env in
            let tysc = generalize env' (subst_ty s1 ty) in
            (name, tysc) :: env'
        | FunDef { name; params; body_expr; _ } ->
            let param_tys = List.map (fun _ -> fresh_tyvar ()) params.items in
            let param_env =
              List.map2
                (fun (_, pname) pty -> (pname, tysc_of_ty pty))
                params.items param_tys
            in
            let fn_env = param_env @ env in
            let s1, body_ty = infer_expr fresh_tyvar fn_env body_expr in
            let fn_ty = TyFun (List.map (subst_ty s1) param_tys, body_ty) in
            let env' = subst_env s1 env in
            let tysc = generalize env' (subst_ty s1 fn_ty) in
            (name, tysc) :: env'
        | VarDecl { name; value; _ } ->
            let s1, ty = infer_expr fresh_tyvar env value in
            let env' = subst_env s1 env in
            let tysc = tysc_of_ty (subst_ty s1 ty) in
            (name, tysc) :: env'
        | ProcDef { name = proc_name; params; body; _ } ->
            let param_tys = List.map (fun _ -> fresh_tyvar ()) params.items in
            let param_env =
              List.map2
                (fun (_, pname) pty -> (pname, tysc_of_ty pty))
                params.items param_tys
            in
            let return_ty = fresh_tyvar () in
            let fn_ty = TyFun (param_tys, return_ty) in
            let proc_env = ((proc_name, tysc_of_ty fn_ty) :: param_env) @ env in
            let mutable_vars =
              List.filter_map
                (fun (id, tysc) ->
                  match tysc with
                  | TyScheme ([], ty) ->
                      if
                        List.exists
                          (fun (item : item) ->
                            match item with
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
            let s1 = check_body ctx body in
            let fn_ty' =
              TyFun (List.map (subst_ty s1) param_tys, subst_ty s1 return_ty)
            in
            let env' = subst_env s1 env in
            let tysc = generalize env' (subst_ty s1 fn_ty') in
            (proc_name, tysc) :: env'
        | Process { proc; lo; hi; _ } ->
            let loc = { line = 0; col = 0 } in
            (match List.assoc_opt proc env with
            | None -> type_error (Unbound_variable proc) loc
            | Some _ -> ());
            let s1, lo_ty = infer_expr fresh_tyvar env lo in
            let env' = subst_env s1 env in
            let s2, hi_ty = infer_expr fresh_tyvar env' hi in
            let s12 = compose_subst s2 s1 in
            let s3 = unify loc [ (subst_ty s2 lo_ty, TyInt); (hi_ty, TyInt) ] in
            subst_env (compose_subst s3 s12) env)
      [] m.items
  in
  ()

(* ===== Check a program ===== *)

let check (prog : program) : unit =
  List.iter (fun m -> check_module m) prog.modules

(* ===== Pretty printing for error messages ===== *)

let rec string_of_ty = function
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyUnit -> "unit"
  | TyVar v ->
      let c = Char.chr (Char.code 'a' + (v mod 26)) in
      Printf.sprintf "'%c" c
  | TyTuple tys -> "(" ^ String.concat ", " (List.map string_of_ty tys) ^ ")"
  | TySeq ty -> "[" ^ string_of_ty ty ^ "]"
  | TyFun (params, ret) ->
      let params_s =
        match params with
        | [ p ] -> paren_fun_ty p
        | _ -> "(" ^ String.concat ", " (List.map string_of_ty params) ^ ")"
      in
      params_s ^ " -> " ^ string_of_ty ret

and paren_fun_ty = function
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
  | Return_type_mismatch -> "return type mismatch"
  | Assign_to_non_variable id ->
      Printf.sprintf "Cannot assign to %s: not a mutable variable" id
  | Recursive_type -> "Recursive type detected"
