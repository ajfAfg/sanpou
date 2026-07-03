open Ast

(* ===== Alpha conversion: AST → AST with unique variable names ===== *)

(* BinderVar = MapInit binder: never becomes a TLA state variable, but is
   still needed to demangle descriptions *)
type rename_kind = ParamVar | LocalVar | BinderVar

type rename = {
  tla_name : string;
  original : string;
  proc : string;
  kind : rename_kind;
}

type alpha_module = { ast : Ast.module_def; renames : rename list }

type alpha_state = {
  var_name_counter : int ref;
  renames : rename list ref;
  current_proc : string ref;
}

let create_state () =
  { var_name_counter = ref 0; renames = ref []; current_proc = ref "" }

let fresh_var_name st base =
  incr st.var_name_counter;
  base ^ "__" ^ string_of_int !(st.var_name_counter)

let collect_rename st ~original ~tla_name ~kind =
  st.renames :=
    { tla_name; original; proc = !(st.current_proc); kind } :: !(st.renames)

let get_renames st = List.rev !(st.renames)

let resolve env name =
  match List.assoc_opt name env with Some n -> n | None -> name

(* ===== Expression alpha conversion ===== *)

let rec alpha_expr env st (e : expr) : expr =
  let desc =
    match e.desc with
    | (IntLit _ | BoolLit _ | Self) as d -> d
    | Var name -> Var (resolve env name)
    | UnOp (op, rhs) -> UnOp (op, alpha_expr env st rhs)
    | BinOp (op, lhs, rhs) ->
        BinOp (op, alpha_expr env st lhs, alpha_expr env st rhs)
    | App (name, args) -> App (name, List.map (alpha_expr env st) args)
    | Subscript (lhs, index) ->
        Subscript (alpha_expr env st lhs, alpha_expr env st index)
    | MapInit { binder; lo; hi; value } ->
        let binder' = fresh_var_name st binder in
        collect_rename st ~original:binder ~tla_name:binder' ~kind:BinderVar;
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
  { e with desc }

let alpha_assign_target env st = function
  | VarTarget name -> VarTarget (resolve env name)
  | SubscriptTarget (name, index) ->
      SubscriptTarget (resolve env name, alpha_expr env st index)

(* ===== Step and body alpha conversion ===== *)

let rec alpha_step st env (step : step) : step =
  let desc =
    match step.desc with
    | SimpleStep stmts ->
        let alpha_simple_stmt (stmt : simple_stmt) : simple_stmt =
          let desc =
            match stmt.desc with
            | Assign (target, value) ->
                Assign
                  (alpha_assign_target env st target, alpha_expr env st value)
            | Call (name, args) ->
                Call (name, List.map (alpha_expr env st) args)
            | Return value -> Return (alpha_expr env st value)
            | (Break | Continue) as d -> d
            | Await cond -> Await (alpha_expr env st cond)
          in
          { stmt with desc }
        in
        SimpleStep (List.map alpha_simple_stmt stmts)
    | EmptyStep -> EmptyStep
    | BlockStep stmt -> BlockStep (alpha_block_stmt st env stmt)
    | VarStep _ -> failwith "VarStep should be handled in alpha_body"
  in
  { step with desc }

and alpha_block_stmt st env = function
  | While { cond; body } ->
      While { cond = alpha_expr env st cond; body = alpha_body st env body }
  | If { cond; body; else_body } ->
      If
        {
          cond = alpha_expr env st cond;
          body = alpha_body st env body;
          else_body = Option.map (alpha_body st env) else_body;
        }

and alpha_body st env (steps : body) : body =
  match steps with
  | [] -> []
  | ({ desc = VarStep (name, value); _ } as step) :: rest ->
      let tla_name = fresh_var_name st name in
      collect_rename st ~original:name ~tla_name ~kind:LocalVar;
      let alpha_value = alpha_expr env st value in
      let new_env = (name, tla_name) :: env in
      { step with desc = VarStep (tla_name, alpha_value) }
      :: alpha_body st new_env rest
  | step :: rest -> alpha_step st env step :: alpha_body st env rest

(* ===== Module transformation ===== *)

let transform_module (m : Ast.module_def) : alpha_module =
  let st = create_state () in
  let items =
    List.map
      (fun (item : Ast.item) ->
        match item.desc with
        | ProcDef { name; params; body } ->
            st.current_proc := name;
            let env_rev, params_rev =
              List.fold_left
                (fun (env_acc, params_acc) param ->
                  let fresh = fresh_var_name st param in
                  collect_rename st ~original:param ~tla_name:fresh
                    ~kind:ParamVar;
                  ((param, fresh) :: env_acc, fresh :: params_acc))
                ([], []) params
            in
            let env = List.rev env_rev in
            let params = List.rev params_rev in
            {
              item with
              desc = ProcDef { name; params; body = alpha_body st env body };
            }
        | _ -> item)
      m.items
  in
  { ast = { m with items }; renames = get_renames st }

(* ===== Public API ===== *)

let transform (prog : Ast.program) : alpha_module list =
  List.map transform_module prog
