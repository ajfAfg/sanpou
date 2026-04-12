open Cst

(* ===== Alpha conversion: CST → CST with unique variable names ===== *)

type alpha_module = { cst : Cst.module_def; local_vars : string list }

type alpha_state = {
  var_name_counter : int ref;
  state_local_vars : string list ref;
}

let create_state () = { var_name_counter = ref 0; state_local_vars = ref [] }

let fresh_var_name st base =
  incr st.var_name_counter;
  base ^ "__" ^ string_of_int !(st.var_name_counter)

let collect_local_var st name =
  st.state_local_vars := name :: !(st.state_local_vars)

let get_local_vars st = List.rev !(st.state_local_vars)

let resolve env name =
  match List.assoc_opt name env with Some n -> n | None -> name

(* ===== Expression alpha conversion ===== *)

let rec alpha_expr env st = function
  | IntLit _ as e -> e
  | BoolLit _ as e -> e
  | Var r -> Var { r with name = resolve env r.name }
  | Self _ as e -> e
  | UnOp r -> UnOp { r with rhs = alpha_expr env st r.rhs }
  | BinOp r ->
      BinOp
        { r with lhs = alpha_expr env st r.lhs; rhs = alpha_expr env st r.rhs }
  | App r ->
      App
        {
          r with
          args =
            { r.args with items = List.map (alpha_expr env st) r.args.items };
        }
  | Subscript r ->
      Subscript
        {
          r with
          lhs = alpha_expr env st r.lhs;
          index = alpha_expr env st r.index;
        }
  | MapInit r ->
      let binder' = fresh_var_name st r.binder in
      let env' = (r.binder, binder') :: env in
      MapInit
        {
          r with
          binder = binder';
          lo = alpha_expr env st r.lo;
          hi = alpha_expr env st r.hi;
          value = alpha_expr env' st r.value;
        }
  | Tuple r ->
      Tuple
        {
          r with
          elems =
            { r.elems with items = List.map (alpha_expr env st) r.elems.items };
        }
  | Sequence r ->
      Sequence
        {
          r with
          elems =
            { r.elems with items = List.map (alpha_expr env st) r.elems.items };
        }
  | Paren r -> Paren { r with inner = alpha_expr env st r.inner }

and alpha_assign_target env st = function
  | VarTarget r -> VarTarget { r with name = resolve env r.name }
  | SubscriptTarget r ->
      SubscriptTarget
        { r with name = resolve env r.name; index = alpha_expr env st r.index }

(* ===== Step and body alpha conversion ===== *)

let rec alpha_step st env = function
  | SimpleStep r ->
      let alpha_simple_stmt = function
        | Assign assign_r ->
            Assign
              {
                assign_r with
                target = alpha_assign_target env st assign_r.target;
                value = alpha_expr env st assign_r.value;
              }
        | Call call_r ->
            Call
              {
                call_r with
                args =
                  {
                    call_r.args with
                    items = List.map (alpha_expr env st) call_r.args.items;
                  };
              }
        | Return return_r ->
            Return { return_r with value = alpha_expr env st return_r.value }
        | Break _ as simple_stmt -> simple_stmt
        | Continue _ as simple_stmt -> simple_stmt
        | Await await_r ->
            Await { await_r with cond = alpha_expr env st await_r.cond }
      in
      let stmts =
        { r.stmts with items = List.map alpha_simple_stmt r.stmts.items }
      in
      SimpleStep { r with stmts }
  | EmptyStep _ as s -> s
  | BlockStep r -> BlockStep { r with stmt = alpha_block_stmt st env r.stmt }
  | VarStep _ -> failwith "VarStep should be handled in alpha_body"

and alpha_block_stmt st env = function
  | While r ->
      While
        {
          r with
          cond = alpha_expr env st r.cond;
          body = alpha_body st env r.body;
        }
  | If r ->
      If
        {
          r with
          cond = alpha_expr env st r.cond;
          body = alpha_body st env r.body;
          else_branch =
            Option.map
              (fun (else_t, else_lb, else_body, else_rb) ->
                (else_t, else_lb, alpha_body st env else_body, else_rb))
              r.else_branch;
        }

and alpha_body st env steps =
  match steps with
  | [] -> []
  | VarStep r :: rest ->
      let tla_name = fresh_var_name st r.name in
      collect_local_var st tla_name;
      let alpha_value = alpha_expr env st r.value in
      let new_env = (r.name, tla_name) :: env in
      VarStep { r with name = tla_name; value = alpha_value }
      :: alpha_body st new_env rest
  | step :: rest -> alpha_step st env step :: alpha_body st env rest

(* ===== Module transformation ===== *)

let transform_module (m : Cst.module_def) : alpha_module =
  let st = create_state () in
  let items =
    List.map
      (fun (item : Cst.item) ->
        match item with
        | ProcDef r ->
            let env_rev, params_rev =
              List.fold_left
                (fun (env_acc, params_acc) (comma, param) ->
                  let fresh = fresh_var_name st param in
                  collect_local_var st fresh;
                  ((param, fresh) :: env_acc, (comma, fresh) :: params_acc))
                ([], []) r.params.items
            in
            let env = List.rev env_rev in
            let params = { r.params with items = List.rev params_rev } in
            ProcDef { r with params; body = alpha_body st env r.body }
        | _ -> item)
      m.items
  in
  { cst = { m with items }; local_vars = get_local_vars st }

(* ===== Public API ===== *)

let transform (prog : Cst.program) : alpha_module list =
  List.map transform_module prog.modules
