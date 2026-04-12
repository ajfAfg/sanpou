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

let rec alpha_expr env = function
  | IntLit _ as e -> e
  | BoolLit _ as e -> e
  | Var r -> Var { r with name = resolve env r.name }
  | UnOp r -> UnOp { r with rhs = alpha_expr env r.rhs }
  | BinOp r ->
      BinOp { r with lhs = alpha_expr env r.lhs; rhs = alpha_expr env r.rhs }
  | App r ->
      App
        {
          r with
          args = { r.args with items = List.map (alpha_expr env) r.args.items };
        }
  | Tuple r ->
      Tuple
        {
          r with
          elems =
            { r.elems with items = List.map (alpha_expr env) r.elems.items };
        }
  | Sequence r ->
      Sequence
        {
          r with
          elems =
            { r.elems with items = List.map (alpha_expr env) r.elems.items };
        }
  | Paren r -> Paren { r with inner = alpha_expr env r.inner }

(* ===== Statement alpha conversion ===== *)

let alpha_simple_stmt env = function
  | Assign r ->
      Assign
        { r with name = resolve env r.name; value = alpha_expr env r.value }
  | Call r ->
      Call
        {
          r with
          args = { r.args with items = List.map (alpha_expr env) r.args.items };
        }
  | Return r -> Return { r with value = alpha_expr env r.value }
  | Break _ as s -> s
  | Await r -> Await { r with cond = alpha_expr env r.cond }

let alpha_comma_list env (cl : simple_stmt comma_list) =
  { cl with items = List.map (alpha_simple_stmt env) cl.items }

(* ===== Step and body alpha conversion ===== *)

let rec alpha_step st env = function
  | SimpleStep r -> SimpleStep { r with stmts = alpha_comma_list env r.stmts }
  | EmptyStep _ as s -> s
  | BlockStep r -> BlockStep { r with stmt = alpha_block_stmt st env r.stmt }
  | LetStep _ -> failwith "LetStep should be handled in alpha_body"

and alpha_block_stmt st env = function
  | While r ->
      While
        { r with cond = alpha_expr env r.cond; body = alpha_body st env r.body }
  | If r ->
      If
        { r with cond = alpha_expr env r.cond; body = alpha_body st env r.body }

and alpha_body st env steps =
  match steps with
  | [] -> []
  | LetStep r :: rest ->
      let tla_name = fresh_var_name st r.name in
      collect_local_var st tla_name;
      let alpha_value = alpha_expr env r.value in
      let new_env = (r.name, tla_name) :: env in
      LetStep { r with name = tla_name; value = alpha_value }
      :: alpha_body st new_env rest
  | step :: rest -> alpha_step st env step :: alpha_body st env rest

(* ===== Module transformation ===== *)

let transform_module (m : Cst.module_def) : alpha_module =
  let st = create_state () in
  let items =
    List.map
      (fun (item : Cst.item) ->
        match item with
        | ProcDef r -> ProcDef { r with body = alpha_body st [] r.body }
        | _ -> item)
      m.items
  in
  { cst = { m with items }; local_vars = get_local_vars st }

(* ===== Public API ===== *)

let transform (prog : Cst.program) : alpha_module list =
  List.map transform_module prog.modules
