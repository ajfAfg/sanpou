(* Property-based pretty-print/parse roundtrip: for a random surface tree,
   [Ast_printer.print_pretty] followed by [Compile]'s parse must reproduce the
   tree ([equal_loc] ignores locations). This generalizes the fixed roundtrip
   corpus in test_ast_printer.ml and probes the printer's parenthesization and
   the grammar's disambiguation (brace forms, tuples, else-if chains).

   The generators produce only trees the parser can output in the first place:
   - no [Builtin] nodes — the parser always yields [App]; builtins are
     resolved later, in [Alpha_convert];
   - non-negative [IntLit] — a negative literal parses as [UnOp (Neg, _)];
   - record literals are non-empty (bare [{}] is the empty set) and their
     labels are distinct (duplicates are a parse error);
   - [Either] has at least two arms and [PathTarget] a non-empty path, as in
     the grammar. *)

open Sanpou.Generic_ast
module Gen = QCheck2.Gen

let node desc = { desc; loc = { line = 0; col = 0 } }

(* Plain identifiers that are neither reserved words nor special-cased by the
   lexer. Callee/label/binder positions all draw from the same pool; collisions
   between them are intentional, since the parser must not care. *)
let names = [ "x"; "y"; "z"; "foo"; "bar"; "baz"; "s1"; "A_b" ]
let gen_name = Gen.oneof_list names

(* Strings have no escape sequences: any character except a double quote and a
   newline stands for itself. *)
let gen_string_lit =
  Gen.string_size
    ~gen:(Gen.oneof_list [ 'a'; 'z'; '0'; ' '; '\\'; '/' ])
    (Gen.int_bound 5)

let ( let* ) = Gen.bind

let gen_fields gen_value =
  let* k = Gen.int_range 1 3 in
  let labels = List.filteri (fun i _ -> i < k) names in
  Gen.flatten_list
    (List.map (fun l -> Gen.map (fun v -> (l, v)) gen_value) labels)

let gen_expr : Sanpou.Surface_ast.expr Gen.t =
  Gen.sized @@ Gen.fix
  @@ fun self n ->
  let leaf =
    Gen.oneof
      [
        Gen.map (fun v -> node (IntLit v)) Gen.nat_small;
        Gen.map (fun b -> node (BoolLit b)) Gen.bool;
        Gen.map (fun s -> node (StrLit s)) gen_string_lit;
        Gen.map (fun a -> node (AtomLit a)) gen_name;
        Gen.map (fun x -> node (Var x)) gen_name;
        Gen.pure (node Self);
      ]
  in
  if n <= 0 then leaf
  else
    let sub = self (n / 2) in
    let subs lo hi = Gen.list_size (Gen.int_range lo hi) sub in
    Gen.oneof
      [
        leaf;
        (let* op = Gen.oneof_list [ Neg; Not ] in
         Gen.map (fun e -> node (UnOp (op, e))) sub);
        (let* op =
           Gen.oneof_list
             [
               Plus;
               Minus;
               Mult;
               Div;
               Mod;
               Lt;
               Gt;
               LtEq;
               GtEq;
               Eq;
               Neq;
               And;
               Or;
               In;
             ]
         in
         Gen.map2 (fun l r -> node (BinOp (op, l, r))) sub sub);
        Gen.map2 (fun f args -> node (App (f, args))) gen_name (subs 0 3);
        Gen.map2 (fun e i -> node (Subscript (e, i))) sub sub;
        Gen.map2 (fun e f -> node (Field (e, f))) sub gen_name;
        Gen.map (fun fields -> node (Record fields)) (gen_fields sub);
        Gen.map2 (fun lo hi -> node (Range (lo, hi))) sub sub;
        Gen.map3
          (fun binder domain value -> node (MapInit { binder; domain; value }))
          gen_name sub sub;
        Gen.map (fun es -> node (SetLit es)) (subs 0 3);
        Gen.map3
          (fun binder domain pred -> node (SetComp { binder; domain; pred }))
          gen_name sub sub;
        Gen.map (fun es -> node (Tuple es)) (subs 0 3);
        Gen.map (fun es -> node (Sequence es)) (subs 0 3);
        Gen.map3 (fun c t e -> node (IfExpr (c, t, e))) sub sub sub;
        (let* quant = Gen.oneof_list [ Forall; Exists ] in
         Gen.map3
           (fun binder domain body ->
             node (Quant { quant; binder; domain; body }))
           gen_name sub sub);
      ]

let gen_assign_target =
  let gen_accessor =
    Gen.oneof
      [
        Gen.map (fun i -> AccIndex i) gen_expr;
        Gen.map (fun f -> AccField f) gen_name;
      ]
  in
  Gen.oneof
    [
      Gen.map (fun x -> VarTarget x) gen_name;
      Gen.map2
        (fun x path -> PathTarget (x, path))
        gen_name
        (Gen.list_size (Gen.int_range 1 2) gen_accessor);
    ]

let gen_simple_stmt : Sanpou.Surface_ast.simple_stmt Gen.t =
  Gen.oneof
    [
      Gen.map2 (fun t v -> node (Assign (t, v))) gen_assign_target gen_expr;
      Gen.map2
        (fun f args -> node (Call (f, args)))
        gen_name
        (Gen.list_size (Gen.int_bound 2) gen_expr);
      Gen.map (fun v -> node (Return v)) gen_expr;
      Gen.pure (node Break);
      Gen.pure (node Continue);
      Gen.map (fun c -> node (Await c)) gen_expr;
      Gen.map (fun c -> node (Assert c)) gen_expr;
    ]

let gen_simple_stmts = Gen.list_size (Gen.int_range 1 2) gen_simple_stmt

let gen_step : Sanpou.Surface_ast.step Gen.t =
  Gen.sized @@ Gen.fix
  @@ fun self n ->
  let leaf =
    Gen.oneof
      [
        Gen.map (fun stmts -> node (SimpleStep stmts)) gen_simple_stmts;
        Gen.pure (node EmptyStep);
        Gen.map2 (fun x v -> node (VarStep (x, v))) gen_name gen_expr;
        Gen.map3
          (fun binder domain stmts -> node (WithStep { binder; domain; stmts }))
          gen_name gen_expr gen_simple_stmts;
      ]
  in
  if n <= 0 then leaf
  else
    let body = Gen.list_size (Gen.int_bound 2) (self (n / 2)) in
    let block =
      Gen.oneof
        [
          Gen.map2 (fun cond body -> While { cond; body }) gen_expr body;
          Gen.map3
            (fun cond body else_body -> If { cond; body; else_body })
            gen_expr body (Gen.option body);
          Gen.map
            (fun arms -> Either arms)
            (Gen.list_size (Gen.int_range 2 3) body);
        ]
    in
    Gen.oneof [ leaf; Gen.map (fun b -> node (BlockStep b)) block ]

let gen_body = Gen.list_size (Gen.int_bound 3) gen_step

let gen_item : Sanpou.Surface_ast.item Gen.t =
  let gen_params = Gen.list_size (Gen.int_bound 3) gen_name in
  Gen.oneof
    [
      Gen.map2
        (fun name value -> node (ConstDef { name; value }))
        gen_name gen_expr;
      Gen.map2
        (fun name value -> node (PropDef { name; value }))
        gen_name gen_expr;
      Gen.map3
        (fun name params body_expr -> node (FunDef { name; params; body_expr }))
        gen_name gen_params gen_expr;
      Gen.map2
        (fun name init -> node (VarDecl { name; init }))
        gen_name
        (Gen.oneof
           [
             Gen.map (fun v -> InitValue v) gen_expr;
             Gen.map (fun d -> InitIn d) gen_expr;
           ]);
      Gen.map3
        (fun name params body -> node (ProcDef { name; params; body }))
        gen_name gen_params gen_body;
      (let* fairness = Gen.oneof_list [ Unfair; WeakFair; StrongFair ] in
       Gen.map3
         (fun name proc domain ->
           node (Process { name; proc; fairness; domain }))
         gen_name gen_name gen_expr);
    ]

let gen_program : Sanpou.Surface_ast.program Gen.t =
  Gen.list_size (Gen.int_range 1 2)
    (Gen.map2
       (fun mod_name items ->
         { mod_name; items; mod_loc = { line = 0; col = 0 } })
       gen_name
       (Gen.list_size (Gen.int_bound 4) gen_item))

let parse input =
  input |> Lexing.from_string |> Sanpou.Parser.program Sanpou.Lexer.main

let show_program = show_program pp_id pp_id
let equal_program = equal_program equal_id equal_id

let roundtrip prog =
  let printed = Sanpou.Ast_printer.print_pretty prog in
  match parse printed with
  | reparsed ->
      equal_program prog reparsed
      || QCheck2.Test.fail_reportf
           "reparsed tree differs@.--- printed ---@.%s@.--- reparsed ---@.%s"
           printed (show_program reparsed)
  | exception exn ->
      QCheck2.Test.fail_reportf "printed program does not parse: %s@.%s"
        (Printexc.to_string exn) printed

let roundtrip_test =
  QCheck2.Test.make ~count:1000 ~name:"pretty-print/parse roundtrip"
    ~print:show_program gen_program roundtrip

let () =
  Alcotest.run "ParsePbt"
    [ ("roundtrip", [ QCheck_alcotest.to_alcotest roundtrip_test ]) ]
