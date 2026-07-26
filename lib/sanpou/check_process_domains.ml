open Generic_ast

(* ===== Process domain constancy: surface AST =====

   PlusCal requires a process set to be a constant expression: the emitted
   spec fixes ProcSet and the domains of pc/stack at Init, so a domain that
   reads mutable state breaks TLC at runtime the moment the state changes.
   sanpou has no CONSTANT stage — literals, atoms, and defs form the constant
   fragment — so a module-level name is non-constant iff it is a [var], or a
   def whose body (transitively) mentions one. Runs after [Typing] (a
   domain's type errors come first, matching the order the combined checker
   reported them in). *)

exception Error of string * loc

(* The first reference to a non-constant name in [e], with its location. *)
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

let check_module (m : Surface_ast.module_def) : unit =
  (* [nonconst] accumulates the module-level names whose values can change
     over a behavior: vars, and defs that (transitively) read one. *)
  let nonconst_def nonconst name body =
    match first_nonconst_ref nonconst body with
    | Some _ -> name :: nonconst
    | None -> nonconst
  in
  let (_ : id list) =
    List.fold_left
      (fun nonconst (item : Surface_ast.item) ->
        match item.desc with
        | ConstDef { name; value } -> nonconst_def nonconst name value
        | PropDef { name; value } -> nonconst_def nonconst name value
        | FunDef { name; params; body_expr } -> (
            (* Params shadow module names inside the body, but the filtered
               list is only for the body check: the accumulator keeps them. *)
            let body_nonconst =
              List.filter (fun n -> not (List.mem n params)) nonconst
            in
            match first_nonconst_ref body_nonconst body_expr with
            | Some _ -> name :: nonconst
            | None -> nonconst)
        | VarDecl { name; _ } -> name :: nonconst
        | ProcDef _ -> nonconst
        | Process { domain; _ } ->
            (match first_nonconst_ref nonconst domain with
            | Some (name, loc) ->
                raise
                  (Error
                     ( Printf.sprintf
                         "a process ID set must be constant, but %s is mutable \
                          state (or depends on it)"
                         name,
                       loc ))
            | None -> ());
            nonconst)
      [] m.items
  in
  ()

let check (prog : Surface_ast.program) : unit = List.iter check_module prog
