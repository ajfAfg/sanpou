open Generic_ast

(* ===== Scope and context checks: surface AST =====

   Everything about a program that is decided by names and syntactic
   context alone, checked before type inference so Typing can assume a
   well-scoped tree:

   - every referenced name is bound (builtins form the outermost scope for
     applications; they are not values);
   - callable kinds: functions and procedures are second-class, so a
     callable cannot be used as a value, only a callable can be applied,
     and only a procedure can be called as a statement;
   - statement context: [break]/[continue] only inside a loop, [self] only
     inside a procedure body, assignment only to a mutable variable;
   - a process root is a previously declared, nullary procedure;
   - module-level declarations and atom texts do not collide with the
     names the emitter reserves ([Reserved_names]);
   - module names are unique across the program.

   Name resolution is sequential and lexical, mirroring Typing's
   environment: a definition is visible from its own item onward (defs are
   not recursive; procedures see themselves), a later binding of the same
   name shadows the earlier one from its point onward, and builtins are
   shadowed by definitions of the same name. *)

exception Error of string * loc

let error fmt =
  Printf.ksprintf (fun message loc -> raise (Error (message, loc))) fmt

(* What a name in scope stands for. Callable-ness is syntactic: only a
   [def f(x) = ...] or a [procedure] binds a callable, and since callables
   cannot be passed, returned, or stored (rejected below), no value binding
   can ever hold one. [Procedure] carries the declared parameter count so
   the process-root arity check needs no types. *)
type binding =
  | Value (* const/property defs, parameters, binders *)
  | Mutable (* module [var]s and procedure-local [var]s: assignable *)
  | Function
  | Procedure of int

(* Most-recent-first: [List.assoc_opt] finds the innermost binding. *)
type env = (id * binding) list

(* ===== Expressions ===== *)

(* [in_proc] is whether the expression sits inside a procedure body, the
   only place [self] is bound. *)
let rec check_expr ~in_proc (env : env) (e : Surface_ast.expr) : unit =
  let check = check_expr ~in_proc env in
  match e.desc with
  | IntLit _ | BoolLit _ | StrLit _ -> ()
  | AtomLit name ->
      (* The atom's text is its TLA+ name (the model value's identity in
         traces and the .cfg), so a reserved text cannot be renamed away
         like a declaration could. *)
      if Reserved_names.is_reserved name then
        error
          "%s is reserved: it collides with a name in the emitted TLA+ module"
          name e.loc
  | Var name -> (
      match List.assoc_opt name env with
      | Some (Value | Mutable) -> ()
      | Some (Function | Procedure _) ->
          (* Functions and procedures are second-class: TLA+ operators
             cannot be passed as plain values. *)
          error
            "%s is a function or procedure and cannot be used as a value; \
             apply it instead"
            name e.loc
      | None -> error "Unbound variable: %s" name e.loc)
  | Self ->
      (* [self] is the process id, bound only inside procedure bodies. *)
      if not in_proc then error "self can only be used inside a procedure" e.loc
  | UnOp (_, rhs) -> check rhs
  | BinOp (_, lhs, rhs) ->
      check lhs;
      check rhs
  | App (name, args) ->
      (* Lexical resolution, mirroring Alpha_convert: a definition in scope
         shadows the builtin of the same name; builtins are the outermost
         scope. A parameter or local can never be applied — nothing but a
         callable declaration can bind a callable. *)
      (match List.assoc_opt name env with
      | Some (Function | Procedure _) -> ()
      | Some (Value | Mutable) -> error "%s is not a function" name e.loc
      | None ->
          if Option.is_none (Builtin.of_name name) then
            error "Unbound variable: %s" name e.loc);
      List.iter check args
  | Builtin (_, args) -> List.iter check args
  | Subscript (lhs, index) ->
      check lhs;
      check index
  | Field (record, _) -> check record
  | Record fields -> List.iter (fun (_, e) -> check e) fields
  | Range (lo, hi) ->
      check lo;
      check hi
  | MapInit { binder; domain; value } ->
      check domain;
      check_expr ~in_proc ((binder, Value) :: env) value
  | SetLit elems | Tuple elems | Sequence elems -> List.iter check elems
  | SetComp { binder; domain; pred } ->
      check domain;
      check_expr ~in_proc ((binder, Value) :: env) pred
  | IfExpr (cond, then_e, else_e) ->
      check cond;
      check then_e;
      check else_e
  | Quant { binder; domain; body; _ } ->
      check domain;
      check_expr ~in_proc ((binder, Value) :: env) body

(* ===== Procedure bodies ===== *)

type ctx = { env : env; in_loop : bool }

(* Statement-level errors (assignment targets, calls, break/continue) point
   at the statement's own location; expression errors point at the
   expression. *)
let check_simple_stmt (ctx : ctx) (stmt : Surface_ast.simple_stmt) : unit =
  let loc = stmt.loc in
  let check_value = check_expr ~in_proc:true ctx.env in
  let check_target name =
    (* Targets resolve lexically like reads: a binder or parameter that
       shadows a mutable variable is not itself assignable. *)
    match List.assoc_opt name ctx.env with
    | Some Mutable -> ()
    | Some (Value | Function | Procedure _) | None ->
        error "Cannot assign to %s: not a mutable variable" name loc
  in
  match stmt.desc with
  | Assign (VarTarget name, value) ->
      check_target name;
      check_value value
  | Assign (PathTarget (name, path), value) ->
      check_target name;
      check_value value;
      List.iter
        (function AccIndex index -> check_value index | AccField _ -> ())
        path
  | Call (name, args) ->
      (* A statement call discards no value only a procedure can produce
         steps for; a def function here would reach the emitter's
         procedure table and crash. *)
      (match List.assoc_opt name ctx.env with
      | Some (Procedure _) -> ()
      | Some (Value | Mutable | Function) | None ->
          error "%s is not a procedure" name loc);
      List.iter check_value args
  | Return value -> check_value value
  | Break -> if not ctx.in_loop then error "break outside of loop" loc
  | Continue -> if not ctx.in_loop then error "continue outside of loop" loc
  | Await cond | Assert cond -> check_value cond

let rec check_body (ctx : ctx) (steps : Surface_ast.body) : unit =
  match steps with
  | [] -> ()
  | step :: rest -> check_body (check_step ctx step) rest

(* Returns the context for the following steps: VarStep extends it, every
   other step leaves it unchanged. *)
and check_step (ctx : ctx) (step : Surface_ast.step) : ctx =
  match step.desc with
  | EmptyStep -> ctx
  | SimpleStep stmts ->
      List.iter (check_simple_stmt ctx) stmts;
      ctx
  | BlockStep (While { cond; body }) ->
      check_expr ~in_proc:true ctx.env cond;
      check_body { ctx with in_loop = true } body;
      ctx
  | BlockStep (If { cond; body; else_body }) ->
      check_expr ~in_proc:true ctx.env cond;
      check_body ctx body;
      Option.iter (check_body ctx) else_body;
      ctx
  | BlockStep (Either arms) ->
      List.iter (check_body ctx) arms;
      ctx
  | WithStep { binder; domain; stmts } ->
      check_expr ~in_proc:true ctx.env domain;
      (* The binder is readable but not assignable — and it shadows any
         same-named mutable variable, so that name is not writable in the
         step either (alpha-conversion resolves the target to the binder). *)
      let binder_ctx = { ctx with env = (binder, Value) :: ctx.env } in
      List.iter (check_simple_stmt binder_ctx) stmts;
      ctx
  | VarStep (name, value) ->
      (* The initializer sees the scope before the binding. *)
      check_expr ~in_proc:true ctx.env value;
      { ctx with env = (name, Mutable) :: ctx.env }

(* ===== Modules ===== *)

let check_module (m : Surface_ast.module_def) : unit =
  (* Module-level names shadow sequentially like every other binding; the
     flat TLA+ namespace is reconciled by alpha-conversion, which renames
     the shadowed declarations. Names the emitter itself generates (or
     pulls in via EXTENDS) are reserved outright. *)
  let declare loc name =
    if Reserved_names.is_reserved name then
      error "%s is reserved: it collides with a name in the emitted TLA+ module"
        name loc
  in
  let (_ : env) =
    List.fold_left
      (fun env (item : Surface_ast.item) ->
        match item.desc with
        | ConstDef { name; value } ->
            declare item.loc name;
            (* the value sees the preceding bindings, not the new one *)
            check_expr ~in_proc:false env value;
            (name, Value) :: env
        | PropDef { name; value } ->
            declare item.loc name;
            check_expr ~in_proc:false env value;
            (name, Value) :: env
        | FunDef { name; params; body_expr } ->
            declare item.loc name;
            (* the body sees the parameters but not the function itself:
               defs are not recursive *)
            let body_env = List.map (fun p -> (p, Value)) params @ env in
            check_expr ~in_proc:false body_env body_expr;
            (name, Function) :: env
        | VarDecl { name; init } ->
            declare item.loc name;
            (match init with
            | InitValue value -> check_expr ~in_proc:false env value
            | InitIn domain -> check_expr ~in_proc:false env domain);
            (name, Mutable) :: env
        | ProcDef { name; params; body } ->
            declare item.loc name;
            (* The body sees the procedure itself (self-recursion), its
               parameters, and the preceding module bindings — the
               procedure's own name shadowing a same-named parameter. *)
            let proc = (name, Procedure (List.length params)) in
            let body_env =
              proc :: (List.map (fun p -> (p, Value)) params @ env)
            in
            check_body { env = body_env; in_loop = false } body;
            proc :: env
        | Process { name; proc; domain; _ } ->
            declare item.loc name;
            (match List.assoc_opt proc env with
            (* The root must be a procedure: a def function here would
               reach the emitter's procedure table and crash. *)
            | Some (Value | Mutable | Function) | None ->
                error "%s is not a procedure" proc item.loc
            (* ... and a nullary one: the wrapper pushes an empty argument
               list, so parameters would silently start as the null frame
               sentinel and blow up inside TLC. *)
            | Some (Procedure n) when n > 0 ->
                error
                  "%s takes %d parameter%s; a process root procedure must take \
                   none (the process wrapper calls it without arguments)"
                  proc n
                  (if n = 1 then "" else "s")
                  item.loc
            | Some (Procedure _) -> ());
            check_expr ~in_proc:false env domain;
            (* The process name is not a binding: nothing can reference a
               process (it only claims the name in the emitted module). *)
            env)
      [] m.items
  in
  ()

(* ===== Programs ===== *)

let check (prog : Surface_ast.program) : unit =
  (* Each module becomes its own <name>.tla output file, so a duplicate
     would silently overwrite the previous module's spec. *)
  let (_ : id list) =
    List.fold_left
      (fun seen (m : Surface_ast.module_def) ->
        if List.mem m.mod_name seen then
          error "module %s is already defined" m.mod_name m.mod_loc;
        m.mod_name :: seen)
      [] prog
  in
  List.iter check_module prog
