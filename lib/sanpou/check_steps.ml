open Generic_ast

(* ===== Step-structure checks: surface AST =====

   One statement list (SimpleStep / WithStep) is one atomic action; two
   structural facts about such a list are checked here, before typing:

   - its assignments become one conjunction of primed equations, so a
     whole-variable write cannot be combined with any other write to the
     same variable in the same step (path updates alone compose
     left-to-right into a single EXCEPT);
   - its call/return/break/continue slot is single-valued, so a control
     transfer must be the step's final statement — a second transfer would
     silently overwrite the first, and statements after a transfer would
     still execute inside the same action. *)

exception Error of string * loc

let error message loc = raise (Error (message, loc))

(* A whole-variable write cannot be combined with another write to the same
   variable in one step: there is no consistent value for the primed
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
        then
          error
            (Printf.sprintf
               "conflicting assignments to %s in one step: a whole-variable \
                assignment cannot be combined with another assignment to the \
                same variable"
               name)
            loc;
        (name, kind) :: seen)
      []
      (List.filter_map write stmts)
  in
  ()

let check_control_transfer_last (stmts : Surface_ast.simple_stmt list) : unit =
  let kind (stmt : Surface_ast.simple_stmt) =
    match stmt.desc with
    | Call _ -> Some "a procedure call"
    | Return _ -> Some "a return"
    | Break -> Some "a break"
    | Continue -> Some "a continue"
    | Assign _ | Await _ | Assert _ -> None
  in
  let rec go = function
    | [] | [ _ ] -> ()
    | stmt :: rest ->
        (match kind stmt with
        | Some k ->
            error
              (Printf.sprintf
                 "%s must be the last statement of its step: the following \
                  statements merge into the same atomic action and the \
                  transfer would discard them"
                 k)
              stmt.loc
        | None -> ());
        go rest
  in
  go stmts

let check_stmts stmts =
  check_no_conflicting_writes stmts;
  check_control_transfer_last stmts

let rec check_body (steps : Surface_ast.body) : unit =
  List.iter check_step steps

and check_step (step : Surface_ast.step) : unit =
  match step.desc with
  | SimpleStep stmts | WithStep { stmts; _ } -> check_stmts stmts
  | EmptyStep | VarStep _ -> ()
  | BlockStep (While { body; _ }) -> check_body body
  | BlockStep (If { body; else_body; _ }) ->
      check_body body;
      Option.iter check_body else_body
  | BlockStep (Either arms) -> List.iter check_body arms

let check_module (m : Surface_ast.module_def) : unit =
  List.iter
    (fun (item : Surface_ast.item) ->
      match item.desc with
      | ProcDef { body; _ } -> check_body body
      | ConstDef _ | PropDef _ | FunDef _ | VarDecl _ | Process _ -> ())
    m.items

let check (prog : Surface_ast.program) : unit = List.iter check_module prog
