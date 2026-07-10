(* ===== Compilation driver =====

   The full pipeline behind one entry point; every failure mode surfaces as
   a located diagnostic instead of a raw exception, so callers only need one
   error-reporting path. *)

type diagnostic = { loc : Generic_ast.loc; message : string }
type output = { tla_module : Tla.Tla_ast.tla_module; source_map : Source_map.t }

let loc_of_position (pos : Lexing.position) : Generic_ast.loc =
  { line = pos.pos_lnum; col = pos.pos_cnum - pos.pos_bol + 1 }

let parse (source : string) : (Surface_ast.program, diagnostic) result =
  let lexbuf = Lexing.from_string source in
  match Parser.program Lexer.main lexbuf with
  | prog -> Ok prog
  | exception Parser.Error ->
      Error
        {
          loc = loc_of_position (Lexing.lexeme_start_p lexbuf);
          message = "Syntax error";
        }
  | exception Generic_ast.Parse_error (loc, message) -> Error { loc; message }
  | exception Lexer.Error message ->
      Error { loc = loc_of_position (Lexing.lexeme_start_p lexbuf); message }

(* A module with no processes has no behavior: ProcSet, Next, and Spec would
   be degenerate (previously even syntactically invalid — an empty
   [ProcSet ==]), and TLC would have nothing to check. Diagnosed here rather
   than in Typing so pure-definition modules remain checkable in isolation. *)
let check_has_processes (prog : Surface_ast.program) : diagnostic option =
  List.find_map
    (fun (m : Surface_ast.module_def) ->
      if
        List.exists
          (fun (item : Surface_ast.item) ->
            match item.desc with Generic_ast.Process _ -> true | _ -> false)
          m.items
      then None
      else
        Some
          {
            loc = m.mod_loc;
            message =
              Printf.sprintf
                "module %s defines no processes; there is no behavior to check"
                m.mod_name;
          })
    prog

(* The sidecar config's property/invariant names go verbatim into the
   generated .cfg, where a typo only fails later inside TLC with an obscure
   message. Validate them against the module items instead: a property must
   name a [property] item ("Termination" is compiler-provided), an invariant
   a [def]. *)
let check_config_names (config : Config.t) (prog : Surface_ast.program) :
    diagnostic option =
  let item_names f (m : Surface_ast.module_def) =
    List.filter_map (fun (item : Surface_ast.item) -> f item.desc) m.items
  in
  List.find_map
    (fun (m : Surface_ast.module_def) ->
      let props =
        item_names
          (function Generic_ast.PropDef { name; _ } -> Some name | _ -> None)
          m
      in
      let defs =
        item_names
          (function Generic_ast.ConstDef { name; _ } -> Some name | _ -> None)
          m
      in
      let missing kind item_kind available names =
        List.find_map
          (fun name ->
            if List.mem name available then None
            else
              Some
                {
                  loc = m.mod_loc;
                  message =
                    Printf.sprintf
                      "the sidecar config lists %s '%s', but module %s defines \
                       no such %s"
                      kind name m.mod_name item_kind;
                })
          names
      in
      let user_properties =
        List.filter (fun p -> p <> "Termination") config.properties
      in
      match missing "property" "property item" props user_properties with
      | Some d -> Some d
      | None -> missing "invariant" "def" defs config.invariants)
    prog

let compile ?config (source : string) : (output list, diagnostic) result =
  match parse source with
  | Error d -> Error d
  | Ok prog -> (
      match
        (* Scope and step-structure checks precede inference so Typing can
           assume a well-scoped tree; the domain-constancy check follows it
           so a domain's type errors come first. *)
        Check_scope.check prog;
        Check_steps.check prog;
        Typing.check prog;
        Check_process_domains.check prog
      with
      | exception Check_scope.Error (message, loc) -> Error { loc; message }
      | exception Check_steps.Error (message, loc) -> Error { loc; message }
      | exception Typing.Type_error (err, loc) ->
          Error { loc; message = Typing.string_of_type_error err }
      | exception Check_process_domains.Error (message, loc) ->
          Error { loc; message }
      | () -> (
          match check_has_processes prog with
          | Some d -> Error d
          | None -> (
              match Option.bind config (fun c -> check_config_names c prog) with
              | Some d -> Error d
              | None -> (
                  let resolved = Alpha_convert.transform prog in
                  match
                    Check_temporal.check resolved;
                    Normalize_calls.normalize resolved
                  with
                  | exception Check_temporal.Error (message, loc) ->
                      Error { loc; message }
                  | exception Normalize_calls.Error (message, loc) ->
                      Error { loc; message }
                  | normalized -> (
                      match Linearize.linearize normalized with
                      | exception Linearize.Error (message, loc) ->
                          Error { loc; message }
                      | irs ->
                          let irs = List.map Fuse.fuse_module irs in
                          let irs =
                            List.map Canonicalize_locals.canonicalize_module irs
                          in
                          Ok
                            (List.map
                               (fun ir ->
                                 {
                                   tla_module =
                                     Emit_tla.generate_module ?config ir;
                                   source_map = Source_map.extract ir;
                                 })
                               irs))))))
