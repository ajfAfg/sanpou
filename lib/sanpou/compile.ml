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

let compile ?config (source : string) : (output list, diagnostic) result =
  match parse source with
  | Error d -> Error d
  | Ok prog -> (
      match Typing.check prog with
      | exception Typing.Type_error (err, loc) ->
          Error { loc; message = Typing.string_of_type_error err }
      | () -> (
          match check_has_processes prog with
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
          | normalized ->
              let irs = Linearize.linearize normalized in
              Ok
                (List.map
                   (fun ir ->
                     {
                       tla_module = Emit_tla.generate_module ?config ir;
                       source_map = Source_map.extract ir;
                     })
                   irs))))
