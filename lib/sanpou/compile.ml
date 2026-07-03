(* ===== Compilation driver =====

   The full pipeline behind one entry point; every failure mode surfaces as
   a located diagnostic instead of a raw exception, so callers only need one
   error-reporting path. *)

type diagnostic = { loc : Ast.loc; message : string }
type output = { tla_module : Tla.Tla_ast.tla_module; source_map : Source_map.t }

let loc_of_position (pos : Lexing.position) : Ast.loc =
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
  | exception Lexer.Error message ->
      Error { loc = loc_of_position (Lexing.lexeme_start_p lexbuf); message }

let compile ?config (source : string) : (output list, diagnostic) result =
  match parse source with
  | Error d -> Error d
  | Ok prog -> (
      match Typing.check prog with
      | exception Typing.Type_error (err, loc) ->
          Error { loc; message = Typing.string_of_type_error err }
      | () -> (
          match Alpha_convert.transform prog |> Normalize_calls.normalize with
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
                   irs)))
