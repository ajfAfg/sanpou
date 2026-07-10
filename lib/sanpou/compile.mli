(** The compilation pipeline behind one entry point: parse, type-check,
    alpha-convert, linearize and emit, with every failure surfaced as a located
    diagnostic. *)

type diagnostic = { loc : Generic_ast.loc; message : string }
type output = { tla_module : Tla.Tla_ast.tla_module; source_map : Source_map.t }

val parse : string -> (Surface_ast.program, diagnostic) result
(** Parse source text; lexical and syntax errors become diagnostics. *)

val compile :
  ?config:Config.t ->
  ?optimize:bool ->
  string ->
  (output list, diagnostic) result
(** Run the full pipeline on source text, one [output] per module. [optimize]
    (default [true]) applies the state-space reduction passes (action fusion,
    dead-local canonicalization); disabling it exists so tests can check that
    the passes preserve the observable reachable states. *)
