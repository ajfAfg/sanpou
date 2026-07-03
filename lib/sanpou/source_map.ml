type entry = {
  label : string;
  proc_name : string;
  description : string;
  line : int;
  col : int;
}

type var_kind = Global | Param | Local | CallRet of string (* callee proc *)

type var_entry = {
  tla_name : string;
  original : string;
  proc : string option;
  kind : var_kind;
}

type t = { module_name : string; entries : entry list; vars : var_entry list }

let extract (ir : Ir.module_ir) : t =
  let action_entries =
    List.concat_map
      (fun (proc : Ir.proc_ir) ->
        List.map
          (fun (action : Ir.action) ->
            {
              label = action.label;
              proc_name = action.source.proc_name;
              description = action.source.description;
              line = action.source.line;
              col = action.source.col;
            })
          proc.actions)
      ir.procs
  in
  (* Process wrapper actions are synthesized later in Emit_tla, so their
     source-map entries are synthesized here from the process declarations. *)
  let wrapper_entries =
    List.concat_map
      (fun (p : Ir.process_ir) ->
        [
          {
            label = Ir.wrapper_entry_label p.name;
            proc_name = p.name;
            description = "[process " ^ p.name ^ " starts " ^ p.proc ^ "]";
            line = p.loc.line;
            col = p.loc.col;
          };
          {
            label = Ir.wrapper_discard_label p.name;
            proc_name = p.name;
            description = "[process " ^ p.name ^ " finished]";
            line = p.loc.line;
            col = p.loc.col;
          };
        ])
      ir.processes
  in
  let vars =
    List.map
      (fun (v : Ir.var_info) ->
        {
          tla_name = v.tla_name;
          original = v.original;
          proc = v.proc;
          kind =
            (match v.kind with
            | Ir.Global -> Global
            | Ir.Param -> Param
            | Ir.Local -> Local
            | Ir.CallRet callee -> CallRet callee);
        })
      ir.var_infos
  in
  {
    module_name = ir.name;
    entries = action_entries @ wrapper_entries;
    vars;
  }

(* ===== JSON serialization ===== *)

let entry_to_json (e : entry) : Json.t =
  Json.Object
    [
      ("label", String e.label);
      ("proc", String e.proc_name);
      ("desc", String e.description);
      ("line", Int e.line);
      ("col", Int e.col);
    ]

let var_entry_to_json (v : var_entry) : Json.t =
  Json.Object
    (("tla", Json.String v.tla_name)
     :: ("name", Json.String v.original)
     :: (match v.proc with
        | Some p -> [ ("proc", Json.String p) ]
        | None -> [])
    @ (match v.kind with
      | Global -> [ ("kind", Json.String "global") ]
      | Param -> [ ("kind", Json.String "param") ]
      | Local -> [ ("kind", Json.String "local") ]
      | CallRet callee ->
          [ ("kind", Json.String "callret"); ("callee", Json.String callee) ]))

let to_json (smap : t) : string =
  Json.to_string
    (Json.Object
       [
         ("version", Int 2);
         ("module", String smap.module_name);
         ("labels", Array (List.map entry_to_json smap.entries));
         ("vars", Array (List.map var_entry_to_json smap.vars));
       ])

(* ===== JSON deserialization ===== *)

let entry_of_json (v : Json.t) : entry =
  {
    label = Json.to_string_value (Json.field "label" v);
    proc_name = Json.to_string_value (Json.field "proc" v);
    description = Json.to_string_value (Json.field "desc" v);
    line = Json.to_int (Json.field "line" v);
    col = Json.to_int (Json.field "col" v);
  }

let var_entry_of_json (v : Json.t) : var_entry =
  {
    tla_name = Json.to_string_value (Json.field "tla" v);
    original = Json.to_string_value (Json.field "name" v);
    proc = Option.map Json.to_string_value (Json.field_opt "proc" v);
    kind =
      (match Json.to_string_value (Json.field "kind" v) with
      | "global" -> Global
      | "param" -> Param
      | "local" -> Local
      | "callret" ->
          CallRet (Json.to_string_value (Json.field "callee" v))
      | other -> failwith ("Source_map: unknown var kind " ^ other));
  }

let from_json (s : string) : t =
  match Json.parse s with
  (* v1 format: a bare array of label entries (no module name, no vars).
     Loads fine but disables demangling in the trace printer. *)
  | Json.Array items ->
      { module_name = ""; entries = List.map entry_of_json items; vars = [] }
  | v ->
      {
        module_name = Json.to_string_value (Json.field "module" v);
        entries = List.map entry_of_json (Json.to_array (Json.field "labels" v));
        vars =
          List.map var_entry_of_json (Json.to_array (Json.field "vars" v));
      }
