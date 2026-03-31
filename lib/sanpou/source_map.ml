type entry = {
  label : string;
  proc_name : string;
  description : string;
  line : int;
  col : int;
}

let extract (ir : Compile.module_ir) : entry list =
  List.concat_map
    (fun (proc : Compile.proc_ir) ->
      List.map
        (fun (action : Compile.action) ->
          {
            label = action.label;
            proc_name = action.source.proc_name;
            description = action.source.description;
            line = action.source.line;
            col = action.source.col;
          })
        proc.actions)
    ir.procs

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

let to_json (entries : entry list) : string =
  Json.to_string (Json.Array (List.map entry_to_json entries))

(* ===== JSON deserialization ===== *)

let entry_of_json (v : Json.t) : entry =
  {
    label = Json.to_string_value (Json.field "label" v);
    proc_name = Json.to_string_value (Json.field "proc" v);
    description = Json.to_string_value (Json.field "desc" v);
    line = Json.to_int (Json.field "line" v);
    col = Json.to_int (Json.field "col" v);
  }

let from_json (s : string) : entry list =
  let v = Json.parse s in
  List.map entry_of_json (Json.to_array v)
