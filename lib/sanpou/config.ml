type checks = { deadlock : bool; termination : bool }
type t = { checks : checks; properties : string list }

let default =
  { checks = { deadlock = true; termination = false }; properties = [] }

let bool_field_with_default key default_value json =
  match Json.field_opt key json with
  | None -> default_value
  | Some value -> Json.to_bool value

let string_list_field_with_default key default_value json =
  match Json.field_opt key json with
  | None -> default_value
  | Some value -> List.map Json.to_string_value (Json.to_array value)

let of_json json =
  let checks_json =
    match Json.field_opt "checks" json with
    | None -> Json.Object []
    | Some value -> value
  in
  let checks =
    {
      deadlock =
        bool_field_with_default "deadlock" default.checks.deadlock checks_json;
      termination =
        bool_field_with_default "termination" default.checks.termination
          checks_json;
    }
  in
  let properties =
    string_list_field_with_default "properties" default.properties json
  in
  { checks; properties }

let from_string s = Json.parse s |> of_json

let cfg_properties config =
  let properties = config.properties in
  if config.checks.termination && not (List.mem "Termination" properties) then
    properties @ [ "Termination" ]
  else properties

let bool_to_upper_string b = if b then "TRUE" else "FALSE"

let to_cfg_string config =
  let lines =
    ref
      [
        "SPECIFICATION Spec";
        "";
        "CHECK_DEADLOCK " ^ bool_to_upper_string config.checks.deadlock;
      ]
  in
  let properties = cfg_properties config in
  if properties <> [] then (
    lines := !lines @ [ ""; "PROPERTIES" ];
    List.iter
      (fun property -> lines := !lines @ [ "    " ^ property ])
      properties);
  String.concat "\n" !lines ^ "\n"
