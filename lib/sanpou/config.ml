type checks = { deadlock : bool; termination : bool }

type t = {
  checks : checks;
  properties : string list;
  invariants : string list;
}

let default =
  {
    checks = { deadlock = true; termination = false };
    properties = [];
    invariants = [];
  }

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
  let invariants =
    string_list_field_with_default "invariants" default.invariants json
  in
  { checks; properties; invariants }

let from_string s = Json.parse s |> of_json

let cfg_properties config =
  let properties = config.properties in
  if config.checks.termination && not (List.mem "Termination" properties) then
    properties @ [ "Termination" ]
  else properties

let bool_to_upper_string b = if b then "TRUE" else "FALSE"

(* [default_init_value]: when the module declares CONSTANT defaultInitValue
   (the null sentinel for unassigned frame fields), the cfg must assign it.
   Assigning the constant to its own name makes TLC create a model value,
   which compares unequal to every user value without a type error. *)
let to_cfg_string ?(default_init_value = false) config =
  let constant_lines =
    if default_init_value then
      [ ""; "CONSTANT defaultInitValue = defaultInitValue" ]
    else []
  in
  let invariant_lines =
    match config.invariants with
    | [] -> []
    | invariants ->
        "" :: "INVARIANTS"
        :: List.map (fun invariant -> "    " ^ invariant) invariants
  in
  let property_lines =
    match cfg_properties config with
    | [] -> []
    | properties ->
        "" :: "PROPERTIES"
        :: List.map (fun property -> "    " ^ property) properties
  in
  let lines =
    [ "SPECIFICATION Spec" ] @ constant_lines
    @ [ ""; "CHECK_DEADLOCK " ^ bool_to_upper_string config.checks.deadlock ]
    @ invariant_lines @ property_lines
  in
  String.concat "\n" lines ^ "\n"
