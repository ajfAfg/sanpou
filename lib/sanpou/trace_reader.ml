(* Parse TLC model checker output to extract counterexample traces *)

type state_var = { var_name : string; value : string }

type step_header =
  | InitialPredicate
  | Action of { label : string; process_id : int }

type trace_step = {
  step_num : int;
  header : step_header;
  state : state_var list;
}

type trace = {
  is_deadlock : bool;
  steps : trace_step list;
  module_name : string option;
}

(* ===== Parsing helpers ===== *)

let starts_with prefix s =
  let plen = String.length prefix in
  String.length s >= plen && String.sub s 0 plen = prefix

(* Parse header like "1: <Initial predicate>" or "2: <L34(1) line ...>" *)
let parse_header line =
  try
    let colon = String.index line ':' in
    let step_num = int_of_string (String.trim (String.sub line 0 colon)) in
    let rest =
      String.trim (String.sub line (colon + 1) (String.length line - colon - 1))
    in
    if rest = "<Initial predicate>" then Some (step_num, InitialPredicate)
    else
      (* Expected: <Label(ProcessId) line ... > *)
      let len = String.length rest in
      if len > 2 && rest.[0] = '<' then
        let inner = String.sub rest 1 (len - 2) in
        (* strip < > *)
        (* Find label and process_id: "L34(1) line ..." *)
        let paren_open = try String.index inner '(' with Not_found -> -1 in
        if paren_open > 0 then
          let label = String.sub inner 0 paren_open in
          let paren_close = try String.index inner ')' with Not_found -> -1 in
          if paren_close > paren_open then
            let pid_s =
              String.sub inner (paren_open + 1) (paren_close - paren_open - 1)
            in
            let process_id = try int_of_string pid_s with Failure _ -> 0 in
            Some (step_num, Action { label; process_id })
          else None
        else None
      else None
  with _ -> None

(* Parse state variable line: "/\ var = value" *)
let parse_state_var line =
  let trimmed = String.trim line in
  if starts_with "/\\ " trimmed then
    let rest = String.sub trimmed 3 (String.length trimmed - 3) in
    (* Find first " = " *)
    let eq_re = Str.regexp " = " in
    try
      let pos = Str.search_forward eq_re rest 0 in
      let var_name = String.sub rest 0 pos in
      let value = String.sub rest (pos + 3) (String.length rest - pos - 3) in
      Some { var_name = String.trim var_name; value = String.trim value }
    with Not_found -> None
  else None

(* ===== Main parser ===== *)

(* SANY logs "Semantic processing of module <M>" for each module, dependencies
   first and the checked module last. Internal modules (e.g. _TLCTrace) start
   with '_' and are skipped. *)
let semantic_processing_prefix = "Semantic processing of module "

let parse_module_name line =
  let trimmed = String.trim line in
  if starts_with semantic_processing_prefix trimmed then
    let plen = String.length semantic_processing_prefix in
    let name = String.sub trimmed plen (String.length trimmed - plen) in
    let name = String.trim name in
    if name <> "" && name.[0] <> '_' then Some name else None
  else None

(* Folded over the input lines; [steps_rev] and [current_vars] accumulate in
   reverse. *)
type parse_state = {
  is_deadlock : bool;
  module_name : string option;
  steps_rev : trace_step list;
  current_step : (int * step_header) option;
  current_vars : state_var list;
  in_state_block : bool;
  continuation_value : bool;
}

let initial_state =
  {
    is_deadlock = false;
    module_name = None;
    steps_rev = [];
    current_step = None;
    current_vars = [];
    in_state_block = false;
    continuation_value = false;
  }

let flush_step st =
  match st.current_step with
  | Some (step_num, header) ->
      {
        st with
        steps_rev =
          { step_num; header; state = List.rev st.current_vars } :: st.steps_rev;
        current_step = None;
        current_vars = [];
      }
  | None -> st

let extend_last_value line st =
  match st.current_vars with
  | last :: rest ->
      let extended_value = last.value ^ " " ^ String.trim line in
      { st with current_vars = { last with value = extended_value } :: rest }
  | [] -> st

let parse_state_line st line =
  match st.current_step with
  | None -> (
      (* First line in block is the header *)
      match parse_header line with
      | Some (step_num, header) ->
          { st with current_step = Some (step_num, header) }
      | None -> st)
  | Some _ -> (
      if st.continuation_value then
        (* This line continues a multi-line value *)
        match st.current_vars with
        | _ :: _ -> extend_last_value line st
        | [] -> { st with continuation_value = String.trim line <> "" }
      else
        match parse_state_var line with
        | Some var ->
            {
              st with
              current_vars = var :: st.current_vars;
              continuation_value = false;
            }
        | None ->
            (* Could be a continuation of the previous value *)
            if String.trim line <> "" then extend_last_value line st else st)

let parse_line st line =
  let st =
    match parse_module_name line with
    | Some name -> { st with module_name = Some name } (* keep the last match *)
    | None -> st
  in
  if starts_with "@!@!@STARTMSG 2114" line then { st with is_deadlock = true }
  else if starts_with "@!@!@STARTMSG 2217" line then
    { (flush_step st) with in_state_block = true; continuation_value = false }
  else if starts_with "@!@!@ENDMSG 2217" line then
    { st with in_state_block = false; continuation_value = false }
  else if st.in_state_block then parse_state_line st line
  else st

let parse (input : string) : trace =
  let lines = String.split_on_char '\n' input in
  let st = flush_step (List.fold_left parse_line initial_state lines) in
  {
    is_deadlock = st.is_deadlock;
    steps = List.rev st.steps_rev;
    module_name = st.module_name;
  }
