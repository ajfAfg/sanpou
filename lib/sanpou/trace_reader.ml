(* Parse TLC model checker output to extract counterexample traces *)

type state_var = { var_name : string; value : string }

type step_header =
  | InitialPredicate
  (* TLC's action headers name the label but not the acting process;
     [process_id] is only present in the older "<Label(pid) ...>" form. *)
  | Action of { label : string; process_id : int option }

type trace_step = {
  step_num : int;
  header : step_header;
  state : state_var list;
}

type trace = {
  is_deadlock : bool;
  steps : trace_step list;
  module_name : string option;
  errors : string list;
      (* error-severity TLC messages (assert failures, invariant and
         temporal violations, evaluation errors), in output order *)
  stuttering_step : int option;
      (* the step number of a "N: Stuttering" loop marker, if any *)
}

(* ===== Parsing helpers ===== *)

let starts_with prefix s =
  let plen = String.length prefix in
  String.length s >= plen && String.sub s 0 plen = prefix

(* Parse header like "1: <Initial predicate>" or
   "2: <L34 line 16, col 5 to line 18, col 31 of module m>". TLC does not
   say which process moved; the older "2: <L34(1) line ...>" form carries a
   process id and is still accepted. *)
let parse_header line =
  try
    let colon = String.index line ':' in
    let step_num = int_of_string (String.trim (String.sub line 0 colon)) in
    let rest =
      String.trim (String.sub line (colon + 1) (String.length line - colon - 1))
    in
    if rest = "<Initial predicate>" then Some (step_num, InitialPredicate)
    else
      let len = String.length rest in
      if len > 2 && rest.[0] = '<' then
        let inner = String.sub rest 1 (len - 2) in
        (* strip < > *)
        (* The label ends at '(' (process id) or ' ' (location info) *)
        let label_end =
          match (String.index_opt inner '(', String.index_opt inner ' ') with
          | Some p, Some s -> min p s
          | Some p, None -> p
          | None, Some s -> s
          | None, None -> String.length inner
        in
        let label = String.sub inner 0 label_end in
        if label = "" then None
        else
          let process_id =
            if label_end < String.length inner && inner.[label_end] = '(' then
              match String.index_from_opt inner label_end ')' with
              | Some paren_close ->
                  int_of_string_opt
                    (String.sub inner (label_end + 1)
                       (paren_close - label_end - 1))
              | None -> None
            else None
          in
          Some (step_num, Action { label; process_id })
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
  capture : (int * string list) option;
      (* message code and accumulated lines (reversed) of the error/verdict
         message currently being read *)
  errors_rev : string list;
  stuttering_step : int option;
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
    capture = None;
    errors_rev = [];
    stuttering_step = None;
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

(* "@!@!@STARTMSG <code>:<severity> @!@!@" — severity 1 marks errors. *)
let startmsg_re = Str.regexp {|@!@!@STARTMSG \([0-9]+\):\([0-9]+\)|}

let finish_capture st =
  match st.capture with
  | None -> st
  | Some (code, lines_rev) -> (
      let text = String.trim (String.concat "\n" (List.rev lines_rev)) in
      let st = { st with capture = None } in
      match code with
      | 2218 ->
          (* "N: Stuttering" — the loop point of a liveness counterexample *)
          let step =
            match String.index_opt text ':' with
            | Some i -> int_of_string_opt (String.sub text 0 i)
            | None -> None
          in
          { st with stuttering_step = step }
      | 2121 | 2103 | 2264 ->
          (* trace-introduction / evaluation-stack boilerplate: "The
             behavior up to this point is:", "The error occurred when TLC
             was evaluating the nested expressions ...", "The following
             behavior constitutes a counter-example:" *)
          st
      | _ -> { st with errors_rev = text :: st.errors_rev })

let parse_line st line =
  let st =
    match parse_module_name line with
    | Some name -> { st with module_name = Some name } (* keep the last match *)
    | None -> st
  in
  if starts_with "@!@!@STARTMSG 2114" line then { st with is_deadlock = true }
  else if starts_with "@!@!@STARTMSG 2217" line then
    {
      (flush_step (finish_capture st)) with
      in_state_block = true;
      continuation_value = false;
    }
  else if starts_with "@!@!@ENDMSG 2217" line then
    { st with in_state_block = false; continuation_value = false }
  else if Str.string_match startmsg_re line 0 then
    let code = int_of_string (Str.matched_group 1 line) in
    let severity = int_of_string (Str.matched_group 2 line) in
    let st = finish_capture st in
    if severity = 1 || code = 2218 then { st with capture = Some (code, []) }
    else st
  else if starts_with "@!@!@ENDMSG" line then finish_capture st
  else
    match st.capture with
    | Some (code, lines_rev) ->
        { st with capture = Some (code, line :: lines_rev) }
    | None -> if st.in_state_block then parse_state_line st line else st

let parse (input : string) : trace =
  let lines = String.split_on_char '\n' input in
  let st =
    flush_step (finish_capture (List.fold_left parse_line initial_state lines))
  in
  {
    is_deadlock = st.is_deadlock;
    steps = List.rev st.steps_rev;
    module_name = st.module_name;
    errors = List.rev st.errors_rev;
    stuttering_step = st.stuttering_step;
  }
