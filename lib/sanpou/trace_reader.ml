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

type trace = { is_deadlock : bool; steps : trace_step list }

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

let parse (input : string) : trace =
  let lines = String.split_on_char '\n' input in
  let is_deadlock = ref false in
  let steps = ref [] in
  let current_step = ref None in
  let current_vars = ref [] in
  let in_state_block = ref false in
  let continuation_value = ref false in
  let flush_step () =
    match !current_step with
    | Some (step_num, header) ->
        steps :=
          !steps @ [ { step_num; header; state = List.rev !current_vars } ];
        current_step := None;
        current_vars := []
    | None -> ()
  in
  List.iter
    (fun line ->
      if starts_with "@!@!@STARTMSG 2114" line then is_deadlock := true
      else if starts_with "@!@!@STARTMSG 2217" line then (
        flush_step ();
        in_state_block := true;
        continuation_value := false)
      else if starts_with "@!@!@ENDMSG 2217" line then (
        in_state_block := false;
        continuation_value := false)
      else if !in_state_block then
        if !current_step = None then
          (* First line in block is the header *)
          match parse_header line with
          | Some (step_num, header) -> current_step := Some (step_num, header)
          | None -> ()
        else if !continuation_value then (
          (* This line continues a multi-line value *)
          match !current_vars with
          | last :: rest ->
              let extended_value = last.value ^ " " ^ String.trim line in
              current_vars := { last with value = extended_value } :: rest
          | [] ->
              ();
              (* Check if this line still has a continuation (no /\ prefix on next) *)
              continuation_value := not (String.length (String.trim line) = 0))
        else
          match parse_state_var line with
          | Some var ->
              current_vars := var :: !current_vars;
              (* Heuristic: if value ends with >> that's complete;
                 if it has unbalanced << it continues *)
              continuation_value := false
          | None -> (
              if
                (* Could be a continuation of previous value *)
                String.trim line <> "" && !current_vars <> []
              then
                match !current_vars with
                | last :: rest ->
                    let extended_value = last.value ^ " " ^ String.trim line in
                    current_vars := { last with value = extended_value } :: rest
                | [] -> ()))
    lines;
  flush_step ();
  { is_deadlock = !is_deadlock; steps = !steps }
