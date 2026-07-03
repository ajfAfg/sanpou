(* Pretty-print TLC trace annotated with sanpou source info *)

open Trace_reader

let find_source_info (smap : Source_map.t) (label : string) =
  List.find_opt (fun (e : Source_map.entry) -> e.label = label) smap.entries

let find_var_entry (smap : Source_map.t) (name : string) =
  List.find_opt (fun (v : Source_map.var_entry) -> v.tla_name = name) smap.vars

(* Variables to hide from user-facing trace display: TLA bookkeeping plus
   compiler-generated call-return temporaries. The returned value of a call
   is visible on the next assignment, and frame save/restore would otherwise
   show noisy resets of callRet variables. *)
let is_internal_var (smap : Source_map.t) name =
  name = "pc" || name = "stack"
  ||
  match find_var_entry smap name with
  | Some { kind = CallRet _; _ } -> true
  | _ -> false

let filter_user_vars (smap : Source_map.t) (vars : state_var list) :
    state_var list =
  List.filter (fun v -> not (is_internal_var smap v.var_name)) vars

(* Compute changed variables between two states (on raw TLC values) *)
let changed_vars (smap : Source_map.t) (prev : state_var list)
    (curr : state_var list) : state_var list =
  List.filter
    (fun v ->
      match List.find_opt (fun p -> p.var_name = v.var_name) prev with
      | None -> true
      | Some p -> p.value <> v.value)
    (filter_user_vars smap curr)

(* ===== Display names ===== *)

(* Source name to show for a TLA variable. When distinct variables share a
   source name (e.g. a module-level `x` and a procedure parameter `x`),
   module-level ones stay bare and procedure-local ones are qualified as
   "x (proc)". *)
let display_var_name (smap : Source_map.t) (tla_name : string) : string =
  match find_var_entry smap tla_name with
  | None -> tla_name
  | Some entry -> (
      let same_original =
        List.filter
          (fun (v : Source_map.var_entry) -> v.original = entry.original)
          smap.vars
      in
      match (List.length same_original <= 1, entry.proc) with
      | true, _ | _, None -> entry.original
      | false, Some proc -> entry.original ^ " (" ^ proc ^ ")")

(* ===== TLC value parsing ===== *)

(* Split [inner] on top-level commas, respecting << >>, [ ], ( ), { } nesting
   and quoted strings. Returns None on malformed input. *)
let split_top_level (inner : string) : string list option =
  let n = String.length inner in
  let buf = Buffer.create 16 in
  (* [acc] holds the completed elements in reverse *)
  let rec go acc ~depth ~in_string i =
    if i >= n then
      if in_string || depth <> 0 then None
      else
        let last = String.trim (Buffer.contents buf) in
        let elems = if last = "" && acc = [] then [] else last :: acc in
        Some (List.rev elems)
    else
      let c = inner.[i] in
      if in_string then (
        Buffer.add_char buf c;
        go acc ~depth ~in_string:(c <> '"') (i + 1))
      else if c = '"' then (
        Buffer.add_char buf c;
        go acc ~depth ~in_string:true (i + 1))
      else if c = '<' && i + 1 < n && inner.[i + 1] = '<' then (
        Buffer.add_string buf "<<";
        go acc ~depth:(depth + 1) ~in_string (i + 2))
      else if c = '>' && i + 1 < n && inner.[i + 1] = '>' then (
        Buffer.add_string buf ">>";
        if depth = 0 then None else go acc ~depth:(depth - 1) ~in_string (i + 2))
      else if c = '[' || c = '(' || c = '{' then (
        Buffer.add_char buf c;
        go acc ~depth:(depth + 1) ~in_string (i + 1))
      else if c = ']' || c = ')' || c = '}' then (
        Buffer.add_char buf c;
        if depth = 0 then None else go acc ~depth:(depth - 1) ~in_string (i + 1))
      else if c = ',' && depth = 0 then (
        let elem = String.trim (Buffer.contents buf) in
        Buffer.clear buf;
        go (elem :: acc) ~depth ~in_string (i + 1))
      else (
        Buffer.add_char buf c;
        go acc ~depth ~in_string (i + 1))
  in
  go [] ~depth:0 ~in_string:false 0

(* Split the top-level elements of a TLC tuple value "<<a, b, c>>".
   Returns None when the string is not in tuple form (e.g. a function over a
   non-1..n domain, printed as "(5 :> 0 @@ 6 :> 0)"). *)
let split_tuple_elements (s : string) : string list option =
  let s = String.trim s in
  let len = String.length s in
  if len < 4 || String.sub s 0 2 <> "<<" || String.sub s (len - 2) 2 <> ">>"
  then None
  else split_top_level (String.sub s 2 (len - 4))

(* Parse a TLC record value "[k1 |-> v1, k2 |-> v2, ...]" into field pairs.
   TLC reorders fields relative to the source, so callers must look fields up
   by name, never by position. *)
let split_record_fields (s : string) : (string * string) list option =
  let s = String.trim s in
  let len = String.length s in
  if len < 2 || s.[0] <> '[' || s.[len - 1] <> ']' then None
  else
    match split_top_level (String.sub s 1 (len - 2)) with
    | None -> None
    | Some parts ->
        let parse_field part =
          let re = Str.regexp_string "|->" in
          match Str.bounded_split_delim re part 2 with
          | [ name; value ] -> Some (String.trim name, String.trim value)
          | _ -> None
        in
        let fields = List.map parse_field parts in
        if List.exists Option.is_none fields then None
        else Some (List.map Option.get fields)

(* ===== Rendering ===== *)

let render_header (header : step_header) (smap : Source_map.t) : string =
  match header with
  | InitialPredicate -> "Initial state"
  | Action { label; process_id } -> (
      match find_source_info smap label with
      | Some entry ->
          Printf.sprintf "%s (process %d): %s  [line %d]" entry.proc_name
            process_id entry.description entry.line
      | None -> Printf.sprintf "%s(process %d)" label process_id)

(* ===== Frame-resident locals ===== *)

(* Frame bookkeeping fields and callRet temporaries are not user variables *)
let is_hidden_field (smap : Source_map.t) name =
  name = "procedure" || name = "return_pc" || name = "value"
  ||
  match find_var_entry smap name with
  | Some { kind = CallRet _; _ } -> true
  | _ -> false

(* Unassigned frame fields hold the compiler's null sentinel (a TLA string,
   which user values can never be); show it as "null" *)
let display_field_value v = if v = "\"__null__\"" then "null" else v

(* The given process's stack frames, top first. None when the stack value
   cannot be parsed (e.g. non-1..n ProcSet). *)
let frames_of (stack_value : string) (process_id : int) : string list option =
  match split_tuple_elements stack_value with
  | None -> None
  | Some per_process -> (
      match List.nth_opt per_process (process_id - 1) with
      | None -> None
      | Some process_stack -> split_tuple_elements process_stack)

let find_stack_var (state : state_var list) =
  List.find_opt (fun v -> v.var_name = "stack") state

(* Lines describing the acting process's locals, read from the top stack
   frame. Same stack depth as the previous state -> show changed fields only
   (a plain assignment). Depth changed (call/return) or previous state
   unavailable -> the top frame is a different frame, so show its contents as
   context on one line rather than as spurious "changes". *)
let render_local_lines (smap : Source_map.t) ~prev_state
    (state : state_var list) (process_id : int) : string list =
  let frames_in st =
    Option.bind (find_stack_var st) (fun v -> frames_of v.value process_id)
  in
  match frames_in state with
  | None | Some [] -> []
  | Some (top :: _ as curr_frames) -> (
      match split_record_fields top with
      | None -> []
      | Some fields -> (
          let visible =
            List.filter (fun (k, _) -> not (is_hidden_field smap k)) fields
          in
          let context_line () =
            if visible = [] then []
            else
              [
                "  (frame: "
                ^ String.concat ", "
                    (List.map
                       (fun (k, v) ->
                         Printf.sprintf "%s = %s" (display_var_name smap k)
                           (display_field_value v))
                       visible)
                ^ ")";
              ]
          in
          match frames_in prev_state with
          | Some (prev_top :: _ as prev_frames)
            when List.length prev_frames = List.length curr_frames -> (
              match split_record_fields prev_top with
              | Some prev_fields ->
                  List.filter_map
                    (fun (k, v) ->
                      match List.assoc_opt k prev_fields with
                      | Some prev_v when prev_v = v -> None
                      | _ ->
                          Some
                            (Printf.sprintf "  %s = %s"
                               (display_var_name smap k) (display_field_value v)))
                    visible
              | None -> context_line ())
          | _ -> context_line ()))

(* ===== Step rendering ===== *)

let render_var_lines (smap : Source_map.t) (vars : state_var list) : string list
    =
  List.map
    (fun v ->
      Printf.sprintf "  %s = %s" (display_var_name smap v.var_name) v.value)
    vars

let render_step ~prev_state (step : trace_step) (smap : Source_map.t) : string =
  let header_str = render_header step.header smap in
  let global_lines =
    match step.header with
    | InitialPredicate ->
        render_var_lines smap (filter_user_vars smap step.state)
    | Action _ ->
        render_var_lines smap (changed_vars smap prev_state step.state)
  in
  let local_lines =
    match step.header with
    | InitialPredicate -> [] (* stacks are empty: no locals exist yet *)
    | Action { process_id; _ } ->
        render_local_lines smap ~prev_state step.state process_id
  in
  let lines =
    match global_lines @ local_lines with
    | [] -> [ "  (no changes)" ]
    | ls -> ls
  in
  Printf.sprintf "Step %d: %s\n%s" step.step_num header_str
    (String.concat "\n" lines)

let render_deadlock_summary (trace : trace) (smap : Source_map.t) : string =
  match List.rev trace.steps with
  | [] -> ""
  | last_step :: _ -> (
      (* Find pc values from the last state *)
      let pc_var = List.find_opt (fun v -> v.var_name = "pc") last_step.state in
      match pc_var with
      | None -> "Deadlock: unable to determine process states"
      | Some pc ->
          (* pc value looks like <<"L25", "__w_workers_entry__">>;
             parse out individual labels *)
          let pc_str = pc.value in
          let re = Str.regexp {|"[A-Za-z_][A-Za-z0-9_]*"|} in
          let rec collect_labels acc pos =
            match Str.search_forward re pc_str pos with
            | _ ->
                let m = Str.matched_string pc_str in
                let label = String.sub m 1 (String.length m - 2) in
                collect_labels (label :: acc) (Str.match_end ())
            | exception Not_found -> List.rev acc
          in
          let labels = collect_labels [] 0 in
          let process_line i label =
            let pid = i + 1 in
            if label = "Done" then Printf.sprintf "  process %d: finished\n" pid
            else
              match find_source_info smap label with
              | Some entry ->
                  Printf.sprintf "  process %d (%s): %s  [line %d]\n" pid
                    entry.proc_name entry.description entry.line
              | None -> Printf.sprintf "  process %d: at %s\n" pid label
          in
          "DEADLOCK — all processes blocked:\n"
          ^ String.concat "" (List.mapi process_line labels))

let render (trace : trace) (smap : Source_map.t) : string =
  let rec render_steps prev_state = function
    | [] -> []
    | step :: rest ->
        (render_step ~prev_state step smap ^ "\n\n")
        :: render_steps step.state rest
  in
  String.concat "" (render_steps [] trace.steps)
  ^ if trace.is_deadlock then render_deadlock_summary trace smap else ""
