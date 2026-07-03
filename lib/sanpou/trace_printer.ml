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

(* ===== Per-process value unwrapping ===== *)

(* Split the top-level elements of a TLC tuple value "<<a, b, c>>".
   Returns None when the string is not in tuple form (e.g. a function over a
   non-1..n domain, printed as "(5 :> 0 @@ 6 :> 0)"). *)
let split_tuple_elements (s : string) : string list option =
  let s = String.trim s in
  let len = String.length s in
  if len < 4 || String.sub s 0 2 <> "<<" || String.sub s (len - 2) 2 <> ">>"
  then None
  else
    let inner = String.sub s 2 (len - 4) in
    let n = String.length inner in
    let elems = ref [] in
    let buf = Buffer.create 16 in
    let depth = ref 0 in
    let in_string = ref false in
    let error = ref false in
    let i = ref 0 in
    while !i < n && not !error do
      let c = inner.[!i] in
      if !in_string then (
        Buffer.add_char buf c;
        if c = '"' then in_string := false;
        incr i)
      else if c = '"' then (
        Buffer.add_char buf c;
        in_string := true;
        incr i)
      else if c = '<' && !i + 1 < n && inner.[!i + 1] = '<' then (
        Buffer.add_string buf "<<";
        incr depth;
        i := !i + 2)
      else if c = '>' && !i + 1 < n && inner.[!i + 1] = '>' then (
        Buffer.add_string buf ">>";
        decr depth;
        if !depth < 0 then error := true;
        i := !i + 2)
      else if c = '[' || c = '(' || c = '{' then (
        Buffer.add_char buf c;
        incr depth;
        incr i)
      else if c = ']' || c = ')' || c = '}' then (
        Buffer.add_char buf c;
        decr depth;
        if !depth < 0 then error := true;
        incr i)
      else if c = ',' && !depth = 0 then (
        elems := String.trim (Buffer.contents buf) :: !elems;
        Buffer.clear buf;
        incr i)
      else (
        Buffer.add_char buf c;
        incr i)
    done;
    if !error || !in_string || !depth <> 0 then None
    else
      let last = String.trim (Buffer.contents buf) in
      let elems = if last = "" && !elems = [] then [] else last :: !elems in
      Some (List.rev elems)

let rec all_equal = function
  | [] | [ _ ] -> true
  | x :: (y :: _ as rest) -> x = y && all_equal rest

(* Procedure-local variables are functions over ProcSet, printed by TLC as
   "<<v1, ..., vn>>" when ProcSet = 1..n. Show only the relevant process's
   value; on any mismatch fall back to the raw string. *)
let display_value (smap : Source_map.t) (header : step_header)
    (v : state_var) : string =
  let is_per_process =
    match find_var_entry smap v.var_name with
    | Some { proc = Some _; _ } -> true
    | _ -> false
  in
  if not is_per_process then v.value
  else
    match split_tuple_elements v.value with
    | None -> v.value
    | Some elems -> (
        match header with
        | Action { process_id; _ } -> (
            match List.nth_opt elems (process_id - 1) with
            | Some elem -> elem
            | None -> v.value)
        | InitialPredicate -> (
            match elems with
            | first :: _ when all_equal elems -> first
            | _ -> v.value))

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

let render_vars (smap : Source_map.t) (header : step_header)
    (vars : state_var list) : string =
  if vars = [] then "  (no changes)"
  else
    String.concat "\n"
      (List.map
         (fun v ->
           Printf.sprintf "  %s = %s"
             (display_var_name smap v.var_name)
             (display_value smap header v))
         vars)

let render_step ~prev_state (step : trace_step) (smap : Source_map.t) : string
    =
  let header_str = render_header step.header smap in
  let vars =
    match step.header with
    | InitialPredicate -> filter_user_vars smap step.state
    | Action _ -> changed_vars smap prev_state step.state
  in
  Printf.sprintf "Step %d: %s\n%s" step.step_num header_str
    (render_vars smap step.header vars)

let render_deadlock_summary (trace : trace) (smap : Source_map.t) : string =
  match List.rev trace.steps with
  | [] -> ""
  | last_step :: _ -> (
      (* Find pc values from the last state *)
      let pc_var = List.find_opt (fun v -> v.var_name = "pc") last_step.state in
      match pc_var with
      | None -> "Deadlock: unable to determine process states"
      | Some pc ->
          let buf = Buffer.create 128 in
          Buffer.add_string buf "DEADLOCK — all processes blocked:\n";
          (* pc value looks like <<"L25", "__w_workers_entry__">> *)
          (* Parse out individual labels *)
          let pc_str = pc.value in
          let labels = ref [] in
          let re = Str.regexp {|"[A-Za-z_][A-Za-z0-9_]*"|} in
          let pos = ref 0 in
          (try
             while true do
               let _ = Str.search_forward re pc_str !pos in
               let m = Str.matched_string pc_str in
               let label = String.sub m 1 (String.length m - 2) in
               labels := !labels @ [ label ];
               pos := Str.match_end ()
             done
           with Not_found -> ());
          List.iteri
            (fun i label ->
              let pid = i + 1 in
              if label = "Done" then
                Buffer.add_string buf
                  (Printf.sprintf "  process %d: finished\n" pid)
              else
                match find_source_info smap label with
                | Some entry ->
                    Buffer.add_string buf
                      (Printf.sprintf "  process %d (%s): %s  [line %d]\n" pid
                         entry.proc_name entry.description entry.line)
                | None ->
                    Buffer.add_string buf
                      (Printf.sprintf "  process %d: at %s\n" pid label))
            !labels;
          Buffer.contents buf)

let render (trace : trace) (smap : Source_map.t) : string =
  let buf = Buffer.create 512 in
  let prev_state = ref [] in
  List.iter
    (fun step ->
      Buffer.add_string buf (render_step ~prev_state:!prev_state step smap);
      Buffer.add_char buf '\n';
      Buffer.add_char buf '\n';
      prev_state := step.state)
    trace.steps;
  if trace.is_deadlock then
    Buffer.add_string buf (render_deadlock_summary trace smap);
  Buffer.contents buf
