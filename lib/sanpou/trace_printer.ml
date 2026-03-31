(* Pretty-print TLC trace annotated with sanpou source info *)

open Trace_reader

let find_source_info (smap : Source_map.entry list) (label : string) =
  List.find_opt (fun (e : Source_map.entry) -> e.label = label) smap

(* Variables to hide from user-facing trace display *)
let is_internal_var name = name = "pc" || name = "stack"

let filter_user_vars (vars : state_var list) : state_var list =
  List.filter (fun v -> not (is_internal_var v.var_name)) vars

(* Compute changed variables between two states *)
let changed_vars (prev : state_var list) (curr : state_var list) :
    state_var list =
  List.filter
    (fun v ->
      match List.find_opt (fun p -> p.var_name = v.var_name) prev with
      | None -> true
      | Some p -> p.value <> v.value)
    (filter_user_vars curr)

(* ===== Rendering ===== *)

let render_header (header : step_header) (smap : Source_map.entry list) : string
    =
  match header with
  | InitialPredicate -> "Initial state"
  | Action { label; process_id } -> (
      match find_source_info smap label with
      | Some entry ->
          Printf.sprintf "%s (process %d): %s  [line %d]" entry.proc_name
            process_id entry.description entry.line
      | None -> Printf.sprintf "%s(process %d)" label process_id)

let render_vars (vars : state_var list) : string =
  if vars = [] then "  (no changes)"
  else
    String.concat "\n"
      (List.map (fun v -> Printf.sprintf "  %s = %s" v.var_name v.value) vars)

let render_step ~prev_state (step : trace_step) (smap : Source_map.entry list) :
    string =
  let header_str = render_header step.header smap in
  let vars =
    match step.header with
    | InitialPredicate -> filter_user_vars step.state
    | Action _ -> changed_vars prev_state step.state
  in
  Printf.sprintf "Step %d: %s\n%s" step.step_num header_str (render_vars vars)

let render_deadlock_summary (trace : trace) (smap : Source_map.entry list) :
    string =
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
          (* pc value looks like <<"L25", "L25">> *)
          (* Parse out individual labels *)
          let pc_str = pc.value in
          let labels = ref [] in
          let re = Str.regexp {|"L[0-9]+"|} in
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

let render (trace : trace) (smap : Source_map.entry list) : string =
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
