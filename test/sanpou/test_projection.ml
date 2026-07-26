(* Optimization soundness, checked mechanically: for a battery of small
   models, the reachable-state set projected onto what a behavior can
   observe must be identical with and without the state-space reduction
   passes (action fusion, dead-local canonicalization).

   The projection keeps every module-level variable and reduces [pc] to a
   per-process termination flag; it drops [stack] (frame locals are exactly
   what the passes are allowed to rewrite) and the pc label itself (the
   passes remove interleaving points, so intermediate label values exist
   only in the unoptimized compilation — but since a local action cannot
   change a module variable, dropping labels loses no observable
   configuration). Properties and invariants can only reference
   module-level names, so equality of this projection is precisely the
   soundness claim of the passes.

   Each model is compiled twice ([optimize] on and off), checked by TLC
   with `-dump`, and the projected dumps are compared as sets. Runs only
   when SANPOU_JAVA / SANPOU_TLA2TOOLS_JAR are set (mise provides both);
   otherwise the cases pass with a skip note, like the TLC-backed cram
   tests. *)

module StringSet = Set.Make (String)

let tlc_env () =
  match
    (Sys.getenv_opt "SANPOU_JAVA", Sys.getenv_opt "SANPOU_TLA2TOOLS_JAR")
  with
  | Some java, Some jar when java <> "" && jar <> "" -> Some (java, jar)
  | _ -> None

(* ===== Small file/process helpers ===== *)

let read_all path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let write_all path content =
  let oc = open_out_bin path in
  output_string oc content;
  close_out oc

(* ===== Compile one model, with or without the optimization passes ===== *)

(* Reachability only: no deadlock check (a blocked process must not abort
   the exploration) and no properties. *)
let config =
  Sanpou.Config.from_string
    {|{ "checks": { "deadlock": false, "termination": false } }|}

let compile_to_dir ~optimize ~dir source : string =
  match Sanpou.Compile.compile ~config ~optimize source with
  | Error { loc = { line; col }; message } ->
      Alcotest.failf "compilation failed at %d:%d: %s" line col message
  | Ok [ output ] ->
      let name = output.tla_module.name in
      write_all
        (Filename.concat dir (name ^ ".tla"))
        (Tla.Tla_printer.render output.tla_module);
      let constants =
        List.concat_map
          (function Tla.Tla_ast.DConstants names -> names | _ -> [])
          output.tla_module.body
      in
      write_all
        (Filename.concat dir (name ^ ".cfg"))
        (Sanpou.Config.to_cfg_string ~constants config);
      name
  | Ok _ -> Alcotest.fail "expected exactly one module"

let run_tlc ~java ~jar ~dir name : string =
  let dump = Filename.concat dir (name ^ ".reach.dump") in
  let log = Filename.concat dir (name ^ ".tlc.log") in
  (* A private java.io.tmpdir per run: tla2tools extracts the standard
     modules (TLC.tla, Naturals.tla, ...) into the shared temp directory
     under fixed names, and concurrent TLC processes can race on them
     (see #169). This suite multiplies TLC concurrency, so it opts out. *)
  let tmpdir = Filename.concat dir "jtmp" in
  Sys.mkdir tmpdir 0o755;
  let cmd =
    Printf.sprintf
      "%s -cp %s -XX:+UseParallelGC -Djava.io.tmpdir=%s tlc2.TLC -workers 1 \
       -metadir %s -config %s -dump %s %s > %s 2>&1"
      (Filename.quote java) (Filename.quote jar) (Filename.quote tmpdir)
      (Filename.quote (Filename.concat dir "states"))
      (Filename.quote (Filename.concat dir (name ^ ".cfg")))
      (Filename.quote dump)
      (Filename.quote (Filename.concat dir (name ^ ".tla")))
      (Filename.quote log)
  in
  let code = Sys.command cmd in
  if code <> 0 then
    Alcotest.failf "TLC exited with %d on %s:\n%s" code name (read_all log);
  read_all dump

(* ===== Dump parsing and projection ===== *)

(* A dump is a sequence of blocks:

     State 1:
     /\ stack = ...
     /\ x = 0
     /\ pc = <<"L1", "Done">>

   separated by blank lines. A conjunct is one line; be defensive about
   wrapped values by folding continuation lines into the previous
   conjunct. *)
let parse_dump (content : string) : string list list =
  let flush block blocks =
    match block with [] -> blocks | _ -> List.rev block :: blocks
  in
  let block, blocks =
    List.fold_left
      (fun (block, blocks) line ->
        let trimmed = String.trim line in
        if trimmed = "" then ([], flush block blocks)
        else if String.length trimmed >= 5 && String.sub trimmed 0 5 = "State"
        then (block, blocks)
        else if String.length line >= 3 && String.sub line 0 3 = "/\\ " then
          (String.sub line 3 (String.length line - 3) :: block, blocks)
        else
          match block with
          | last :: rest -> ((last ^ " " ^ trimmed) :: rest, blocks)
          | [] -> (block, blocks))
      ([], [])
      (String.split_on_char '\n' content)
  in
  List.rev (flush block blocks)

let quoted_strings (s : string) : string list =
  let buf = Buffer.create 8 in
  let acc = ref [] in
  let in_quote = ref false in
  String.iter
    (fun c ->
      if c = '"' then begin
        if !in_quote then begin
          acc := Buffer.contents buf :: !acc;
          Buffer.clear buf
        end;
        in_quote := not !in_quote
      end
      else if !in_quote then Buffer.add_char buf c)
    s;
  List.rev !acc

(* One conjunct: "name = value". Drop [stack], reduce [pc] to termination
   flags, keep module variables verbatim. *)
let project_conjunct (conjunct : string) : string option =
  let name =
    match String.index_opt conjunct ' ' with
    | Some i -> String.sub conjunct 0 i
    | None -> conjunct
  in
  if name = "stack" then None
  else if name = "pc" then
    Some
      ("pc_done = ["
      ^ String.concat ";"
          (List.map
             (fun label -> if label = Sanpou.Ir.done_label then "D" else "-")
             (quoted_strings conjunct))
      ^ "]")
  else Some conjunct

let project_dump (content : string) : StringSet.t =
  parse_dump content
  |> List.map (fun conjuncts ->
      conjuncts
      |> List.filter_map project_conjunct
      |> List.sort compare |> String.concat " /\\ ")
  |> StringSet.of_list

(* ===== The check ===== *)

let sample set =
  StringSet.elements set
  |> List.filteri (fun i _ -> i < 3)
  |> String.concat "\n  "

let check_model source () =
  match tlc_env () with
  | None ->
      print_endline
        "skipped: SANPOU_JAVA / SANPOU_TLA2TOOLS_JAR not set (run via mise)"
  | Some (java, jar) ->
      let reachable ~optimize =
        let dir = Filename.temp_dir "sanpou_projection" "" in
        let name = compile_to_dir ~optimize ~dir source in
        project_dump (run_tlc ~java ~jar ~dir name)
      in
      let optimized = reachable ~optimize:true in
      let unoptimized = reachable ~optimize:false in
      if StringSet.is_empty optimized then
        Alcotest.fail "empty reachable set: the dump was not parsed";
      if not (StringSet.equal optimized unoptimized) then
        Alcotest.failf
          "projected reachable sets differ (%d optimized vs %d unoptimized)\n\
           only with optimization:\n\
          \  %s\n\
           only without optimization:\n\
          \  %s"
          (StringSet.cardinal optimized)
          (StringSet.cardinal unoptimized)
          (sample (StringSet.diff optimized unoptimized))
          (sample (StringSet.diff unoptimized optimized))

(* ===== Battery =====

   Small models picked to exercise each rewrite the passes perform:
   local chains and dependent steps, loops with condition actions,
   call/return with used and unused results, recursion, either arms,
   with binders, guards behind local preparation, path writes to local
   maps, and asserts with break/continue/empty steps. *)

let models =
  [
    ( "local chain",
      {|mod chain {
          var x = 0;
          procedure f() {
            var a = 1;
            var b = 0;
            b = a + 1;
            x = x + b;
            return ();
          }
          process p(self in 1..2) = f;
        }|}
    );
    ( "scan loop",
      {|mod scan {
          def n = 2;
          var tickets = { i in 1..n -> 0 };
          var winner = 0;
          procedure f(idx) {
            tickets[idx] = idx;
            var max = 0;
            var i = 1;
            while (i <= n) {
              var t = tickets[i];
              if (max < t) {
                max = t;
              }
              i = i + 1;
            }
            winner = max;
            return ();
          }
          procedure g() {
            f(self);
            return ();
          }
          process p(self in 1..n) = g;
        }|}
    );
    ( "call results",
      {|mod calls {
          var x = 0;
          procedure inc() {
            x = x + 1;
            return x;
          }
          procedure f() {
            var used = inc();
            var unused = inc();
            x = x + used;
            return ();
          }
          process p(self in 1..2) = f;
        }|}
    );
    ( "recursion",
      {|mod recur {
          var out = 0;
          procedure fact(n) {
            if (n == 0) {
              return 1;
            } else {
              var r = n * fact(n - 1);
              return r;
            }
          }
          procedure f() {
            out = fact(3);
            return ();
          }
          process p(self in 1..2) = f;
        }|}
    );
    ( "either arms",
      {|mod choice {
          var x = 0;
          procedure f() {
            var d = 0;
            either {
              await x % 2 == 0,
              d = 1;
            } or {
              d = 2;
            }
            x = x + d;
            return ();
          }
          process p(self in 1..2) = f;
        }|}
    );
    ( "with binder",
      {|mod withm {
          var x = 0;
          procedure f() {
            var v = 0;
            with (w in 1..2) {
              v = w;
            }
            x = x + v;
            return ();
          }
          process p(self in 1..2) = f;
        }|}
    );
    ( "guard behind locals",
      {|mod guard {
          var flag = false;
          var x = 0;
          procedure setter() {
            flag = true;
            return ();
          }
          procedure waiter() {
            var t = 5;
            await flag,
            x = t;
            return ();
          }
          process s(self in {1}) = setter;
          process w(self in {2}) = waiter;
        }|}
    );
    ( "local path writes",
      {|mod pathw {
          def n = 2;
          var sum = 0;
          procedure f() {
            var m = { i in 1..n -> 0 };
            m[1] = 3;
            m[2] = m[1] + 1;
            sum = sum + m[1] + m[2];
            return ();
          }
          process p(self in 1..2) = f;
        }|}
    );
    ( "assert and jumps",
      {|mod misc {
          var x = 0;
          procedure f() {
            var i = 0;
            while (true) {
              i = i + 1;
              if (i > 2) {
                break;
              }
              ;
              continue;
            }
            assert i == 3;
            x = x + i;
            return ();
          }
          process p(self in 1..2) = f;
        }|}
    );
  ]

let () =
  Alcotest.run "projection"
    [
      ( "optimized vs unoptimized reachability",
        List.map
          (fun (name, source) ->
            Alcotest.test_case name `Slow (check_model source))
          models );
    ]
