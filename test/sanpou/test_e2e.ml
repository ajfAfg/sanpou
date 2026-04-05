(* E2E tests: sanpou source → TLA+ compile → TLC model check *)

(* ===== TLC environment ===== *)

let java_path = Sys.getenv_opt "SANPOU_JAVA"
let tla2tools_path = Sys.getenv_opt "SANPOU_TLA2TOOLS_JAR"

let tlc_available =
  match (java_path, tla2tools_path) with Some _, Some _ -> true | _ -> false

(* ===== Compile pipeline ===== *)

let compile_to_tla source =
  let cst =
    source |> Lexing.from_string |> Sanpou.Parser.program Sanpou.Lexer.main
  in
  Sanpou.Typing.check cst;
  let irs =
    cst |> Sanpou.Alpha_convert.transform |> Sanpou.Linearize.linearize
  in
  List.map
    (fun ir ->
      let m = Sanpou.Emit_tla.generate_module ir in
      (m.Tla.Tla_ast.name, Tla.Tla_printer.render m))
    irs

(* ===== TLC runner ===== *)

type tlc_result = Pass | Deadlock | Error of string

let default_cfg = "SPECIFICATION Spec\nCHECK_DEADLOCK TRUE\n"

let read_all_from ic =
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_char buf (input_char ic)
     done
   with End_of_file -> ());
  Buffer.contents buf

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let run_tlc ~dir ~module_name ~cfg =
  let java = Option.get java_path in
  let jar = Option.get tla2tools_path in
  let tla_path = Filename.concat dir (module_name ^ ".tla") in
  let cfg_path = Filename.concat dir (module_name ^ ".cfg") in
  let states_dir = Filename.concat dir "states" in
  if not (Sys.file_exists states_dir) then Sys.mkdir states_dir 0o755;
  write_file cfg_path cfg;
  let cmd =
    Printf.sprintf "%s -cp %s tlc2.TLC -config %s -metadir %s %s 2>&1"
      (Filename.quote java) (Filename.quote jar) (Filename.quote cfg_path)
      (Filename.quote states_dir)
      (Filename.quote tla_path)
  in
  let ic = Unix.open_process_in cmd in
  let output = read_all_from ic in
  let _status = Unix.close_process_in ic in
  let contains pat =
    try
      ignore (Str.search_forward (Str.regexp_string pat) output 0);
      true
    with Not_found -> false
  in
  if contains "Model checking completed. No error has been found." then Pass
  else if contains "Deadlock reached" then Deadlock
  else Error output

let run_e2e ?(cfg = default_cfg) source =
  let modules = compile_to_tla source in
  let dir = Filename.temp_dir "sanpou_e2e" "" in
  List.map
    (fun (module_name, tla_content) ->
      write_file (Filename.concat dir (module_name ^ ".tla")) tla_content;
      (module_name, run_tlc ~dir ~module_name ~cfg))
    modules

(* ===== Alcotest helpers ===== *)

let check_pass msg result =
  match result with
  | Pass -> ()
  | Deadlock -> Alcotest.fail (msg ^ ": expected Pass but got Deadlock")
  | Error s -> Alcotest.fail (msg ^ ": expected Pass but got Error:\n" ^ s)

let check_deadlock msg result =
  match result with
  | Deadlock -> ()
  | Pass -> Alcotest.fail (msg ^ ": expected Deadlock but got Pass")
  | Error s -> Alcotest.fail (msg ^ ": expected Deadlock but got Error:\n" ^ s)

(* ===== Test cases ===== *)

(* --- passes: non-terminating processes with finite state space --- *)

let test_single_process_loop () =
  let results =
    run_e2e
      {|
mod m {
  let x = 0;
  fn f() {
    while (true) {
      x = 1 - x;
    }
  }
  process p = f in 1..1;
}
|}
  in
  check_pass "single process loop" (snd (List.hd results))

let test_multiple_processes_loop () =
  let results =
    run_e2e
      {|
mod m {
  let x = 0;
  let y = 0;
  fn f() {
    while (true) {
      x = 1 - x;
    }
  }
  fn g() {
    while (true) {
      y = 1 - y;
    }
  }
  process p = f in 1..1;
  process q = g in 2..2;
}
|}
  in
  check_pass "multiple processes" (snd (List.hd results))

let test_procedure_call_in_loop () =
  let results =
    run_e2e
      {|
mod m {
  let x = 0;
  fn toggle() { x = 1 - x; return (); }
  fn f() {
    while (true) {
      toggle();
    }
  }
  process p = f in 1..1;
}
|}
  in
  check_pass "procedure call in loop" (snd (List.hd results))

let test_nested_while () =
  let results =
    run_e2e
      {|
mod m {
  let x = 0;
  fn f() {
    while (true) {
      while (x < 3) {
        x = x + 1;
      }
      x = 0;
    }
  }
  process p = f in 1..1;
}
|}
  in
  check_pass "nested while" (snd (List.hd results))

(* --- deadlock: processes that block forever --- *)

let test_mutual_deadlock () =
  let results =
    run_e2e
      {|
mod m {
  let a = false;
  let b = false;
  fn fa() {
    while (true) {
      await b == true,
      a = true;
    }
  }
  fn fb() {
    while (true) {
      await a == true,
      b = true;
    }
  }
  process pa = fa in 1..1;
  process pb = fb in 2..2;
}
|}
  in
  check_deadlock "mutual deadlock" (snd (List.hd results))

let test_single_await_deadlock () =
  let results =
    run_e2e
      {|
mod m {
  let x = 0;
  fn f() {
    while (true) {
      await x == 1,
      x = 0;
    }
  }
  process p = f in 1..1;
}
|}
  in
  check_deadlock "single await deadlock" (snd (List.hd results))

(* --- comparison and logical operators --- *)

let test_lteq_gteq_and () =
  let results =
    run_e2e
      {|
mod m {
  let x = 0;
  fn f() {
    while (true) {
      if (x <= 2 && x >= 0) {
        x = x + 1;
      }
      if (x >= 3) {
        x = 0;
      }
    }
  }
  process p = f in 1..1;
}
|}
  in
  check_pass "lteq gteq and" (snd (List.hd results))

let test_semaphore () =
  let cfg = "SPECIFICATION Spec\nCHECK_DEADLOCK FALSE\n" in
  let results =
    run_e2e ~cfg
      {|
mod semaphore {
  def num = 2;
  def threadsNum = 2;
  let cnt = 0;
  def convergence = globally(0 <= cnt) && globally(finally(cnt <= num));
  fn semaphore() {
    while (true) {
      while (cnt >= num) {}
      cnt = cnt + 1;
      if (cnt <= num) {
        break;
      }
      cnt = cnt - 1;
    }
  }
  process processes = semaphore in 1..threadsNum;
}
|}
  in
  check_pass "semaphore" (snd (List.hd results))

let test_fair_process () =
  let results =
    run_e2e
      {|
mod m {
  let x = 0;
  fn f() {
    while (true) {
      x = 1 - x;
    }
  }
  fair process p = f in 1..1;
}
|}
  in
  check_pass "fair process" (snd (List.hd results))

(* ===== Runner ===== *)

let () =
  if not tlc_available then
    Printf.printf
      "E2E tests skipped: set SANPOU_JAVA and SANPOU_TLA2TOOLS_JAR to enable.\n"
  else
    let open Alcotest in
    run "E2E"
      [
        ( "passes",
          [
            test_case "single process loop" `Slow test_single_process_loop;
            test_case "multiple processes" `Slow test_multiple_processes_loop;
            test_case "procedure call in loop" `Slow test_procedure_call_in_loop;
            test_case "nested while" `Slow test_nested_while;
          ] );
        ( "deadlock",
          [
            test_case "mutual blocking" `Slow test_mutual_deadlock;
            test_case "single await deadlock" `Slow test_single_await_deadlock;
          ] );
        ( "operators",
          [
            test_case "lteq gteq and" `Slow test_lteq_gteq_and;
            test_case "semaphore" `Slow test_semaphore;
            test_case "fair process" `Slow test_fair_process;
          ] );
      ]
