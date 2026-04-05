let read_all path =
  let ic = open_in path in
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_char buf (input_char ic)
     done
   with End_of_file -> ());
  close_in ic;
  Buffer.contents buf

let write_all path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let sidecar_config_path file =
  if Filename.check_suffix file ".snp" then
    Filename.chop_suffix file ".snp" ^ ".json"
  else file ^ ".json"

let load_compile_config file =
  let config_path = sidecar_config_path file in
  if Sys.file_exists config_path then (
    try Some (Sanpou.Config.from_string (read_all config_path))
    with Failure msg ->
      Printf.eprintf "Error: invalid sanpou config '%s': %s\n" config_path msg;
      exit 1)
  else None

let cmd_compile file outdir =
  let input = read_all file in
  let lexbuf = Lexing.from_string input in
  let config_opt = load_compile_config file in
  let config = Option.value ~default:Sanpou.Config.default config_opt in
  let cst = Sanpou.Parser.program Sanpou.Lexer.main lexbuf in
  Sanpou.Typing.check cst;
  let irs = Sanpou.Alpha_convert.transform cst |> Sanpou.Linearize.linearize in
  if not (Sys.file_exists outdir) then Sys.mkdir outdir 0o755;
  List.iter
    (fun (ir : Sanpou.Ir.module_ir) ->
      let tla_module = Sanpou.Emit_tla.generate_module ~config ir in
      let tla_path = Filename.concat outdir (tla_module.name ^ ".tla") in
      write_all tla_path (Tla.Tla_printer.render tla_module);
      (match config_opt with
      | Some config ->
          let cfg_path = Filename.concat outdir (tla_module.name ^ ".cfg") in
          write_all cfg_path (Sanpou.Config.to_cfg_string config)
      | None -> ());
      let smap = Sanpou.Source_map.extract ir in
      let json_path =
        Filename.concat outdir (tla_module.name ^ ".sourcemap.json")
      in
      write_all json_path (Sanpou.Source_map.to_json smap))
    irs;
  ()

let cmd_trace file outdir =
  if not (Sys.file_exists outdir) then (
    Printf.eprintf
      "Error: directory '%s' not found.\n\
       Run 'sanpou compile -o %s' first to generate the source map.\n"
      outdir outdir;
    exit 1);
  let input = read_all file in
  let trace = Sanpou.Trace_reader.parse input in
  (* Find source map file(s) in outdir *)
  let files = Sys.readdir outdir in
  let smap =
    Array.fold_left
      (fun acc f ->
        if Filename.check_suffix f ".sourcemap.json" then
          let path = Filename.concat outdir f in
          let json = read_all path in
          acc @ Sanpou.Source_map.from_json json
        else acc)
      [] files
  in
  print_string (Sanpou.Trace_printer.render trace smap)

let () =
  let outdir = ref "." in
  let args = ref [] in
  let usage =
    "Usage: sanpou <compile|trace> <file> [-o outdir]\n\
    \  compile <file.snp>  Compile sanpou source to .tla and .sourcemap.json\n\
    \                      If <file>.json exists, also generate .cfg\n\
    \  trace <file.out>    Annotate TLC output with source info"
  in
  let speclist = [ ("-o", Arg.Set_string outdir, "Output directory") ] in
  Arg.parse speclist (fun s -> args := !args @ [ s ]) usage;
  match !args with
  | [ "compile"; file ] -> cmd_compile file !outdir
  | [ "trace"; file ] -> cmd_trace file !outdir
  | "compile" :: _ | "trace" :: _ ->
      Printf.eprintf "Error: missing file argument.\n";
      Arg.usage speclist usage;
      exit 1
  | other :: _ ->
      Printf.eprintf "Unknown subcommand: %s\n" other;
      Arg.usage speclist usage;
      exit 1
  | [] ->
      Arg.usage speclist usage;
      exit 1
