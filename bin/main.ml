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

let cmd_compile file outdir =
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  let cst = Sanpou.Parser.program Sanpou.Lexer.main lexbuf in
  Sanpou.Typing.check cst;
  let irs = Sanpou.Alpha_convert.transform cst |> Sanpou.Linearize.linearize in
  if not (Sys.file_exists outdir) then Sys.mkdir outdir 0o755;
  List.iter
    (fun (ir : Sanpou.Ir.module_ir) ->
      let tla_module = Sanpou.Emit_tla.generate_module ir in
      let tla_path = Filename.concat outdir (tla_module.name ^ ".tla") in
      let oc = open_out tla_path in
      output_string oc (Tla.Tla_printer.render tla_module);
      close_out oc;
      let smap = Sanpou.Source_map.extract ir in
      let json_path =
        Filename.concat outdir (tla_module.name ^ ".sourcemap.json")
      in
      let oc = open_out json_path in
      output_string oc (Sanpou.Source_map.to_json smap);
      close_out oc)
    irs;
  close_in ic

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
