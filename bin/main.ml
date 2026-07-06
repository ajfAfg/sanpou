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
  let config_opt = load_compile_config file in
  let config = Option.value ~default:Sanpou.Config.default config_opt in
  match Sanpou.Compile.compile ~config input with
  | Error { loc = { line; col }; message } ->
      Printf.eprintf "%s:%d:%d: %s\n" file line col message;
      exit 1
  | Ok outputs ->
      if not (Sys.file_exists outdir) then Sys.mkdir outdir 0o755;
      List.iter
        (fun (o : Sanpou.Compile.output) ->
          let name = o.tla_module.name in
          let tla_path = Filename.concat outdir (name ^ ".tla") in
          write_all tla_path (Tla.Tla_printer.render o.tla_module);
          (match config_opt with
          | Some config ->
              let cfg_path = Filename.concat outdir (name ^ ".cfg") in
              let constants =
                List.concat_map
                  (function
                    | Tla.Tla_ast.DConstants names -> names | _ -> [])
                  o.tla_module.body
              in
              write_all cfg_path (Sanpou.Config.to_cfg_string ~constants config)
          | None -> ());
          let json_path = Filename.concat outdir (name ^ ".sourcemap.json") in
          write_all json_path (Sanpou.Source_map.to_json o.source_map))
        outputs

let cmd_trace file outdir =
  if not (Sys.file_exists outdir) then (
    Printf.eprintf
      "Error: directory '%s' not found.\n\
       Run 'sanpou compile -o %s' first to generate the source map.\n"
      outdir outdir;
    exit 1);
  let input = read_all file in
  let trace = Sanpou.Trace_reader.parse input in
  (* Load only the source map of the module actually checked: label names
     (L1, L2, ...) collide across modules, so merging several source maps
     would mix up descriptions. *)
  let module_name =
    match trace.module_name with
    | Some name -> name
    | None -> Filename.remove_extension (Filename.basename file)
  in
  let smap_path = Filename.concat outdir (module_name ^ ".sourcemap.json") in
  if not (Sys.file_exists smap_path) then (
    let candidates =
      Sys.readdir outdir |> Array.to_list
      |> List.filter (fun f -> Filename.check_suffix f ".sourcemap.json")
    in
    Printf.eprintf "Error: source map '%s' not found for module '%s'.\n"
      smap_path module_name;
    (match candidates with
    | [] -> ()
    | _ ->
        Printf.eprintf "Available source maps in '%s': %s\n" outdir
          (String.concat ", " candidates));
    Printf.eprintf
      "Run 'sanpou compile <file.snp> -o %s' first to generate it.\n" outdir;
    exit 1);
  let smap = Sanpou.Source_map.from_json (read_all smap_path) in
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
