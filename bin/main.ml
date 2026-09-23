(* `utf gen python FILE` scaffolds and refreshes the python side of every
   [@@utest] module in FILE:

     - <service>_server.py is generated code, regenerated on every run;
     - <service>_functions.py is where the implementation work lives: it is
       created when missing and never overwritten unless --force is passed.

   The OCaml signature stays the source of truth for both. *)

open Cmdliner

let write_file (path : string) (contents : string) : unit =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let fail message =
  Printf.eprintf "utf: %s\n" message;
  exit 1

let gen_python (file : string) (force : bool) : unit =
  let services =
    try Utf_core.Extract.services_of_file file with
    | Utf_core.Extract.Bad_source message -> fail message
    | Sys_error message -> fail message
    | raised -> fail (Printexc.to_string raised)
  in
  if services = [] then (
    Printf.eprintf "utf: no [@@utest] modules found in %s\n" file;
    exit 1);
  let directory = Filename.dirname file in
  List.iter
    (fun (module_name, service) ->
      let service_id = Utf_core.Names.service_id_of_module module_name in
      let server_path =
        Filename.concat directory (Utf_core.Names.server_file service_id)
      in
      write_file server_path
        (Utf_core.Gen_python.server_source ~service_id service);
      Printf.printf "wrote %s\n" server_path;
      let functions_path =
        Filename.concat directory (Utf_core.Names.functions_file service_id)
      in
      if Sys.file_exists functions_path && not force then
        Printf.printf "kept  %s (already exists; --force overwrites)\n"
          functions_path
      else (
        write_file functions_path
          (Utf_core.Gen_python.functions_stub ~service_id service);
        Printf.printf "wrote %s\n" functions_path))
    services

let file_arg =
  let doc = "OCaml source file containing the [@@utest] modules." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let force_arg =
  Arg.(
    value & flag
    & info [ "f"; "force" ] ~doc:"Overwrite an existing <service>_functions.py.")

let gen_python_cmd =
  let doc =
    "Generate the python side (wrapper server + function stubs) for the \
     [@@utest] modules in FILE."
  in
  Cmd.v (Cmd.info "python" ~doc) Term.(const gen_python $ file_arg $ force_arg)

let gen_cmd =
  let doc = "Generate code for the bridged language." in
  Cmd.group (Cmd.info "gen" ~doc) [ gen_python_cmd ]

let main_cmd =
  let doc =
    "Universal testing framework: write OCaml specs for functions \
     implemented in other languages and call them over the bridge."
  in
  Cmd.group (Cmd.info "utf" ~doc) [ gen_cmd ]

let () = exit (Cmd.eval main_cmd)
