(* Runtime client for the utf_bridge protocol.

   The generated PPX code calls [Client.call ~service ~func args]; this module
   speaks the newline-delimited JSON protocol:

     request   {"service": s, "func": f, "args": {name: value, ...}}
     success   {"ok": true, "value": value}
     failure   {"ok": false, "error": "..."}

   Connections are kept open (one per service, per process) and the wrapper
   server process is spawned on demand.

   NOT THREAD-SAFE: the connection table, the spawn bookkeeping and the
   request/response exchange are unsynchronised process-global state, so a
   single process may only issue one bridge call at a time.

   Style note: nested [match] expressions used as branch bodies are wrapped in
   [begin ... end] so that no sequencing [;] can be swallowed by the last
   branch. *)

let protocol_error detail = "utf_bridge: protocol error: " ^ detail

(* Raises the library-wide exception.  Kept polymorphic so that it can be used
   in any branch position. *)
let raise_error msg = raise (Error.Error msg)

let protocol_fail fmt =
  Printf.ksprintf (fun detail -> raise_error (protocol_error detail)) fmt

let ignore_errors f = try f () with _ -> ()

let getenv_nonempty name =
  match Sys.getenv_opt name with Some v when v <> "" -> Some v | _ -> None

let cwd () = try Sys.getcwd () with _ -> "."

(* ------------------------------------------------------------------ *)
(* Connection state                                                    *)
(* ------------------------------------------------------------------ *)

type conn = { ic : in_channel; oc : out_channel }

let connections : (string, conn) Hashtbl.t = Hashtbl.create 8
let spawned : (string, int) Hashtbl.t = Hashtbl.create 8
let cleanup_registered : (string, unit) Hashtbl.t = Hashtbl.create 8

let socket_path service =
  match getenv_nonempty "UTF_SOCKET" with
  | Some p -> p
  | None ->
      Filename.concat (Filename.get_temp_dir_name ())
        ("utf_bridge_" ^ service ^ "_" ^ string_of_int (Unix.getpid ())
       ^ ".sock")

(* Where to find `<service>_server.py`: [UTF_SERVER] first, then the current
   directory (where dune runs test actions), then next to the running
   executable (where `dune exec` leaves the build directory setup while the
   process itself keeps the caller's working directory). *)
let server_script service =
  match getenv_nonempty "UTF_SERVER" with
  | Some p -> p
  | None ->
      let name = service ^ "_server.py" in
      let in_cwd = Filename.concat (cwd ()) name in
      if Sys.file_exists in_cwd then in_cwd
      else
        let beside_executable =
          Filename.concat (Filename.dirname Sys.executable_name) name
        in
        if Sys.file_exists beside_executable then beside_executable else in_cwd

let python_interpreter () =
  match getenv_nonempty "UTF_PYTHON" with Some p -> p | None -> "python3"

let try_connect path =
  let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  match Unix.connect fd (Unix.ADDR_UNIX path) with
  | () -> Some fd
  | exception _ ->
      ignore_errors (fun () -> Unix.close fd);
      None

let close_conn service conn =
  Hashtbl.remove connections service;
  ignore_errors (fun () -> close_in_noerr conn.ic);
  ignore_errors (fun () -> close_out_noerr conn.oc)

(* ------------------------------------------------------------------ *)
(* Spawning the wrapper server                                         *)
(* ------------------------------------------------------------------ *)

(* Registered once per service, on the first successful spawn: terminate the
   server and remove its socket file when this process exits. *)
let register_cleanup service path =
  let already = Hashtbl.mem cleanup_registered service in
  if not already then begin
    Hashtbl.replace cleanup_registered service ();
    at_exit (fun () ->
        match Hashtbl.find_opt spawned service with
        | None -> ()
        | Some pid ->
            ignore_errors (fun () -> Unix.kill pid Sys.sigterm);
            ignore_errors (fun () -> Unix.unlink path))
  end

let spawn_server service path =
  let script = server_script service in
  let dir = cwd () in
  let manual_hint python =
    Printf.sprintf
      "Set UTF_SERVER to the wrapper server script, or start it manually:\n  %s \
       %s --socket %s"
      python script path
  in
  let script_missing = not (Sys.file_exists script) in
  if script_missing then begin
    let python = python_interpreter () in
    raise_error
      (Printf.sprintf
         "utf_bridge: no server for service %S: could not connect to socket %s \
          and the server script %s does not exist (current directory: %s).\n%s"
         service path script dir (manual_hint python))
  end;
  let python = python_interpreter () in
  let argv =
    [|
      python;
      script;
      "--socket";
      path;
      "--parent-pid";
      string_of_int (Unix.getpid ());
    |]
  in
  let devnull = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  ignore_errors (fun () -> Unix.set_close_on_exec devnull);
  let pid =
    match Unix.create_process python argv devnull Unix.stdout Unix.stderr with
    | pid -> pid
    | exception e ->
        ignore_errors (fun () -> Unix.close devnull);
        raise_error
          (Printf.sprintf
             "utf_bridge: could not start the server for service %S: failed to \
              run %S (%s). Set UTF_PYTHON to a working Python 3 interpreter."
             service python (Printexc.to_string e))
  in
  ignore_errors (fun () -> Unix.close devnull);
  Hashtbl.replace spawned service pid;
  (* poll for the socket, up to 5 s, in 25 ms steps *)
  let deadline = Unix.gettimeofday () +. 5.0 in
  let rec poll () =
    match try_connect path with
    | Some fd -> fd
    | None ->
        let now = Unix.gettimeofday () in
        if now >= deadline then begin
          ignore_errors (fun () -> Unix.kill pid Sys.sigkill);
          ignore_errors (fun () -> ignore (Unix.waitpid [ Unix.WNOHANG ] pid));
          ignore_errors (fun () -> Unix.unlink path);
          raise_error
            (Printf.sprintf
               "utf_bridge: the server for service %S did not come up within 5 s \
                (socket: %s).\n%s"
               service path (manual_hint python))
        end
        else begin
          ignore_errors (fun () -> ignore (Unix.select [] [] [] 0.025));
          poll ()
        end
  in
  let fd = poll () in
  register_cleanup service path;
  fd

(* ------------------------------------------------------------------ *)
(* Connecting                                                          *)
(* ------------------------------------------------------------------ *)

let connect_service service =
  match Hashtbl.find_opt connections service with
  | Some conn -> conn
  | None ->
      let path = socket_path service in
      let fd =
        match try_connect path with
        | Some fd -> fd
        | None -> spawn_server service path
      in
      let ic = Unix.in_channel_of_descr fd in
      (* a second descriptor for writing, so that closing one channel does not
         invalidate the other *)
      let oc = Unix.out_channel_of_descr (Unix.dup fd) in
      let conn = { ic; oc } in
      Hashtbl.replace connections service conn;
      conn

(* ------------------------------------------------------------------ *)
(* Request / response                                                  *)
(* ------------------------------------------------------------------ *)

exception Broken of string

(* Writing to a socket whose peer has gone away raises SIGPIPE, which by default
   terminates the process before we get a chance to retry.  Ignore it for the
   duration of the write only, then restore the previous disposition. *)
let restore_sigpipe previous =
  match previous with
  | Some p -> ignore_errors (fun () -> ignore (Sys.signal Sys.sigpipe p))
  | None -> ()

let with_sigpipe_ignored f =
  let previous =
    try Some (Sys.signal Sys.sigpipe Sys.Signal_ignore) with _ -> None
  in
  match f () with
  | v ->
      restore_sigpipe previous;
      v
  | exception e ->
      restore_sigpipe previous;
      raise e

let make_request service func args =
  let args_json =
    Json.Object
      (List.map (fun (name, value) -> (name, Value.to_json value)) args)
  in
  let json =
    Json.Object
      [
        ("service", Json.String service);
        ("func", Json.String func);
        ("args", args_json);
      ]
  in
  Json.to_string json ^ "\n"

let parse_success service json =
  match Json.member "value" json with
  | None ->
      protocol_fail "successful response of service %S has no \"value\" field"
        service
  | Some value -> begin
      match Value.of_json value with
      | v -> v
      | exception Error.Error msg -> raise_error (protocol_error msg)
    end

let parse_failure service json =
  match Json.member "error" json with
  | Some (Json.String msg) ->
      (* logical failures are reported verbatim *)
      raise_error msg
  | Some other ->
      protocol_fail
        "failure response of service %S has a non-string \"error\" field: %s"
        service (Json.to_string other)
  | None ->
      protocol_fail "failure response of service %S has no \"error\" field"
        service

let parse_response service line =
  let json =
    match Json.of_string line with
    | json -> json
    | exception Json.Parse_error msg ->
        protocol_fail "invalid JSON in the response of service %S (%s): %s"
          service msg line
  in
  begin
    match json with
    | Json.Object _ -> ()
    | other ->
        protocol_fail "response of service %S is not a JSON object: %s" service
          (Json.to_string other)
  end;
  match Json.member "ok" json with
  | Some (Json.Bool true) -> parse_success service json
  | Some (Json.Bool false) -> parse_failure service json
  | Some other ->
      protocol_fail
        "response of service %S has a non-boolean \"ok\" field: %s" service
        (Json.to_string other)
  | None ->
      protocol_fail "response of service %S has no \"ok\" field" service

let exchange service request =
  let conn = connect_service service in
  let send_and_receive () =
    with_sigpipe_ignored (fun () ->
        output_string conn.oc request;
        flush conn.oc;
        input_line conn.ic)
  in
  match send_and_receive () with
  | line -> line
  | exception (End_of_file | Sys_error _ | Unix.Unix_error _) ->
      close_conn service conn;
      raise (Broken "the connection to the service was lost")

(* ------------------------------------------------------------------ *)
(* Entry point                                                         *)
(* ------------------------------------------------------------------ *)

let failure_message service func reason =
  Printf.sprintf "utf_bridge: call to %S in service %S failed (socket: %s): %s"
    func service (socket_path service) reason

let call ~service ~func args =
  let request = make_request service func args in
  let rec attempt retried =
    match exchange service request with
    | line -> parse_response service line
    | exception Broken reason ->
        if retried then
          raise_error
            (Printf.sprintf
               "utf_bridge: giving up on service %S (%s): %s (socket: %s)"
               service func reason (socket_path service))
        else attempt true
  in
  try attempt false with
  | Error.Error msg -> raise (Error.Error msg)
  | Sys.Break -> raise Sys.Break
  | e -> raise_error (failure_message service func (Printexc.to_string e))
