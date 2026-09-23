(* Naming conventions shared by the PPX (which embeds the service id in the
   generated OCaml code) and the generator (which names the python files).
   Keeping them in one place means the two sides cannot drift apart. *)

(* A service is named after the OCaml module that carries the [@@utest]
   attribute, normalised to snake_case: [Spec] -> "spec",
   [MyService] -> "my_service". *)
let service_id_of_module (module_name : string) : string =
  let out = Buffer.create (String.length module_name + 8) in
  String.iteri
    (fun index character ->
      let is_upper =
        Char.uppercase_ascii character = character
        && Char.lowercase_ascii character <> character
      in
      if index > 0 && is_upper then Buffer.add_char out '_';
      Buffer.add_char out (Char.lowercase_ascii character))
    module_name;
  Buffer.contents out

(* The python module the wrapper server imports. *)
let functions_module (service_id : string) : string = service_id ^ "_functions"

(* The file the user implements. Never overwritten by the generator unless
   explicitly forced. *)
let functions_file (service_id : string) : string =
  functions_module service_id ^ ".py"

(* The generated wrapper server: socket handling and typed dispatch. *)
let server_file (service_id : string) : string = service_id ^ "_server.py"
