(* Reads [@@utest] modules out of OCaml source and turns their inline
   signatures into IR services.

   Everything the bridge cannot represent is rejected HERE, with a located
   error, so the PPX and the generator CLI fail at the same place with the
   same message. The PPX turns the resulting IR into OCaml bridge calls
   (see {!Ocaml_gen}); the CLI turns it into python files
   (see {!Gen_python}). *)

open Ppxlib

let utest_attribute = "utest"

let has_utest_attribute (attributes : attributes) : bool =
  List.exists
    (fun (attribute : attribute) -> attribute.attr_name.txt = utest_attribute)
    attributes

let is_function (ct : core_type) : bool =
  match ct.ptyp_desc with
  | Ptyp_arrow _ -> true
  | _ -> false

let type_to_string (ct : core_type) : string =
  Format.asprintf "%a" Pprintast.core_type ct

(* Only types the runtime knows how to marshal cross the bridge. Anything
   else is an error here instead of a silent `any` in the generated code. *)
let rec validate_type (ct : core_type) : Ir.ir_type =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "int"; _ }, []) -> Ir.Ty_int
  | Ptyp_constr ({ txt = Lident "float"; _ }, []) -> Ir.Ty_float
  | Ptyp_constr ({ txt = Lident "string"; _ }, []) -> Ir.Ty_string
  | Ptyp_constr ({ txt = Lident "bool"; _ }, []) -> Ir.Ty_bool
  | Ptyp_constr ({ txt = Lident "list"; _ }, [ inner ]) ->
      Ir.Ty_list (validate_type inner)
  | _ ->
      Location.raise_errorf ~loc:ct.ptyp_loc
        "utest: unsupported type %s in a bridged signature (supported: int, \
         float, string, bool and lists of those)"
        (type_to_string ct)

let ir_func_of_val (val_desc : value_description) : Ir.ir_func =
  let name = val_desc.pval_name.txt in
  if not (is_function val_desc.pval_type) then
    Location.raise_errorf ~loc:val_desc.pval_loc
      "utest: val %s is not a function; bridged signatures may only contain \
       functions"
      name;
  let rec collect index args ty =
    match ty.ptyp_desc with
    | Ptyp_arrow (label, param_ty, rest) ->
        let index = index + 1 in
        let param_name, labelled =
          match label with
          | Nolabel -> (Printf.sprintf "arg%d" index, false)
          | Labelled label -> (label, true)
          | Optional _ ->
              Location.raise_errorf ~loc:ty.ptyp_loc
                "utest: optional arguments are not supported by the bridge \
                 (function %s)"
                name
        in
        let param : Ir.ir_func_param =
          { name = param_name ; ty = validate_type param_ty ; labelled }
        in
        collect index (param :: args) rest
    | _ -> (List.rev args, validate_type ty)
  in
  let args, return_type = collect 0 [] val_desc.pval_type in
  { Ir.name ; args ; return_type }

let service_of_signature (items : signature_item list) : Ir.ir_service =
  List.map
    (fun (item : signature_item) ->
      match item.psig_desc with
      | Psig_value val_desc -> ir_func_of_val val_desc
      | _ ->
          Location.raise_errorf ~loc:item.psig_loc
            "utest: bridged signatures may only contain val declarations")
    items

(* [module Spec : sig ... end = struct end [@@utest]] is the only shape we
   bridge: the signature is the source of truth, and the body has to be empty
   because the val declarations are what get expanded. *)
let service_of_binding (mb : module_binding) : string * Ir.ir_service =
  let module_name =
    match mb.pmb_name.txt with
    | Some name -> name
    | None ->
        Location.raise_errorf ~loc:mb.pmb_name.loc
          "utest: anonymous modules cannot be bridged"
  in
  match mb.pmb_expr.pmod_desc with
  | Pmod_constraint ({ pmod_desc = Pmod_structure body; _ }, mty) ->
      (match body with
      | [] -> ()
      | item :: _ ->
          Location.raise_errorf ~loc:item.pstr_loc
            "utest: the body of %s must be empty; its val declarations are \
             expanded into bridge calls, and the implementation lives in the \
             bridged language"
            module_name);
      let items =
        match mty.pmty_desc with
        | Pmty_signature items -> items
        | _ ->
            Location.raise_errorf ~loc:mty.pmty_loc
              "utest: %s needs an inline signature: module %s : sig ... end = \
               struct end [@@utest]"
              module_name module_name
      in
      (module_name, service_of_signature items)
  | _ ->
      Location.raise_errorf ~loc:mb.pmb_loc
        "utest: %s needs the shape: module %s : sig ... end = struct end \
         [@@utest]"
        module_name module_name

let services_of_structure (structure : structure) :
    (string * Ir.ir_service) list =
  List.filter_map
    (fun (item : structure_item) ->
      match item.pstr_desc with
      | Pstr_module mb when has_utest_attribute mb.pmb_attributes ->
          Some (service_of_binding mb)
      | _ -> None)
    structure

(* The compiler (through the PPX) wants the raw exception so it can report it
   with its own machinery; the generator CLI wants a plain string.
   [services_of_file] is the CLI path, so it re-raises a formatted message. *)
exception Bad_source of string

(* [Location.report_exception] knows how to print located errors (our own
   located errors and parser output alike); everything else falls back to the
   standard printer. *)
let exception_message (raised : exn) : string =
  let buffer = Buffer.create 512 in
  let formatter = Format.formatter_of_buffer buffer in
  Location.report_exception formatter raised;
  Format.pp_print_flush formatter ();
  String.trim (Buffer.contents buffer)

let services_of_file (path : string) : (string * Ir.ir_service) list =
  try
    let channel = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () ->
        let lexbuf = Lexing.from_channel channel in
        Location.init lexbuf path;
        Lexing.set_filename lexbuf path;
        let structure = Parse.implementation lexbuf in
        services_of_structure structure)
  with raised -> raise (Bad_source (exception_message raised))
