open Ppxlib

(* Check if a declreation is of type Ptyp_arrow *)
let is_function (ct : core_type) : bool =
  match ct.ptyp_desc with
  | Ptyp_arrow _ -> true
  | _ -> false

let rec unfold f state =
  match f state with
  | None -> ([], state)
  | Some (item, next_state) ->
      let rest, last = unfold f next_state in
      (item :: rest, last)

let rec core_type_to_ir_type (ocaml_type : core_type) : Ir.ir_type =
  match ocaml_type.ptyp_desc with
  | Ptyp_constr ({txt = Lident "int"; _}, []) -> Ir.Ty_int
  | Ptyp_constr ({txt = Lident "float"; _}, []) -> Ir.Ty_float
  | Ptyp_constr ({txt = Lident "string"; _}, []) -> Ir.Ty_string
  | Ptyp_constr ({txt = Lident "bool"; _}, []) -> Ir.Ty_bool
  | Ptyp_constr ({txt = Lident "list"; _}, [inner] ) -> Ir.Ty_list(core_type_to_ir_type inner)
  | _ -> Ir.Ty_illegal

(* Argument names come from labelled arrows in the signature, e.g.
   [val add : a:int -> b:int -> int]. Unlabelled arrows carry no name in
   the AST, so they get a positional one instead. *)
let param_name (label : arg_label) (index : int) : string =
  match label with
  | Nolabel -> Printf.sprintf "arg%d" index
  | Labelled label | Optional label -> label

let pval_type_to_ir_function (name : string) (ocaml_type : core_type) : Ir.ir_func =
  let rec collect index args ty =
    match ty.ptyp_desc with
    | Ptyp_arrow (label, param_ty, rest) ->
        let index = index + 1 in
        let param : Ir.ir_func_param =
          { name = param_name label index; ty = core_type_to_ir_type param_ty }
        in
        collect index (param :: args) rest
    | _ ->
        (List.rev args, ty)
  in
  let args, return_type = collect 0 [] ocaml_type in
  { name; args; return_type = core_type_to_ir_type return_type }

let get_functions (items : signature_item list) =
  List.filter_map (fun item ->
    match item.psig_desc with
    | Psig_value val_desc when is_function val_desc.pval_type ->
      let name = val_desc.pval_name.txt in
      Some (name, val_desc.pval_type)
    | _ -> None) items

let module_signature (mty : module_type) : signature_item list =
  match mty.pmty_desc with
  | Pmty_signature items -> items
  | _ ->
      Location.raise_errorf ~loc:mty.pmty_loc
        "the utest attribute expects an inline signature"

let extract_functions (items : signature_item list) : Ir.ir_service =
  get_functions items
  |> List.map (fun (name, ty) -> pval_type_to_ir_function name ty)

(* Defines a module as the test entrypoint. *)
let utest = Attribute.declare_flag "utest" Attribute.Context.Module_binding

(* Implementation items for the functions read from a module's signature. *)
let generate_bodies (service : Ir.ir_service) (written : structure_item list) :
    structure_item list =
  ignore (Gen_python.PythonGenerator.generate service);
  written

let generate_binding (mb : module_binding) : module_binding =
  match mb.pmb_expr.pmod_desc with
  | Pmod_constraint ({ pmod_desc = Pmod_structure written; _ } as impl, mty) ->
      let service = extract_functions (module_signature mty) in
      let impl =
        { impl with pmod_desc = Pmod_structure (generate_bodies service written) }
      in
      {
        mb with
        pmb_expr = { mb.pmb_expr with pmod_desc = Pmod_constraint (impl, mty) };
      }
  | _ ->
      Location.raise_errorf ~loc:mb.pmb_loc
        "the utest attribute expects a module of the form: module M : sig ... \
         end = struct ... end"

let mapper =
  object
    inherit Ast_traverse.map as super

    method! module_binding mb =
      let mb = super#module_binding mb in
      if Attribute.has_flag utest mb then generate_binding mb else mb
  end

let () = Driver.register_transformation "utest" ~impl:mapper#structure
