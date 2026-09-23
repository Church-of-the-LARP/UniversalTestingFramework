(* Builds the OCaml side of the bridge.

   Every function in the IR becomes a [let] binding that marshals its
   arguments, performs the socket call, and unmarshals the result:

     let add ~a ~b =
       Utf_bridge.Value.to_int ~context:"Spec.add"
         (Utf_bridge.call ~service:"spec" ~func:"add"
            [ ("a", Utf_bridge.Value.int a); ("b", Utf_bridge.Value.int b) ])

   The declared signature stays in place as a module constraint, so a
   mismatch between the signature and what the bridge can marshal is a
   compile error rather than a runtime surprise. *)

open Ppxlib

let parse_lid (path : string) : Longident.t =
  match String.split_on_char '.' path with
  | [] -> Lident path
  | first :: rest ->
      List.fold_left (fun acc part -> Ldot (acc, part)) (Lident first) rest

let ident ~loc (path : string) : expression =
  Ast_builder.Default.pexp_ident ~loc { loc ; txt = parse_lid path }

let apply ~loc (fn : expression) (args : (arg_label * expression) list) :
    expression =
  Ast_builder.Default.pexp_apply ~loc fn args

let string ~loc (value : string) : expression =
  Ast_builder.Default.estring ~loc value

let unsupported_type ~loc (ty : Ir.ir_type) : 'a =
  ignore ty;
  Location.raise_errorf ~loc
    "utest: internal error: unsupported type reached code generation"

(* A function from a value to [Utf_bridge.Value.t]: [Value.int],
   [Value.list (Value.list Value.int)], ... *)
let rec encoder ~loc (ty : Ir.ir_type) : expression =
  match ty with
  | Ir.Ty_int -> ident ~loc "Utf_bridge.Value.int"
  | Ir.Ty_float -> ident ~loc "Utf_bridge.Value.float"
  | Ir.Ty_string -> ident ~loc "Utf_bridge.Value.string"
  | Ir.Ty_bool -> ident ~loc "Utf_bridge.Value.bool"
  | Ir.Ty_list inner ->
      apply ~loc
        (ident ~loc "Utf_bridge.Value.list")
        [ (Nolabel, encoder ~loc inner) ]
  | Ir.Ty_custom _ | Ir.Ty_illegal -> unsupported_type ~loc ty

(* Decodes a [Utf_bridge.Value.t] expression into the declared type. Nested
   lists get depth-indexed lambda parameters so nothing can be captured. *)
let rec decoder ~loc ~context ~depth (ty : Ir.ir_type) (value : expression) :
    expression =
  let context_expr = string ~loc context in
  match ty with
  | Ir.Ty_int ->
      apply ~loc
        (ident ~loc "Utf_bridge.Value.to_int")
        [ (Labelled "context", context_expr) ; (Nolabel, value) ]
  | Ir.Ty_float ->
      apply ~loc
        (ident ~loc "Utf_bridge.Value.to_float")
        [ (Labelled "context", context_expr) ; (Nolabel, value) ]
  | Ir.Ty_string ->
      apply ~loc
        (ident ~loc "Utf_bridge.Value.to_string")
        [ (Labelled "context", context_expr) ; (Nolabel, value) ]
  | Ir.Ty_bool ->
      apply ~loc
        (ident ~loc "Utf_bridge.Value.to_bool")
        [ (Labelled "context", context_expr) ; (Nolabel, value) ]
  | Ir.Ty_list inner ->
      let element = Printf.sprintf "__utf_%d" depth in
      let element_fn =
        Ast_builder.Default.pexp_fun ~loc Nolabel None
          (Ast_builder.Default.pvar ~loc element)
          (decoder ~loc ~context ~depth:(depth + 1) inner
             (Ast_builder.Default.evar ~loc element))
      in
      apply ~loc
        (ident ~loc "Utf_bridge.Value.to_list")
        [ (Labelled "context", context_expr) ; (Nolabel, element_fn) ;
          (Nolabel, value) ]
  | Ir.Ty_custom _ | Ir.Ty_illegal -> unsupported_type ~loc ty

let binding_of_func ~loc ~service ~service_name (func : Ir.ir_func) :
    structure_item =
  let context = service_name ^ "." ^ func.name in
  let argument_entries =
    List.map
      (fun (param : Ir.ir_func_param) ->
        ( param.name,
          apply ~loc
            (encoder ~loc param.ty)
            [ (Nolabel, Ast_builder.Default.evar ~loc param.name) ] ))
      func.args
  in
  let arguments =
    Ast_builder.Default.elist ~loc
      (List.map
         (fun (name, value) ->
           Ast_builder.Default.pexp_tuple ~loc [ string ~loc name ; value ])
         argument_entries)
  in
  let call =
    apply ~loc
      (ident ~loc "Utf_bridge.call")
      [ (Labelled "service", string ~loc service) ;
        (Labelled "func", string ~loc func.name) ;
        (Nolabel, arguments) ]
  in
  let body = decoder ~loc ~context ~depth:0 func.return_type call in
  let body =
    List.fold_right
      (fun (param : Ir.ir_func_param) acc ->
        Ast_builder.Default.pexp_fun ~loc
          (if param.labelled then Labelled param.name else Nolabel)
          None
          (Ast_builder.Default.pvar ~loc param.name)
          acc)
      func.args body
  in
  Ast_builder.Default.pstr_value ~loc Nonrecursive
    [ Ast_builder.Default.value_binding ~loc
        ~pat:(Ast_builder.Default.pvar ~loc func.name)
        ~expr:body
    ]

let bindings ~loc ~service ~service_name (ir : Ir.ir_service) : structure =
  List.map (binding_of_func ~loc ~service ~service_name) ir

(* Replaces the (required-empty) module body with the generated bindings.
   The signature on the module constraint is kept, so the compiler checks the
   bridge bindings against the declared types. *)
let expand_binding (mb : module_binding) : module_binding =
  let module_name, ir = Extract.service_of_binding mb in
  let service = Names.service_id_of_module module_name in
  match mb.pmb_expr.pmod_desc with
  | Pmod_constraint (impl, mty) ->
      let generated =
        bindings ~loc:impl.pmod_loc ~service ~service_name:module_name ir
      in
      let impl = { impl with pmod_desc = Pmod_structure generated } in
      {
        mb with
        pmb_expr = { mb.pmb_expr with pmod_desc = Pmod_constraint (impl, mty) };
      }
  | _ ->
      (* [Extract.service_of_binding] rejects every other shape. *)
      Location.raise_errorf ~loc:mb.pmb_loc
        "utest: internal error: malformed module binding"