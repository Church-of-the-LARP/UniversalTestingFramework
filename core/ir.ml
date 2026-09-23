(* Language-independent description of a bridged service: what the PPX reads
   out of an [@@utest] signature and what every generator consumes. *)

type ir_type =
  | Ty_int
  | Ty_string
  | Ty_float
  | Ty_bool
  | Ty_custom of string
  | Ty_list of ir_type
  | Ty_illegal

type ir_func_param = {
  name : string ;
  ty : ir_type ;
  (* [true] when the parameter came from a labelled arrow ([a:int -> ...]),
     [false] for a positional one ([int -> ...]). The OCaml bridge bindings
     have to rebuild the same surface, and the python signature uses [name]
     for positional parameters too. *)
  labelled : bool ;
}

type ir_func = {
  name : string ;
  args : ir_func_param list ;
  return_type : ir_type ;
}

type ir_service = ir_func list
