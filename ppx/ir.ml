open Ppxlib

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
  ty   : ir_type ;
}


type ir_func = {
  name        : string ;
  args        : ir_func_param list ;
  return_type : ir_type ;
}

type ir_service = ir_func list
