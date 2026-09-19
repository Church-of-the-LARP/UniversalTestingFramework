open Ir
open Generator

let sfmt = Printf.sprintf

let rec ir_type_to_python (ty : Ir.ir_type) : string =
  match ty with
  | Ir.Ty_bool -> "bool"
  | Ir.Ty_float -> "float"
  | Ir.Ty_int -> "int"
  | Ir.Ty_list inner -> sfmt "list[%s]" (ir_type_to_python inner)
  | Ir.Ty_string -> "str"
  | _ -> "any"

let ir_func_to_python (func : Ir.ir_func) : string =
  let args = List.map (fun {name; ty} -> sfmt "%s: %s" name (ir_type_to_python ty)) func.args in
  let param_list = String.concat ", " args in
  sfmt "def %s(%s) -> %s:\n\tpass\n" func.name param_list (ir_type_to_python func.return_type)

module PythonGenerator : Generator = struct
  let generate (service : Ir.ir_service) : string =
    let functions = List.map (ir_func_to_python) service
      |> String.concat "\n\n" in
    let oc = open_out "main.py" in
    Printf.fprintf oc "%s\n" functions;
    close_out oc;
    functions
end
