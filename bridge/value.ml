(* Bridge values: the typed subset of JSON that can cross the language
   boundary (booleans, numbers, strings, arrays). *)

type t =
  | VInt of int
  | VFloat of float
  | VString of string
  | VBool of bool
  | VList of t list

let int x = VInt x
let float x = VFloat x
let string x = VString x
let bool x = VBool x
let list f xs = VList (List.map f xs)

let kind = function
  | VInt _ -> "int"
  | VFloat _ -> "float"
  | VString _ -> "string"
  | VBool _ -> "bool"
  | VList _ -> "list"

let mismatch ~context expected v =
  raise
    (Error.Error
       (Printf.sprintf "%s: expected %s, got %s" context expected (kind v)))

let to_int ~context = function VInt i -> i | v -> mismatch ~context "int" v

let to_float ~context = function
  | VFloat f -> f
  | VInt i -> float_of_int i
  | v -> mismatch ~context "float" v

let to_string ~context = function
  | VString s -> s
  | v -> mismatch ~context "string" v

let to_bool ~context = function VBool b -> b | v -> mismatch ~context "bool" v

let to_list ~context f = function
  | VList l -> List.map f l
  | v -> mismatch ~context "list" v

let rec to_json = function
  | VInt i -> Json.Int i
  | VFloat f -> Json.Float f
  | VString s -> Json.String s
  | VBool b -> Json.Bool b
  | VList l -> Json.List (List.map to_json l)

let rec of_json = function
  | Json.Int i -> VInt i
  | Json.Float f -> VFloat f
  | Json.String s -> VString s
  | Json.Bool b -> VBool b
  | Json.List l -> VList (List.map of_json l)
  | Json.Null -> raise (Error.Error "unexpected null in bridge value")
  | Json.Object _ -> raise (Error.Error "unexpected object in bridge value")
