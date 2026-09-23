(* Extraction of [@@utest] modules from source, shared by the PPX and the
   `utf gen python` CLI. *)

exception Bad_source of string
(** Raised by {!services_of_file} for anything that stops a source file from
    being turned into services; the message already carries file and line. *)

val has_utest_attribute : Ppxlib.attributes -> bool

val service_of_binding : Ppxlib.module_binding -> string * Ir.ir_service

val services_of_structure :
  Ppxlib.structure -> (string * Ir.ir_service) list

val services_of_file : string -> (string * Ir.ir_service) list
