(* The [@@utest] rewriter.

   [module Spec : sig val add : a:int -> b:int -> int end = struct end] becomes
   a module whose body is a set of bridge calls into the service implemented
   in the bridged language. The signature stays in place as a module
   constraint, so the compiler checks the generated bindings against the
   declared types.

   Everything the bridge cannot represent (unsupported types, optional
   arguments, non-function vals, a non-empty body) is rejected with a located
   error by {!Utf_core.Extract}. This rewriter never writes files: the python
   side is produced by `utf gen python`. *)

open Ppxlib

let utest = Attribute.declare_flag "utest" Attribute.Context.Module_binding

let mapper =
  object
    inherit Ast_traverse.map as super

    method! module_binding mb =
      let mb = super#module_binding mb in
      if Attribute.has_flag utest mb then
        Utf_core.Ocaml_gen.expand_binding mb
      else mb
  end

let () = Driver.register_transformation "utest" ~impl:mapper#structure
