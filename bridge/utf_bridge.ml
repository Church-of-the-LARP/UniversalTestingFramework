(* Public entry point of the utf_bridge runtime library.

   [Utf_bridge.Error] is the very same exception as [Error.Error]: every module
   of this library raises it. *)

exception Error = Error.Error

module Value = Value
module Json = Json

let call = Client.call
