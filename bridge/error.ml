(* The single shared exception raised by every module of the bridge runtime.
   [Utf_bridge.Error] is an alias of this very exception, so the identity is
   shared across the whole library. *)

exception Error of string
