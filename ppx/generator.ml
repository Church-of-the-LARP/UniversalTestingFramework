open Ir

module type Generator = sig
  val generate : Ir.ir_service -> string
end
