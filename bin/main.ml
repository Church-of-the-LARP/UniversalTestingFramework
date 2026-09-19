module Spec : sig
  val add : a:int -> b:int -> int
  val greet : s:string -> bool
  val names : n:int -> string list
  val scale : x:float -> y:float -> float
end = struct
  let add ~a ~b = a + b
  let greet ~s = String.length s > 0
  let names ~n = List.init n string_of_int
  let scale ~x ~y = x *. y
end [@@utest]

let check name ok =
  if ok then Printf.printf "ok   %s\n" name
  else (
    Printf.printf "FAIL %s\n" name;
    exit 1)

let () =
  check "add" (Spec.add ~a:1 ~b:2 = 3);
  check "greet" (Spec.greet ~s:"hi" && not (Spec.greet ~s:""));
  check "names" (Spec.names ~n:2 = [ "0"; "1" ]);
  check "scale" (Spec.scale ~x:2.0 ~y:3.0 = 6.0);
  print_endline "ppx smoke test passed"
