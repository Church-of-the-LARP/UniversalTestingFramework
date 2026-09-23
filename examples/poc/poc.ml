(* Proof of concept for the bridge.

   The module below is written as a signature only. The PPX expands every
   [val] into a function that marshals its arguments, calls service "poc"
   over a unix socket and unmarshals the result:

     let total ~xs =
       Utf_bridge.Value.to_int ~context:"Poc.total"
         (Utf_bridge.call ~service:"poc" ~func:"total"
            [ ("xs", Utf_bridge.Value.list Utf_bridge.Value.int xs) ])

   The python side is two files:

     poc_functions.py   your implementation (the generator only creates it
                        when it is missing; --force overwrites)
     poc_server.py      the generated wrapper server (safe to regenerate)

   Workflow:

     # regenerate the python side after changing the signature above
     dune exec utf -- gen python examples/poc/poc.ml

     # implement poc_functions.py, then run any of these
     dune build @examples/poc/runtest
     (cd examples/poc && dune exec ./poc.exe)
     UTF_SERVER=examples/poc/poc_server.py dune exec examples/poc/poc.exe

   The wrapper server is spawned automatically on the first call and stopped
   when this program exits. Inspect what the PPX generated with:

     dune describe pp examples/poc/poc.ml *)

module Poc : sig
  val total : xs:int list -> int
  val repeat : s:string -> n:int -> string
  val divide : a:int -> b:int -> int
end = struct end [@@utest]

let () =
  Printf.printf "total [1; 2; 3] = %d\n" (Poc.total ~xs:[ 1; 2; 3 ]);
  Printf.printf "repeat \"ab\" 3  = %S\n" (Poc.repeat ~s:"ab" ~n:3);
  Printf.printf "divide 7 2      = %d\n" (Poc.divide ~a:7 ~b:2);
  (match Poc.divide ~a:1 ~b:0 with
  | _ -> print_endline "divide 1 0      = no error?!"
  | exception Utf_bridge.Error message ->
      Printf.printf "divide 1 0      -> Utf_bridge.Error: %s\n" message);
  print_endline "proof of concept finished"
