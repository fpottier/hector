(* This code is functionally equivalent to an application of the functor
   [Mono.Make] to the module [X] below. However, by forcing the compiler to
   recompile the code with the knowledge that the type [t] is [int], we get
   better code. In particular, when reading and writing arrays, the special
   case of floating-point arrays (tagged 254) disappears; and when writing
   arrays, the write barrier (_caml_modify) vanishes. *)

module X = struct
  type t = int
  let make = Array.make
end

#include "MonoBody.frag.ml"
