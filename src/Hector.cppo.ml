(******************************************************************************)
(*                                                                            *)
(*                                   Hector                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2024--2024 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

#include "Loop.frag.ml"

(* -------------------------------------------------------------------------- *)

(* Integer vectors and monomorphic vectors. *)

#define VECTOR   vector
#define VECTOR'  vector
#define SYNONYM  t
#define ELEMENT  element
#define ELEMENT' element
#define ARRAY    A.t

module type MONOVECTOR = sig
  type element
  #include "Signature.frag.mli"
end

module Int = struct

  type element = int

  module A = IntArray

  #include "Vector.frag.ml"

end

module Mono = struct

  module[@inline] Make_ (X : sig
    type t
    val alloc : int -> t -> t array
    val make  : int -> t -> t array
  end) = struct

    type element = X.t

    module A = struct
      type t = element array
      let empty = [||]
      let alloc = X.alloc
      let make  = X.make
      #include "MonoArray.frag.ml"
    end

    #include "Vector.frag.ml"

  end (* Make_ *)

  module[@inline] Make (X : sig type t end) =
    Make_(struct
      include X
      let alloc = Array.make
      let make = Array.make
    end)

end

#undef VECTOR
#undef VECTOR'
#undef SYNONYM
#undef ELEMENT
#undef ELEMENT'
#undef ARRAY

#define VECTOR   'a vector
#define VECTOR'  'b vector
#define SYNONYM  'a t
#define ELEMENT  'a
#define ELEMENT' 'b
#define ARRAY    'a A.t

module type POLYVECTOR = sig
  #include "Signature.frag.mli"
end

module Poly = struct

  module A = struct
    include Array
    let alloc = make
    let blit_disjoint = blit
  end

  #include "Vector.frag.ml"

end
