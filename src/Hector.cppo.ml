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

include ArraySignature

(* -------------------------------------------------------------------------- *)

(* Set up our macros for monomorphic vectors. *)

#define VECTOR   vector
#define VECTOR'  vector
#define SYNONYM  t
#define ELEMENT  element
#define ELEMENT' element
#define ARRAY    A.t

(* -------------------------------------------------------------------------- *)

(* The signature of monomorphic vectors. *)

module type MONOVECTOR = sig
  type element
  #include "Signature.frag.mli"
end

(* -------------------------------------------------------------------------- *)

(* Integer vectors are a special case of monomorphic vectors. *)

module Int = struct
  type element = int
  module A = IntArray
  #include "Vector.frag.ml"
end

(* -------------------------------------------------------------------------- *)

(* Monomorphic vectors. *)

module Mono = struct

  module[@inline] OutOfArray (A : MONOARRAY) = struct

    type element = A.element

    #include "Vector.frag.ml"

  end (* OutOfArray *)

  module[@inline] Make (E : sig type t end) = struct

    type element = E.t

    module A = struct
      type t = element array
      let empty = [||]
      let alloc = Array.make
      let make  = Array.make
      #include "MonoArray.frag.ml"
    end

    #include "Vector.frag.ml"

  end (* Make *)

end

(* -------------------------------------------------------------------------- *)

(* Set up our macros for polymorphic vectors. *)

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

(* -------------------------------------------------------------------------- *)

(* The signature of monomorphic vectors. *)

module type POLYVECTOR = sig
  #include "Signature.frag.mli"
end

(* -------------------------------------------------------------------------- *)

(* Polymorphic vectors. *)

module Poly = struct
  module A = struct
    include Array
    let empty = [||]
    let alloc = make
    let blit_disjoint = blit
  end
  #include "Vector.frag.ml"
end
