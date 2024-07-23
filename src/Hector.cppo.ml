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


module type MONOVECTOR = sig
  type element
  #undef VECTOR
  #undef SYNONYM
  #undef ELEMENT
  #undef VECTOR'
  #undef ELEMENT'
  #define VECTOR   vector
  #define SYNONYM  t
  #define ELEMENT  element
  #define VECTOR'  vector
  #define ELEMENT' element
  #include "Signature.frag.mli"
end

module Int  = Int

module Mono = Mono

#undef VECTOR
#undef SYNONYM
#undef ELEMENT
#undef VECTOR'
#undef ELEMENT'

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

  #include "Loop.frag.ml"
  #include "Common.frag.ml"

end
