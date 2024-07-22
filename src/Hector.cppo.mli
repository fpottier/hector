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

module type POLYVECTOR = sig
  #undef VECTOR
  #undef SYNONYM
  #undef ELEMENT
  #undef VECTOR'
  #undef ELEMENT'
  #define VECTOR   'a vector
  #define SYNONYM  'a t
  #define ELEMENT  'a
  #define VECTOR'  'b vector
  #define ELEMENT' 'b
  #include "Signature.frag.mli"
end

module Int : MONOVECTOR with type element = int

module Mono : sig
  module Make_ (X : sig
    type t
    val alloc : int -> t -> t array
    val make  : int -> t -> t array
  end)
  : MONOVECTOR with type element = X.t
  module Make (X : sig
    type t
  end)
  : MONOVECTOR with type element = X.t
end

module Poly : POLYVECTOR
