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

include module type of ArraySignature

module type MONOVECTOR = sig
  type element
  #undef INJECTIVE
  #undef VECTOR
  #undef SYNONYM
  #undef ELEMENT
  #undef VECTOR'
  #undef ELEMENT'
  #define INJECTIVE
  #define VECTOR   vector
  #define SYNONYM  t
  #define ELEMENT  element
  #define VECTOR'  vector
  #define ELEMENT' element
  #include "Vector.frag.mli"
end

module type POLYVECTOR = sig
  #undef INJECTIVE
  #undef VECTOR
  #undef SYNONYM
  #undef ELEMENT
  #undef VECTOR'
  #undef ELEMENT'
  #define INJECTIVE !
  #define VECTOR   'a vector
  #define SYNONYM  'a t
  #define ELEMENT  'a
  #define VECTOR'  'b vector
  #define ELEMENT' 'b
  #include "Vector.frag.mli"
end

module Int : MONOVECTOR with type element = int

module Mono : sig

  module OutOfArray (A : MONOARRAY)
  : MONOVECTOR with type element = A.element

  (**The functor [Make] lets the user choose the type [element].
     The function [Array.make] is used to allocate arrays. *)
  module Make (E : sig
    type t
  end)
  : MONOVECTOR with type element = E.t

end

module Poly : POLYVECTOR

module IntArray : module type of IntArray
