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
  #undef ELEMENT
  #define VECTOR vector
  #define ELEMENT   element
  #include "Signature.frag.mli"
end

module type POLYVECTOR = sig
  #undef VECTOR
  #undef ELEMENT
  #define VECTOR 'a vector
  #define ELEMENT   'a
  #include "Signature.frag.mli"
end

module Int  = Int
module Mono = Mono
module Poly = Poly