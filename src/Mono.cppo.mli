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

#define VECTOR  vector
#define SYNONYM t
#define ELEMENT element

(**The functor [Make_] lets the user choose the type [element] as well as
   the array allocation function [alloc]. The function call [alloc n x] is
   expected to allocate an array of size [n], and is not required to
   initialize this array. (The parameter [x] may be unused.) *)
module Make_ (X : sig
  type t
  val alloc : int -> t -> t array
  val make  : int -> t -> t array
end) : sig
  type element = X.t
  #include "Signature.frag.mli"
end

(**The functor [Make] lets the user choose the type [element].
   The function [Array.make] is used to allocate arrays. *)
module Make (X : sig
  type t
end) : sig
  type element = X.t
  #include "Signature.frag.mli"
end
