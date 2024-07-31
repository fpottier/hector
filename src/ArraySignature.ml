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

(**A minimal signature for monomorphic arrays. *)
module type MONOARRAY = sig

  (**The type of elements. *)
  type element

  (**The function [alloc] expects a dummy element. *)
  type dummy = element

  (**The type of arrays. *)
  type t = element array

  (**The length of an array is a nonnegative integer. *)
  type length = int

  (**An index into an array is an integer value in the semi-open
     interval {m [0,n)}, where {m n} is the length of the array. *)
  type index = int

  (**An offset into an array is an integer value in the closed interval
     {m [0,n]}, where {m n} is the length of the array. When an offset
     {m o} and a length {m k} are used in concert to designate an array
     segment, both {m o} and {m o+k} must be valid offsets. *)
  type offset = int

  (**The empty array. *)
  val empty : t

  (**[length a] returns the length of the array [a]. *)
  val length : t -> length

  (**[unsafe_get a i] returns the element found at index [i] in the array
     [a]. {b The index [i] must be valid}, or all hell may break loose. *)
  val unsafe_get : t -> index -> element

  (**[unsafe_set a i x] writes the value [x] at index [i] in the array
     [a]. {b The index [i] must be valid}, or all hell may break loose. *)
  val unsafe_set : t -> index -> element -> unit

  (**[alloc n d] returns a new array of length [n]. The dummy element [d]
     may be used to initialize this array, but this is not guaranteed.
     Thus, {b this array must be considered uninitialized}: every slot
     must be written before it is read. *)
  val alloc : length -> dummy -> t

  (**[make n x] returns a new array of length [n], where every slot contains
     the value [x]. *)
  val make : length -> element -> t

  (**[grow n d a k] returns a new array of length [n]. The lower segment of
     this array, determined by offset [0] and length [k], is initialized by
     copying data from array [a], at offset [0] and length [k]. The inequality
     [k <= n] must hold. {b The upper segment of this array, determined by
     offset [k] and length [n - k], must be considered uninitialized}: every
     slot must be written before it is read. *)
  val grow : length -> element -> t -> length -> t

  (**[init n f] returns a new array of length [n], where the slot at index
     [i] contains the value [f i]. *)
  val init : length -> (index -> element) -> t

  (**[sub a o k] returns a new array of length [k] whose content is the
     content of the array segment identified by array [a], offset [o],
     and length [k]. *)
  val sub : t -> offset -> length -> t

  (**[unsafe_blit a1 o1 a2 o2 k] copies the content of the array segment
     identified by array [a1], offset [o1], and length [k] into the array
     segment identified by array [a2], offset [o2], and length [k]. {b No
     bounds check is performed.} *)
  val unsafe_blit : t -> offset -> t -> offset -> length -> unit

  (**[blit a1 o1 a2 o2 k] copies the content of the array segment
     identified by array [a1], offset [o1], and length [k] into the array
     segment identified by array [a2], offset [o2], and length [k]. *)
  val blit : t -> offset -> t -> offset -> length -> unit

  (**[fill a o k x] fills the array segment identified by array [a],
     offset [o], and length [k] with the value [x]. *)
  val fill : t -> int -> int -> element -> unit

end
