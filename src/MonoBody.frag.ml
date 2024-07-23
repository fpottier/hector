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

(* This file contains the body of the functor [Mono.Make_]. *)

type element = X.t
type length = int
type capacity = int
type index = int

(* -------------------------------------------------------------------------- *)

module A = struct
  type t = element array
  let empty = [||]
  let alloc = X.alloc
  let make  = X.make
  #include "MonoArray.frag.ml"
end

(* -------------------------------------------------------------------------- *)

(* Types. *)

(* In [create] and in [set_higher_capacity], the allocation of an array of
   size [capacity] is delayed, because we do not have a value of type ['a]
   at hand. *)

type vector = {

  (* The logical length of the vector. *)
  mutable length   : int;

  (* The desired physical capacity of the vector. We impose the invariant
     [length = 0 || A.length data = capacity]. That is, unless the vector
     is logically empty, [capacity] is the length of the [data] array. *)
  mutable capacity : int;

  (* The data array. *)
  mutable data     : element array;

}

type t =
  vector

(* -------------------------------------------------------------------------- *)

#define ELEMENT element
#include "Common.frag.ml"

(* -------------------------------------------------------------------------- *)
