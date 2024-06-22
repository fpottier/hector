module A = Array

(* -------------------------------------------------------------------------- *)

(* Types. *)

type length = int
type capacity = int
type index = int

(* In [make] and in [set_higher_capacity], the allocation of an array of
   size [capacity] is delayed, because we do not have a value of type ['a]
   at hand. *)

type 'a vector = {

  (* The logical length of the vector. *)
  mutable length   : int;

  (* The desired physical capacity of the vector. We impose the invariant
     [length = 0 || A.length data = capacity]. That is, unless the vector
     is logically empty, [capacity] is the length of the [data] array. *)
  mutable capacity : int;

  (* The data array. *)
  mutable data     : 'a A.t;

}

(* -------------------------------------------------------------------------- *)

#include "Common.frag.ml"