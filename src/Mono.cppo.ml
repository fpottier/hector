module[@inline] Make (X : sig type t end) = struct

(* -------------------------------------------------------------------------- *)

module A = struct

  include Array

  let defensive = true

  (* We implement [init] and [sub] using [make], so that [make] is our
     single factory function for arrays. *)

  let init n f =
    assert (0 <= n);
    if n = 0 then [||] else
    let x = f 0 in
    let a = make n x in
    for i = 1 to n - 1 do
      unsafe_set a i (f i) (* safe *)
    done;
    a

  (* [sub a o n] is equivalent to [init n (fun i -> A.get a (o + i))]. *)

  let sub a o n =
    assert (0 <= n);
    if defensive && not (0 <= o && o + n <= length a) then
      Printf.ksprintf invalid_arg
        "invalid offset-length pair (%d, %d) in an array of length %d"
        o n (length a);
    if n = 0 then [||] else
    let x = unsafe_get a o in (* safe *)
    let a' = make n x in
    for i = 1 to n - 1 do
      unsafe_set a' i (unsafe_get a (o + i)) (* safe *)
    done;
    a'

end

(* -------------------------------------------------------------------------- *)

(* Types. *)

type element = X.t

type length = int
type capacity = int
type index = int

(* In [make] and in [set_higher_capacity], the allocation of an array of
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
  mutable data     : element A.t;

}

(* -------------------------------------------------------------------------- *)

#include "Common.frag.ml"

end (* Make *)
