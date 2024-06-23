module[@inline] Make (X : sig
  type t
  val make : int -> t -> t array
end) = struct

(* -------------------------------------------------------------------------- *)

module A = struct

  open Array

  (* To construct arrays, we do not use [Array.make]. Instead, we use the
     factory function provided the user, [X.make]. This allows the user
     to use exotic (possibly unorthodox) array construction methods. *)

  let make = X.make

  (* We implement [init] and [sub] using [make], so that [make] is our
     single factory function for arrays. We also re-implement [blit].
     This guarantees that [unsafe_set] is our sole way of writing an
     array. *)

  let init n f =
    assert (0 <= n);
    if n = 0 then [||] else
    let x = f 0 in
    let a = make n x in
    for i = 1 to n - 1 do
      unsafe_set a i (f i) (* safe *)
    done;
    a

  (* Validation ensures that our use of [unsafe_get] and [unsafe_set]
     is safe. *)

  let defensive = true

  let[@inline never] violation a o n =
    Printf.ksprintf invalid_arg
      "invalid offset/length pair (%d, %d) in an array of length %d"
      o n (length a)

  let[@inline] validate a o n =
    if defensive && not (0 <= n && 0 <= o && o + n <= length a) then
      violation a o n

  (* [sub a o n] is equivalent to [init n (fun i -> A.get a (o + i))]. *)

  let sub a o n =
    validate a o n;
    if n = 0 then [||] else
    let x = unsafe_get a o in (* safe *)
    let a' = make n x in
    for i = 1 to n - 1 do
      unsafe_set a' i (unsafe_get a (o + i)) (* safe *)
    done;
    a'

  (* We implement just a special case of [blit] where the two arrays
     are distinct. *)

  let[@inline] blit src sofs dst dofs n =
    assert (src != dst);
    validate src sofs n;
    validate dst dofs n;
    for i = 0 to n - 1 do
      unsafe_set dst (dofs + i) (unsafe_get src (sofs + i)) (* safe *)
    done

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
  mutable data     : element array;

}

(* -------------------------------------------------------------------------- *)

#include "Common.frag.ml"

end (* Make *)
