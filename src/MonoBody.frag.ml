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

(* -------------------------------------------------------------------------- *)

(* Types. *)

type element = X.t

type length = int
type capacity = int
type index = int

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

(* We implement our own functions on arrays, so that [alloc], [unsafe_get],
   and [unsafe_set] are the only primitive operations on which we rely. *)

(* We add type annotations, where necessary, to ensure that our code is
   monomorphic. Thus, if the type [element] is [int], we avoid the test
   for float arrays, and we avoid the write barrier (_caml_modify). *)

module A = struct

  open Array

  (* To construct arrays, we use the factory functions provided the user,
     [X.alloc] and [X.make]. This allows the user to provide exotic (and
     possibly unorthodox) array construction methods. *)

  let alloc = X.alloc
  let make  = X.make

  (* We implement [init] and [sub] using [alloc], so that [alloc] is our
     single factory function for arrays. *)

  let init n f : element array =
    assert (0 <= n);
    if n = 0 then [||] else
    let x = f 0 in
    let a = alloc n x in
    unsafe_set a 0 x; (* safe *)
    LOOP5(i, 1, n, unsafe_set a i (f i) (* safe *));
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

  (* We implement just the special case of [blit] where there is no
     overlap between the source and destination. Thus, in C, [memcpy]
     can be used; there is no need for [memmove]. *)

  #ifdef USE_MEMCPY

  (* If the type [element] is immediate (i.e., not a pointer type)
     then [memcpy] can be used. *)

  (* In the case of integer arrays, [memcpy] can be 4 times faster than
     hand-written loop, and 12 times faster than [Array.blit]. *)

  external unsafe_blit :
    int array -> int ->
    int array -> int ->
    int ->
    unit
  = "hector_array_blit"

  #else

  (* I have hesitated between using [Array.blit] and re-implementing it. One
     advantage of re-implementing it might be that [unsafe_set] is then our
     sole way of writing an array, so we can safely use truly initialized
     arrays (where an uninitialized slot contains arbitrary bits). But this
     seems too dangerous anyway. *)

  (* In the case of integer arrays, a loop can be 3 times faster than
     [Array.blit]. In the case of polymorphic arrays, a loop can be 30%
     slower than [Array.blit]. So, here, a loop seems advantageous if
     the code is specialized for a known immediate type; otherwise,
     using [Array.blit] is advantageous. *)

  (* The parallel loop LOOPRW5 is not noticeably faster than the ordinary
     loop LOOP5. *)

  let[@inline] unsafe_blit (src : element array) sofs dst dofs n =
   (* LOOP5(j, 0, n, unsafe_set dst (dofs + j) (unsafe_get src (sofs + j))) *)
   (* LOOPRW5(j, 0, n, x, unsafe_get src (sofs + j), unsafe_set dst (dofs + j) x) *)
   Array.blit src sofs dst dofs n

  #endif

  let blit (src : element array) sofs dst dofs n =
    validate src sofs n;
    validate dst dofs n;
    assert (src != dst || sofs + n <= dofs || dofs + n <= sofs);
    unsafe_blit src sofs dst dofs n

  (* [sub a o n] is equivalent to [init n (fun i -> A.get a (o + i))]. *)

  let sub a o n : element array =
    validate a o n;
    if n = 0 then [||] else
    let dummy = unsafe_get a o in (* safe *)
    let a' = alloc n dummy in
    blit a o a' 0 n;
    a'

end

(* -------------------------------------------------------------------------- *)

#define ELEMENT element
#include "Common.frag.ml"

(* -------------------------------------------------------------------------- *)
