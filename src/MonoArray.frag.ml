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

(* This file implements several functions on arrays, based on [alloc],
   [unsafe_get], and [unsafe_set]. We also need the empty array, [empty]. *)

(* We ensure that [alloc] is the only function that constructs arrays.
   This allows the user to provide exotic (and possibly unorthodox)
   array construction methods. *)

(* -------------------------------------------------------------------------- *)

(* [length], [unsafe_get], [unsafe_set] are taken from [Stdlib.Array]. *)

(* Type annotations ensure that we perform monomorphic
   array accesses only. (These produce better code.) *)

let length =
  Array.length

let[@inline] unsafe_get (a : t) i =
  Array.unsafe_get a i

let[@inline] unsafe_set (a : t) i x =
  Array.unsafe_set a i x

(* -------------------------------------------------------------------------- *)

(* Validation ensures that our use of [unsafe_get] and [unsafe_set]
   is safe. *)

let[@inline never] violation a ofs len =
  Printf.ksprintf invalid_arg
    "invalid segment (ofs = %d, len = %d) in a sequence of length %d"
    ofs len (length a)

let[@inline] validate a ofs len =
  if not (0 <= len && 0 <= ofs && ofs + len <= length a) then
    violation a ofs len

(* -------------------------------------------------------------------------- *)

(* [blit_disjoint] is the special case of [blit] where there is no overlap
   between the source and destination. In C, [memcpy] can be used; there is
   no need for [memmove]. *)

#ifdef USE_MEMCPY

(* If the type [element] is immediate (i.e., not a pointer type)
   then [memcpy] can be used. *)

(* In the case of integer arrays, [memcpy] can be 4 times faster than
   hand-written loop, and 12 times faster than [Array.blit]. *)

external unsafe_blit_disjoint :
  int array -> int ->
  int array -> int ->
  int ->
  unit
= "hector_array_blit_disjoint"

#else

(* Should we use [Array.blit], or re-implement it using a loop? *)

(* One advantage of re-implementing it might be that [unsafe_set] is
   then our sole way of writing an array, so we can safely use truly
   initialized arrays (where an uninitialized slot contains arbitrary
   bits). But this seems too dangerous anyway. We do not currently use
   uninitialized arrays. *)

(* In the case of integer arrays, a loop can be 3 times faster than
   [Array.blit]. In the case of polymorphic arrays, a loop can be 30%
   slower than [Array.blit]. So, a loop seems advantageous if the code
   is specialized for a known immediate type; otherwise, [Array.blit]
   is advantageous. *)

(* The parallel loop LOOPRW5 is not noticeably faster than the ordinary
   loop LOOP5. *)

(* Unfortunately, we do not know in which situation we are: this code
   is used as part of the functor [Mono.Make], which can be applied
   either to an immediate type, or to a pointer type. Furthermore,
   this code could be compiled either by the maintream OCaml compiler,
   which ignores [@inline] annotations on functors, or by the Flambda2
   compiler, which does specialize functors. *)

(* In the end, it seems preferable to always use [Array.blit]. *)

let[@inline] unsafe_blit_disjoint (src : t) sofs dst dofs n =
 (* LOOP5(j, 0, n, unsafe_set dst (dofs + j) (unsafe_get src (sofs + j))) *)
 (* LOOPRW5(j, 0, n, x, unsafe_get src (sofs + j), unsafe_set dst (dofs + j) x) *)
 Array.blit src sofs dst dofs n

#endif

let blit_disjoint (src : t) sofs dst dofs n =
  validate src sofs n;
  validate dst dofs n;
  assert (src != dst || sofs + n <= dofs || dofs + n <= sofs);
  unsafe_blit_disjoint src sofs dst dofs n

(* -------------------------------------------------------------------------- *)

(* We implement [init] and [sub] using [alloc], so that [alloc] is our
   single factory function for arrays. *)

let init n f =
  assert (0 <= n);
  if n = 0 then empty else
  let x = f 0 in
  let a = alloc n x in
  unsafe_set a 0 x; (* safe *)
  LOOP5(i, 1, n, unsafe_set a i (f i) (* safe *));
  a

(* [sub a o n] is equivalent to [init n (fun i -> A.get a (o + i))]. *)

let sub a o n =
  validate a o n;
  if n = 0 then empty else
  let dummy = unsafe_get a o in (* safe *)
  let a' = alloc n dummy in
  blit_disjoint a o a' 0 n;
  a'

(* -------------------------------------------------------------------------- *)

(* [fill] is taken from [Stdlib.Array]. *)

let[@inline] fill (a : t) o k x =
  Array.fill a o k x
