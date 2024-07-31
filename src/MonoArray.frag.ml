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

(* [blit]. *)

(* All of our internal uses of [blit] fall in a special case where there is
   no overlap between the source and destination. Nevertheless, we implement
   [blit] in the general case, because a few extra checks cost nothing. In C,
   we use [memmove], which supports overlap, as opposed to [memcpy], which
   does not. *)

#ifdef IMMEDIATE

(* If the type [element] is immediate (i.e., not a pointer type) then
   [memmove] can be used. *)

(* In the case of integer arrays, [memmove] can be 4 times faster than
   hand-written loop, and 12 times faster than [Array.blit]. *)

external unsafe_blit :
  int array -> int ->
  int array -> int ->
  int ->
  unit
= "hector_array_blit"

let blit (src : t) sofs dst dofs n =
  validate_segment (length src) sofs n;
  validate_segment (length dst) dofs n;
  unsafe_blit src sofs dst dofs n

#else

(* We always use [Array.blit], as opposed to a hand-written loop, because this
   is easier, and because [Array.blit] can be more efficient. In particular,
   in [Array.blit], some tests (e.g., is this array young?) are hoisted out of
   the loop. *)

let[@inline] unsafe_blit (src : t) sofs dst dofs n =
 Array.blit src sofs dst dofs n

let blit =
  unsafe_blit

#endif

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
  validate_segment (length a) o n;
  if n = 0 then empty else
  let dummy = unsafe_get a o in (* safe *)
  let a' = alloc n dummy in
  blit a o a' 0 n;
  a'

(* -------------------------------------------------------------------------- *)

(* [fill] is taken from [Stdlib.Array]. *)

(* Because there is no word-sized variant of [memset] in C, we cannot
   implement [fill] in a more efficient way, even when IMMEDIATE is
   #defined. *)

let[@inline] fill (a : t) o k x =
  Array.fill a o k x
