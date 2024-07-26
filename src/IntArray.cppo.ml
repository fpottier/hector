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

#include "Loop.frag.ml"

(* Our implementation of integer arrays is compiled by the OCaml compiler with
   the knowledge that elements have type [int]. As a result, we get better
   machine code. In particular, when reading and writing arrays, the special
   case of floating-point arrays (tagged 254) disappears; and when writing
   arrays, the write barrier (_caml_modify) vanishes. *)

(* -------------------------------------------------------------------------- *)

(* Types. *)

type element = int
type dummy = element
type t = element array
type length = int
type index = int
type offset = int

(* -------------------------------------------------------------------------- *)

(* [unsafe_fill_bytes b o n c] fills the buffer [b], at offset [o], with [n]
   copies of the character [c]. It is implemented in runtime/str.c as a call
   to [memset]. *)

external unsafe_fill_bytes :
  Bytes.t ->
  (* offset in bytes: *) int  ->
  (* length in bytes: *) int  ->
  (* value:           *) char ->
  unit = "caml_fill_bytes"

(* [unsafe_initialize_int_array_segment a o n] initializes the array segment
   determined by array [a], offset [o], and length [n], with arbitrary (valid)
   integer values. *)

let unsafe_initialize_int_array_segment (a : int array) (o : int) (n : int) =
  (* Translate offset and length into bytes. *)
  let o = o * (Sys.word_size / 8) in
  let n = n * (Sys.word_size / 8) in
  (* Fill the array with odd bytes, which are valid integers. *)
  unsafe_fill_bytes (Obj.magic a) o n '\001'

(* -------------------------------------------------------------------------- *)

(* Allocation: [empty], [alloc], and [make]. *)

let empty =
  [||]

(* Instead of [Array.make], we use an unorthodox method to allocate a custom
   block, which the garbage collector does not scan, and disguise it as an
   integer array. The arrays slots are uninitialized; they may contain
   arbitrary data. *)

let alloc (n : length) (_dummy : element) : t =
  assert (0 <= n);
  (* Allocate an uninitialized memory block, which the GC does not scan. *)
  let a = Obj.new_block Obj.abstract_tag n in
  (* Cast it to the type [int array]. *)
  let a : int array = Obj.obj a in
  (* Initialize it. *)
  (* We cannot use [Array.fill], as it can (in some circumstances) read and
     interpret the previous content of the array. A simple loop would work,
     but would be a bit slow (not vectorized; with a safe point). [memset]
     is faster. *)
  unsafe_initialize_int_array_segment a 0 n;
  (* Done. *)
  a

let make (n : length) (x : element) : t =
  assert (0 <= n);
  (* Allocate an uninitialized memory block, which the GC does not scan. *)
  let a = Obj.new_block Obj.abstract_tag n in
  (* Cast it to the type [int array]. *)
  let a : int array = Obj.obj a in
  (* Initialize it. *)
  (* As above, we cannot use [Array.fill]. There is no [memset64] in C.
     So, we use a loop. *)
  LOOP5(i, 0, n, Array.unsafe_set a i x (* safe *));
  (* Done. *)
  a

(* -------------------------------------------------------------------------- *)

(* Then comes our implementation of monomorphic arrays. *)

#define USE_MEMCPY

#include "MonoArray.frag.ml"

(* -------------------------------------------------------------------------- *)

(* [grow] allocates a semi-initialized array. *)

(* Of course, [grow] can always be implemented by a combination of [alloc]
   and [blit_disjoint]. Our implementation (below) can in principle be more
   efficient, as the lower segment is written just once, instead of twice.
   In practice, we observe that it yields only a 1% performance improvement
   on the [push] benchmark. *)

let grow (n : length) (_dummy : element) (s : t) (k : length) : t =
  assert (0 <= k && k <= n);
  (* Allocate an uninitialized memory block, which the GC does not scan. *)
  let a = Obj.new_block Obj.abstract_tag n in
  (* Cast it to the type [int array]. *)
  let a : int array = Obj.obj a in
  (* Initialize the lower segment by copying data from [s]. *)
  blit_disjoint s 0 a 0 k;
  (* Initialize the upper segment with arbitrary integer values. *)
  unsafe_initialize_int_array_segment a k (n - k);
  (* Done. *)
  a
