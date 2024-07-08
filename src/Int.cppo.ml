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

(* [unsafe_fill_bytes b o n c] fills the buffer [b], at offset [o], with [n]
   copies of the character [c]. It is implemented in runtime/str.c as a call
   to [memset]. *)

external unsafe_fill_bytes :
  Bytes.t ->
  (* offset in bytes: *) int  ->
  (* length in bytes: *) int  ->
  (* value:           *) char ->
  unit = "caml_fill_bytes"

(* [unsafe_initialize_int_array a n] initializes the integer array [a],
   whose length is [n], with arbitrary (valid) integer values. *)

let unsafe_initialize_int_array (a : int array) (n : int) =
  (* Compute a length in bytes. *)
  let n = n * (Sys.word_size / 8) in
  (* Fill the array with odd bytes, which are valid integers. *)
  unsafe_fill_bytes (Obj.magic a) 0 n '\001'

(* -------------------------------------------------------------------------- *)

(* The following code is functionally equivalent to an application of the
   functor [Mono.Make_] to the module [X] below. However, by forcing the
   compiler to recompile this code with the knowledge that the type [t] is
   [int], we get better machine code. In particular, when reading and writing
   arrays, the special case of floating-point arrays (tagged 254) disappears;
   and when writing arrays, the write barrier (_caml_modify) vanishes. *)

(* Instead of using [Array.make], we use an unorthodox method to allocate a
   custom block, which the garbage collector does not scan, and disguise it
   as an integer array. The arrays slots are uninitialized; they may contain
   arbitrary data. *)

module X = struct

  type t = int

  let alloc n (_x : int) =
    (* Allocate an uninitialized memory block, which the GC does not scan. *)
    let a = Obj.new_block Obj.abstract_tag n in
    (* Cast it to the type [int array]. *)
    let a : int array = Obj.obj a in
    (* Initialize it. *)
    (* We cannot use [Array.fill], as it can (in some circumstances) read and
       interpret the previous content of the array. A simple loop would work,
       but would be a bit slow (not vectorized; with a safe point). [memset]
       is faster. *)
    unsafe_initialize_int_array a n;
    (* Done. *)
    a

  let make n (x : int) =
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

end

#define USE_MEMCPY 1

#include "MonoBody.frag.ml"
