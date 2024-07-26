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

open ArraySignature

include MONOARRAY with type element = int

(* This module provides [alloc], but we do not include this function
   in the signature [MONOARRAY], because this signature describes the
   minimal requirements of our vector implementation: it serves as an
   input signature for the functor [Mono.OutOfArray]. *)

(**[alloc n d] returns a new array of length [n]. The dummy element [d]
   may be used to initialize this array, but this is not guaranteed.
   Thus, {b this array must be considered uninitialized}: every slot
   must be written before it is read. *)
val alloc : length -> dummy -> t
