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

(** @inline *)
include MONOARRAY with type element = int

(* The following functions are not part of the signature [MONOARRAY] because
   they are not needed by Hector. Nevertheless, they can be useful to clients
   who wish to use [Hector.IntArray] directly. *)

(**[copy a] returns a new array whose length and content are those of
   the array [a]. *)
val copy : t -> t
