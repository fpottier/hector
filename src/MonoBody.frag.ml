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

type element = X.t

(* -------------------------------------------------------------------------- *)

module A = struct
  type t = element array
  let empty = [||]
  let alloc = X.alloc
  let make  = X.make
  #include "MonoArray.frag.ml"
end

(* -------------------------------------------------------------------------- *)

#define VECTOR  vector
#define ARRAY   A.t
#define SYNONYM t
#include "Common.frag.ml"

(* -------------------------------------------------------------------------- *)
