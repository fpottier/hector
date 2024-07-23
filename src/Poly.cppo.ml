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

module A = struct
  include Array
  let alloc = make
  let blit_disjoint = blit
end

#define VECTOR  'a vector
#define ARRAY   'a A.t
#define SYNONYM 'a t
#include "Loop.frag.ml"
#include "Common.frag.ml"
