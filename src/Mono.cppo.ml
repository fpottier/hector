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

module[@inline] Make_ (X : sig
  type t
  val make : int -> t -> t array
end) = struct

  #include "MonoBody.frag.ml"

end (* Make_ *)

module[@inline] Make (X : sig type t end) =
  Make_(struct include X let make = Array.make end)
