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

module Make (X : sig
  type t
  val make : int -> t -> t array
end) : sig

  type element = X.t

  #define VECTOR vector
  #define ELEMENT   element

  #include "Signature.frag.mli"

end
