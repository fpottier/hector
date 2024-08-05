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

(* -------------------------------------------------------------------------- *)

(**[validate_segment n ofs len] checks that the offset [ofs] and the
   length [len] determine a valid interval inside an array or vector of
   length [n]. *)

let[@inline never] invalid_segment n ofs len =
  Printf.ksprintf invalid_arg
    "invalid segment (ofs = %d, len = %d) in a sequence of length %d"
    ofs len n

let[@inline] validate_segment n ofs len =
  if not (0 <= len && 0 <= ofs && ofs + len <= n) then
    invalid_segment n ofs len
