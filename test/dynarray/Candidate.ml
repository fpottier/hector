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

include Dynarray

(* [Dynarray.append] cannot be applied twice to the same vector.
   We must compensate for that. *)
let append v1 v2 =
  append v1 (copy v2)

(* [Dynarray.get_last] cannot be applied to an empty vector.
   We must compensate for that. *)
let get_last v =
  if is_empty v then raise Not_found else get_last v

(* [Dynarray.compare] does not have the same semantics as our [compare].
   We must compensate for that. *)
let compare compare v1 v2 =
  List.compare compare (to_list v1) (to_list v2)

(* The following functions do not exist in [Dynarray], so they must be
   implemented here. *)
let check _v = ()
let push_array_segment v a o k =
  append_array v (Array.sub a o k)
let find f v =
  let rec loop i =
    if i = length v then raise Not_found
    else if f (get v i) then i
    else loop (i+1)
  in loop 0

(* Some operations have different names in [Dynarray], and we wish
   to use our preferred names in the test that follows, so we must
   compensate. *)

let push = add_last
let push_list = append_list
let push_array = append_array
let push_vector = append
let push_seq = append_seq
let pop_opt = pop_last_opt
let pop = pop_last
let top = get_last
let top_opt = find_last
let drop = remove_last

(* The following operations do not exist in [Dynarray]. *)

(* We provide vanilla implementations. *)

let concat vs =
  vs
  |> List.map to_array
  |> Array.concat
  |> of_array

let sub v ofs len =
  v
  |> to_array
  |> (fun a -> Array.sub a ofs len)
  |> of_array

let fill v ofs len x =
  for i = ofs to ofs + len - 1 do
    set v i x
  done

let iter_down f v =
  for i = length v - 1 downto 0 do
    f (get v i)
  done

(* -------------------------------------------------------------------------- *)

open Monolith

let () =
  dprintf "          open Dynarray;; (* not quite appropriate; we patch some operations *)\n"
