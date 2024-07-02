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

(* A reference implementation of vectors. *)

type 'a vector =
  'a list ref

let length v =
  List.length !v

let is_empty v =
  !v = []

let create () =
  ref []

let init n f =
  ref (List.init n f)

let copy v =
  ref !v

let elements v =
  Array.of_list !v

let get v i =
  List.nth !v i

let rec update xs i x' =
  match xs, i with
  | [], _ ->
      assert false
  | _ :: xs, 0 ->
      x' :: xs
  | x :: xs, _ ->
      x :: update xs (i-1) x'

let set v i x =
  v := update !v i x

let push v x =
  v := !v @ [x]

let pop_opt v =
  match List.rev !v with
  | x :: xs ->
      v := List.rev xs;
      Some x
  | [] ->
      None

let pop v =
  match List.rev !v with
  | x :: xs ->
      v := List.rev xs;
      x
  | [] ->
      raise Not_found

let drop v =
  match pop v with
  | _
  | exception Not_found ->
      ()

let rec take n xs =
  if n = 0 then []
  else
    match xs with
    | [] ->
        []
    | x :: xs ->
        x :: take (n-1) xs

let truncate v n =
  v := take n !v

let clear v =
  v := []

let reset =
  clear

let ensure_capacity _v _capacity =
  ()

let ensure_extra_capacity _v _delta =
  ()

let fit_capacity _v =
  ()

let set_capacity =
  truncate

let iter f v =
  List.iter f !v

let iteri f v =
  List.iteri f !v

let rec index f xs i =
  match xs with
  | [] ->
      raise Not_found
  | x :: xs ->
      if f x then i else index f xs (i+1)

let find f v =
  index f !v 0
