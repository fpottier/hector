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

let make n x =
  ref (List.init n (fun _i -> x))

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

let push_list v xs =
  v := !v @ xs

let push_array v xs =
  push_list v (Array.to_list xs)

let push_array_segment v xs ofs len =
  push_array v (Array.sub xs ofs len)

let push_vector v v' =
  push_list v !v'

let rec push_seq v xs =
  match xs() with
  | Seq.Nil ->
      ()
  | Seq.Cons (x, xs) ->
      push v x;
      push_seq v xs

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

let peek v =
  let xs = !v in
  let n = List.length xs in
  if n > 0 then
    List.nth xs (n - 1)
  else
    raise Not_found

let peek_opt v =
  let xs = !v in
  let n = List.length xs in
  if n > 0 then
    Some (List.nth xs (n - 1))
  else
    None

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

let map f v =
  ref (List.map f !v)

let mapi f v =
  ref (List.mapi f !v)

let fold_left f accu v =
  List.fold_left f accu !v

let fold_right f v accu =
  List.fold_right f !v accu

let exists f v =
  List.exists f !v

let for_all f v =
  List.for_all f !v

let filter f v =
  ref (List.filter f !v)

let rec index f xs i =
  match xs with
  | [] ->
      raise Not_found
  | x :: xs ->
      if f x then i else index f xs (i+1)

let find f v =
  index f !v 0
