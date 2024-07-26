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

open Monolith

(* -------------------------------------------------------------------------- *)

(* A reference implementation of vectors. *)

module R = Reference

(* -------------------------------------------------------------------------- *)

(* A candidate implementation. *)

module C = Candidate

(* -------------------------------------------------------------------------- *)

(* A Monolith combinator for arrays. *)

let constructible_array spec =
  map_outof
    Array.of_list
    (Array.of_list, constant "Array.of_list")
    (list spec)

let deconstructible_array spec =
  map_into
    Array.to_list
    (Array.to_list, constant "Array.to_list")
    (list spec)

let array spec =
  ifpol
    (constructible_array spec)
    (deconstructible_array spec)

(* -------------------------------------------------------------------------- *)

(* We have one abstract type, namely [vector]. *)

let check _model =
  C.check, constant "check"

let vector =
  declare_abstract_type ~check ()

(* We draw random integer elements. *)

let element =
  semi_open_interval 0 32

(* We can produce and read sequences of elements. *)

(* [to_seq] and [to_seq_rev] produce sequences that are valid only as
   long as the vector is not mutated. This could be expressed, with
   some work, but it is not really worth the trouble. Instead, we
   immediately convert the sequence to a list. This is good enough. *)

let seq ?length:(length=Gen.lt 16) element =
  ifpol

    (* The construction side. *)
    begin
      list ~length element
      |> map_outof List.to_seq (List.to_seq, constant "List.to_seq")
    end

    (* The deconstruction side. *)
    begin
      list element
      |> map_into List.of_seq (List.of_seq, constant "List.of_seq")
    end

(* We draw random integer capacities and lengths. *)

let capacity =
  semi_open_interval 0 32

let length =
  semi_open_interval 0 32

(* We draw indices within the range of a vector. *)

let index v =
  semi_open_interval 0 (R.length v)

(* Exchanging arguments. *)

let flip f x y =
  f y x

(* We test [iter] by converting it to an [elements] function. *)

let elements_of_iter iter v =
  let xs = ref [] in
  iter (fun x -> xs := x :: !xs) v;
  List.rev !xs

let elements_of_iteri iteri v =
  let ixs = ref [] in
  iteri (fun i x -> ixs := (i, x) :: !ixs) v;
  List.rev !ixs

let elements_of_fold_left fold_left v =
  fold_left (fun xs x -> x :: xs) [] v
  |> List.rev

let elements_of_fold_right fold_right v =
  fold_right (fun x xs -> x :: xs) v []

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = vector ^> length in
  declare "length" spec R.length C.length;

  let spec = vector ^> bool in
  declare "is_empty" spec R.is_empty C.is_empty;

  let spec = unit ^> vector in
  declare "create" spec R.create C.create;

  let spec = length ^> element ^> vector in
  declare "make" spec R.make C.make;

  let spec = length ^> vector in
  declare "flip init Fun.id" spec
    (flip R.init Fun.id) (flip C.init Fun.id);

  let spec = vector ^> vector in
  declare "copy" spec R.copy C.copy;

  let spec = vector ^> array element in
  declare "to_array" spec R.to_array C.to_array;

  let spec = vector ^>> fun v -> index v ^> element in
  declare "get" spec R.get C.get;
  declare "unsafe_get" spec R.get C.unsafe_get;

  let spec = vector ^>> fun v -> index v ^> element ^> unit in
  declare "set" spec R.set C.set;
  declare "unsafe_set" spec R.set C.unsafe_set;

  let spec = vector ^> element ^> unit in
  declare "push" spec R.push C.push;

  let spec = vector ^> option element in
  declare "pop_opt" spec R.pop_opt C.pop_opt;

  let spec = vector ^!> element in
  declare "pop" spec R.pop C.pop;

  let spec = vector ^!> unit in
  declare "drop" spec R.drop C.drop;

  let spec = vector ^!> element in
  declare "top" spec R.top C.top;

  let spec = vector ^> option element in
  declare "top_opt" spec R.top_opt C.top_opt;

  let spec = vector ^> array element ^> unit in
  declare "push_array" spec R.push_array C.push_array;

  let spec =
    vector ^>
    array element ^>> fun a ->
    let n = Array.length a in
    closed_interval 0 n ^>> fun ofs ->
    closed_interval 0 (n - ofs) ^>
    unit
  in
  declare "push_array_segment" spec R.push_array_segment C.push_array_segment;

  let spec = vector ^> vector ^> unit in
  declare "push_vector" spec R.push_vector C.push_vector;

  let spec = vector ^> list element ^> unit in
  declare "push_list" spec R.push_list C.push_list;

  let spec = vector ^> seq element ^> unit in
  declare "push_seq" spec R.push_seq C.push_seq;

  (* [push_iter] is not tested. *)

  let spec = vector ^> length ^> unit in
  declare "truncate" spec R.truncate C.truncate;

  let spec = vector ^> unit in
  declare "clear" spec R.clear C.clear;
  declare "reset" spec R.reset C.reset;

  let spec = vector ^> capacity ^> unit in
  declare "ensure_capacity" spec R.ensure_capacity C.ensure_capacity;
  declare "ensure_extra_capacity" spec
    R.ensure_extra_capacity C.ensure_extra_capacity;
  declare "set_capacity" spec R.set_capacity C.set_capacity;

  let spec = vector ^> unit in
  declare "fit_capacity" spec R.fit_capacity C.fit_capacity;

  let spec = vector ^> list element in
  declare "elements_of_iter iter" spec
    (elements_of_iter R.iter) (elements_of_iter C.iter);

  let spec = vector ^> list (int *** element) in
  declare "elements_of_iteri iteri" spec
    (elements_of_iteri R.iteri) (elements_of_iteri C.iteri);

  (* [map] is applied specifically to the function [succ]. *)
  (* We do not check that [map] calls [f x] at most once for each [x]
     and from left to right. *)
  let spec = vector ^> vector in
  declare "map succ" spec (R.map succ) (C.map succ);

  (* [mapi] is applied specifically to the function [(+)]. *)
  (* We do not check that [mapi] calls [f x] at most once for each [x]
     and from left to right. *)
  let spec = vector ^> vector in
  declare "mapi (+)" spec (R.mapi (+)) (C.mapi (+));

  let spec = vector ^> list element in
  declare "elements_of_fold_left fold_left" spec
    (elements_of_fold_left R.fold_left) (elements_of_fold_left C.fold_left);

  let spec = vector ^> list element in
  declare "elements_of_fold_right fold_right" spec
    (elements_of_fold_right R.fold_right) (elements_of_fold_right C.fold_right);

  (* [exists] is applied specifically to the function [(<=) 0]. *)
  let spec = vector ^> bool in
  declare "exists" spec (R.exists ((<=) 0)) (C.exists ((<=) 0));

  (* [for_all] is applied specifically to the function [(<=) 0]. *)
  let spec = vector ^> bool in
  declare "for_all" spec (R.for_all ((<=) 0)) (C.for_all ((<=) 0));

  (* [filter] is applied specifically to the function [(<=) 0]. *)
  let spec = vector ^> vector in
  declare "filter" spec (R.filter ((<=) 0)) (C.filter ((<=) 0));

  (* [filter_map] is applied specifically to the function
     [increment_if_positive]. *)
  let spec = vector ^> vector in
  let increment_if_positive x = if x < 0 then None else Some (x + 1) in
  declare "filter_map increment_if_positive" spec
    (R.filter_map increment_if_positive) (C.filter_map increment_if_positive);

  (* [equal] is applied specifically to the function [(=)]. *)
  let spec = vector ^> vector ^> bool in
  declare "equal (=)" spec (R.equal (=)) (C.equal (=));

  (* [equal] is applied specifically to the function [Int.compare]. *)
  let spec = vector ^> vector ^> int in
  declare "compare Int.compare" spec
    (R.compare Int.compare) (C.compare Int.compare);

  let spec = array element ^> vector in
  declare "of_array" spec R.of_array C.of_array;

  let spec = list element ^> vector in
  declare "of_list" spec R.of_list C.of_list;

  let spec = vector ^> list element in
  declare "to_list" spec R.to_list C.to_list;

  let spec = seq element ^> vector in
  declare "of_seq" spec R.of_seq C.of_seq;

  let spec = vector ^> seq element in
  declare "to_seq" spec R.to_seq C.to_seq;

  let spec = vector ^> seq element in
  declare "to_seq_rev" spec R.to_seq_rev C.to_seq_rev;

  (* [find] is applied specifically to the function [(<=) 0]. *)
  let spec = vector ^!> int in
  declare "find ((<=) 0)" spec (R.find ((<=) 0)) (C.find ((<=) 0));

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  dprintf "          let elements_of_iter iter v = let xs = ref [] in iter (fun x -> xs := x :: !xs) v; List.rev !xs;;\n";
  dprintf "          let elements_of_iteri iteri v = let ixs = ref [] in iteri (fun i x -> ixs := (i, x) :: !ixs) v; List.rev !ixs;;\n";
  dprintf "          let elements_of_fold_left fold_left v = fold_left (fun xs x -> x :: xs) [] v |> List.rev;;\n";
  dprintf "          let elements_of_fold_right fold_right v = fold_right (fun x xs -> x :: xs) v [];;\n";
  dprintf "          let increment_if_positive x = if x < 0 then None else Some (x + 1);;\n";
  let fuel = 128 in
  main fuel
