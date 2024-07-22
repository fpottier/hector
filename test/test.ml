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

module R = Reference

(* Choose one of the following candidate implementations. *)

module CPoly = Hector.Poly

module CMono = Hector.Mono.Make(struct type t = int end)

module CInt  = Hector.Int

module C = CInt

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
  declare "elements" spec R.elements C.elements;

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
  declare "peek" spec R.peek C.peek;

  let spec = vector ^> option element in
  declare "peek_opt" spec R.peek_opt C.peek_opt;

  let spec = vector ^> array element ^> unit in
  declare "push_array" spec R.push_array C.push_array;

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

  (* [find] is applied specifically to the function [(<=) 8]. *)
  let spec = vector ^!> int in
  declare "find ((<=) 0)" spec (R.find ((<=) 0)) (C.find ((<=) 0));

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let prologue () =
    dprintf "          open Hector.Int;;\n";
    dprintf "          let elements_of_iter iter v = let xs = ref [] in iter (fun x -> xs := x :: !xs) v; List.rev !xs;;\n";
    dprintf "          let elements_of_iteri iteri v = let ixs = ref [] in iteri (fun i x -> ixs := (i, x) :: !ixs) v; List.rev !ixs;;\n";
    ()
  in
  let fuel = 128 in
  main ~prologue fuel
