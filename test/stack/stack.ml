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

(* A reference implementation of stacks. *)

module R = struct
  include Stdlib.Stack
  (* [drop] appears only in OCaml 5.1, so we implement it. *)
  let drop v = ignore (pop v)
end

(* -------------------------------------------------------------------------- *)

(* The candidate implementation. *)

module C = Hector.Int.Stack

let () =
  dprintf "          open Hector.Int.Stack;;\n"

(* -------------------------------------------------------------------------- *)

(* Declare the correspondence between [R.Empty] and [C.Empty]. *)

let () =
  override_exn_eq (fun equal rexn cexn ->
    match rexn, cexn with
    | R.Empty, C.Empty ->
        true
    | _, _ ->
        equal rexn cexn
  )

(* -------------------------------------------------------------------------- *)

(* We have one abstract type, namely [stack]. *)

let stack =
  declare_abstract_type()

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

(* We test [iter] by converting it to an [elements] function. *)

let elements_of_iter iter v =
  let xs = ref [] in
  iter (fun x -> xs := x :: !xs) v;
  List.rev !xs

let elements_of_fold_left fold_left v =
  fold_left (fun xs x -> x :: xs) [] v
  |> List.rev

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = unit ^> stack in
  declare "create" spec R.create C.create;

  let spec = element ^> stack ^> unit in
  declare "push" spec R.push C.push;

  let spec = stack ^!> element in
  declare "pop" spec R.pop C.pop;

  let spec = stack ^> option element in
  declare "pop_opt" spec R.pop_opt C.pop_opt;

  let spec = stack ^!> unit in
  declare "drop" spec R.drop C.drop;

  let spec = stack ^!> element in
  declare "top" spec R.top C.top;

  let spec = stack ^> option element in
  declare "top_opt" spec R.top_opt C.top_opt;

  let spec = stack ^> unit in
  declare "clear" spec R.clear C.clear;

  let spec = stack ^> stack in
  declare "copy" spec R.copy C.copy;

  let spec = stack ^> bool in
  declare "is_empty" spec R.is_empty C.is_empty;

  let spec = stack ^> int in
  declare "length" spec R.length C.length;

  let spec = stack ^> list element in
  declare "elements_of_iter iter" spec
    (elements_of_iter R.iter) (elements_of_iter C.iter);

  let spec = stack ^> list element in
  declare "elements_of_fold_left fold" spec
    (elements_of_fold_left R.fold) (elements_of_fold_left C.fold);

  let spec = stack ^> seq element in
  declare "to_seq" spec R.to_seq C.to_seq;

  let spec = stack ^> seq element ^> unit in
  declare "add_seq" spec R.add_seq C.add_seq;

  let spec = seq element ^> stack in
  declare "of_seq" spec R.of_seq C.of_seq;

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  dprintf "          let elements_of_iter iter v = let xs = ref [] in iter (fun x -> xs := x :: !xs) v; List.rev !xs;;\n";
  dprintf "          let elements_of_fold_left fold_left v = fold_left (fun xs x -> x :: xs) [] v |> List.rev;;\n";
  let fuel = 128 in
  main fuel
