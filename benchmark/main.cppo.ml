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

open Printf

module B = Common.Benchmark
let run benchmarks =
  List.iter B.drive_and_display benchmarks

let quota =
  "5.0s"

(* -------------------------------------------------------------------------- *)

(* The implementations that we wish to benchmark. *)

module R = Dynarray
module P = Hector.Poly
module M = Hector.Mono.Make(struct type t = int let make = Array.make end)
module I = Hector.Int

(* -------------------------------------------------------------------------- *)

(* Push. *)

(* Each benchmark is defined as a macro (not a higher-order function)
   because we want to benchmark realistic client code, where calls to
   library functions are inlined (when possible). *)

#define PUSH(candidate,create,push,n) \
( \
  let basis = n \
  and name = sprintf "push (size %d) (%s)" n candidate \
  and run () () = \
    let v = create () in \
    for i = 0 to n-1 do \
      let dummy = i in \
      push v dummy \
    done \
  in \
  B.benchmark ~name ~quota ~basis ~run \
)

let pushes n =
  [
    PUSH("dynarray", R.create, R.add_last, n);
    PUSH("poly", P.create, P.push, n);
    PUSH("mono", M.create, M.push, n);
    PUSH("int", I.create, I.push, n);
  ]

(* -------------------------------------------------------------------------- *)

(* Get. *)

#define GET(candidate, create, push, get, n) \
( \
  let basis = n \
  and name = sprintf "get (size %d) (%s)" n candidate \
  and run () = \
    (* Initialization: *) \
    let v = create () in \
    for i = 0 to n-1 do \
      let dummy = i in \
      push v dummy \
    done; \
    fun () -> \
      (* Benchmark: *) \
      let sum = ref 0 in \
      for i = 0 to n-1 do \
        sum := !sum + get v i \
      done \
  in \
  B.benchmark ~name ~quota ~basis ~run \
)

let gets n =
  [
    GET("dynarray", R.create, R.add_last, R.get, n);
    GET("poly", P.create, P.add_last, P.get, n);
    GET("mono", M.create, M.add_last, M.get, n);
    GET("int", I.create, I.add_last, I.get, n);
    GET("dynarray/unsafe", R.create, R.add_last, R.unsafe_get, n);
    GET("poly/unsafe", P.create, P.add_last, P.unsafe_get, n);
    GET("mono/unsafe", M.create, M.add_last, M.unsafe_get, n);
    GET("int/unsafe", I.create, I.add_last, I.unsafe_get, n);
  ]

(* -------------------------------------------------------------------------- *)

(* Set. *)

(* We manually unroll the main loop in this benchmark, because (only in the
   case of int/unsafe) we otherwise run into the slow-memory-barrier issue
   with Apple processors, which appears to slow down this loop by a factor
   of 100x. *)

#define SET(candidate, create, push, set, n) \
( \
  let basis = n \
  and name = sprintf "set (size %d) (%s)" n candidate \
  and run () = \
    (* Initialization: *) \
    let v = create () in \
    for i = 0 to n-1 do \
      let dummy = i in \
      push v dummy \
    done; \
    fun () -> \
      (* Benchmark: *) \
      let i = ref 0 in \
      while !i + 5 <= n do \
        (let i = !i + 0 in set v i (2 * i)); \
        (let i = !i + 1 in set v i (2 * i)); \
        (let i = !i + 2 in set v i (2 * i)); \
        (let i = !i + 3 in set v i (2 * i)); \
        (let i = !i + 4 in set v i (2 * i)); \
        i := !i + 5 \
      done; \
      while !i < n do \
        (let i = !i + 0 in set v i (2 * i)); \
        i := !i + 1 \
      done \
  in \
  B.benchmark ~name ~quota ~basis ~run \
)

let sets n =
  [
    SET("dynarray", R.create, R.add_last, R.set, n);
    SET("poly", P.create, P.add_last, P.set, n);
    SET("mono", M.create, M.add_last, M.set, n);
    SET("int", I.create, I.add_last, I.set, n);
    SET("dynarray/unsafe", R.create, R.add_last, R.unsafe_set, n);
    SET("poly/unsafe", P.create, P.add_last, P.unsafe_set, n);
    SET("mono/unsafe", M.create, M.add_last, M.unsafe_set, n);
    SET("int/unsafe", I.create, I.add_last, I.unsafe_set, n);
  ]

(* -------------------------------------------------------------------------- *)

(* Iter. *)

(* We use [iter] to implement a summation. *)

#define ITER(candidate, create, push, iter, n) \
( \
  let basis = n \
  and name = sprintf "iter (size %d) (%s)" n candidate \
  and run () = \
    (* Initialization: *) \
    let v = create () in \
    for i = 0 to n-1 do \
      push v i \
    done; \
    fun () -> \
      (* Benchmark: *) \
      let sum = ref 0 in \
      iter (fun x -> sum := !sum + x) v \
  in \
  B.benchmark ~name ~quota ~basis ~run \
)

let iters n =
  [
    ITER("dynarray", R.create, R.add_last, R.iter, n);
    ITER("poly", P.create, P.add_last, P.iter, n);
    ITER("mono", M.create, M.add_last, M.iter, n);
    ITER("int", I.create, I.add_last, I.iter, n);
  ]

(* -------------------------------------------------------------------------- *)

(* Read the command line. *)

let push, get, set, iter =
  ref 0, ref 0, ref 0, ref 0

let () =
  Arg.parse [
    "--push", Arg.Set_int push, " <n> Benchmark push";
    "--get", Arg.Set_int get, " <n> Benchmark get";
    "--set", Arg.Set_int set, " <n> Benchmark set";
    "--iter", Arg.Set_int iter, " <n> Benchmark iter";
  ] (fun _ -> ()) "Invalid usage"

let push, get, set, iter =
  !push, !get, !set, !iter

let possibly n (benchmarks : int -> B.benchmark list) =
  if n > 0 then run (benchmarks n)

(* -------------------------------------------------------------------------- *)

(* Main. *)

let () =
  possibly push pushes;
  possibly get gets;
  possibly set sets;
  possibly iter iters;
  ()
