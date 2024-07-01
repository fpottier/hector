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
      for i = 0 to n-1 do \
        let dummy = 2 * i in \
        set v i dummy \
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

(* This benchmark allocates a lot of memory in the presence of a large vector.
   We test whether using an unscanned vector (Hector.Int) or a scanned vector
   (Dynarray, Hector.Poly) makes a difference. Apparently, it does not. *)

let allocate_a_lot n =
  Array.init n @@ fun k ->
  List.init n @@ fun j ->
  k * j

#define SCAN(candidate, create, push, length, n) \
( \
  let basis = 1 \
  and name = sprintf "scan (size %d) (%s)" n candidate \
  and run () = \
    (* Create an inert vector of size [n*n]: *) \
    let v = create () in \
    let size = n*n in \
    for i = 0 to size-1 do \
      let dummy = i in \
      push v dummy \
    done; \
    fun () -> \
      (* Allocate [3k*n*n] words: *) \
      let k = 10 in \
      for _ = 1 to k do \
        allocate_a_lot n \
        |> Sys.opaque_identity |> ignore \
      done; \
      (* Do something to ensure that [v] is live: *) \
      ignore (length v) \
  in \
  B.benchmark ~name ~quota ~basis ~run \
)

module N = struct
  let create () = ()
  let add_last () _x = ()
  let length () = 0
end

let scans n =
  [
    SCAN("none", N.create, N.add_last, N.length, n);
    SCAN("dynarray", R.create, R.add_last, R.length, n);
    SCAN("poly", P.create, P.add_last, P.length, n);
    SCAN("int", I.create, I.add_last, I.length, n);
  ]

(* -------------------------------------------------------------------------- *)

(* Read the command line. *)

let push, get, set, scan =
  ref 0, ref 0, ref 0, ref 0

let () =
  Arg.parse [
    "--push", Arg.Set_int push, " <n> Benchmark push";
    "--get", Arg.Set_int get, " <n> Benchmark get";
    "--set", Arg.Set_int set, " <n> Benchmark set";
    "--scan", Arg.Set_int scan, " <n> Measure the cost of GC scans";
  ] (fun _ -> ()) "Invalid usage"

let push, get, set, scan =
  !push, !get, !set, !scan

let possibly n (benchmarks : int -> B.benchmark list) =
  if n > 0 then run (benchmarks n)

(* -------------------------------------------------------------------------- *)

(* Main. *)

let () =
  possibly push pushes;
  possibly get gets;
  possibly set sets;
  possibly scan scans;
  ()
