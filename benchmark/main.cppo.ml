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
module M = Hector.Mono.Make(struct type t = int end)
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

let[@inline] stack_push x s =
  Stack.push s x

let pushes n =
  [
    PUSH("dynarray", R.create, R.add_last, n);
    PUSH("poly", P.create, P.push, n);
    PUSH("mono", M.create, M.push, n);
    PUSH("int", I.create, I.push, n);
    PUSH("stack", Stack.create, stack_push, n);
  ]

(* -------------------------------------------------------------------------- *)

(* Pop. *)

#define POP(candidate,init,pop,n) \
( \
  let basis = n \
  and name = sprintf "pop (size %d) (%s)" n candidate \
  and run () () = \
    let v = init n (fun i -> i) in \
    let s = ref 0 in \
    for _ = 0 to n-1 do \
      s := !s + pop v \
    done; \
    assert (!s = n * (n - 1) / 2) \
  in \
  B.benchmark ~name ~quota ~basis ~run \
)

let stack_init n f =
  let s = Stack.create() in
  for i = 0 to n-1 do
    Stack.push (f i) s
  done;
  s

let pops n =
  [
    POP("dynarray", R.init, R.pop_last, n);
    POP("poly", P.init, P.pop, n);
    POP("mono", M.init, M.pop, n);
    POP("int", I.init, I.pop, n);
    POP("stack", stack_init, Stack.pop, n);
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

(* Iteri. *)

(* We use [iteri] and [set] to implement a blit. *)

#define ITERI(candidate, create, push, iteri, set, n) \
( \
  let basis = n \
  and name = sprintf "iteri/set (size %d) (%s)" n candidate \
  and run () = \
    (* Initialization: *) \
    let src = create () in \
    for i = 0 to n-1 do \
      push src i \
    done; \
    let dst = create () in \
    for _ = 0 to n-1 do \
      push dst 0 \
    done; \
    fun () -> \
      (* Benchmark: *) \
      iteri (fun i x -> set dst i x) src \
  in \
  B.benchmark ~name ~quota ~basis ~run \
)

let iteris n =
  [
    ITERI("dynarray", R.create, R.add_last, R.iteri, R.set, n);
    ITERI("poly", P.create, P.add_last, P.iteri, P.set, n);
    ITERI("mono", M.create, M.add_last, M.iteri, M.set, n);
    ITERI("int", I.create, I.add_last, I.iteri, I.set, n);
    ITERI("dynarray/unsafe", R.create, R.add_last, R.iteri, R.unsafe_set, n);
    ITERI("poly/unsafe", P.create, P.add_last, P.iteri, P.unsafe_set, n);
    ITERI("mono/unsafe", M.create, M.add_last, M.iteri, M.unsafe_set, n);
    ITERI("int/unsafe", I.create, I.add_last, I.iteri, I.unsafe_set, n);
  ]

(* -------------------------------------------------------------------------- *)

(* Push by chunks. *)

#define PUSHA(candidate,create,push_array,n,k) \
( \
  let basis = n \
  and name = sprintf "push_array (size %d, chunk size %d) (%s)" n k candidate \
  and run () = \
    let a = Array.init k (fun i -> i) in \
    fun () -> \
    let v = create () in \
    for _ = 0 to n-1 do \
      push_array v a \
    done \
  in \
  B.benchmark ~name ~quota ~basis ~run \
)

let pushas k n =
  let[@inline] naive_push_array v a =
    Array.iter (I.push v) a
  in
  [
    PUSHA("dynarray", R.create, R.append_array, n, k);
    PUSHA("poly", P.create, P.push_array, n, k);
    PUSHA("mono", M.create, M.push_array, n, k);
    PUSHA("int, naive iterated push", I.create, naive_push_array, n, k);
    PUSHA("int", I.create, I.push_array, n, k);
  ]

#define PUSHL(candidate,create,push_list,n,k) \
( \
  let basis = n \
  and name = sprintf "push_list (size %d, chunk size %d) (%s)" n k candidate \
  and run () = \
    let xs = List.init k (fun i -> i) in \
    fun () -> \
    let v = create () in \
    for _ = 0 to n-1 do \
      push_list v xs \
    done \
  in \
  B.benchmark ~name ~quota ~basis ~run \
)

let pushls k n =
  let[@inline] naive_push_list v xs =
    List.iter (I.push v) xs
  in
  [
    PUSHL("dynarray", R.create, R.append_list, n, k);
    PUSHL("poly", P.create, P.push_list, n, k);
    PUSHL("mono", M.create, M.push_list, n, k);
    PUSHL("int, naive iterated push", I.create, naive_push_list, n, k);
    PUSHL("int", I.create, I.push_list, n, k);
  ]

(* -------------------------------------------------------------------------- *)

(* Sort. *)

(* We make sure that all benchmarks use the same random data. *)

let data =
  ref [||]

let get_data n =
  if Array.length !data <> n then
    data := Array.init n (fun _i -> Random.int n);
  !data

#define SORT(candidate,of_array,sort,n) \
( \
  let basis = n \
  and name = sprintf "sort (random data, size %d) (%s)" n candidate \
  and run () = \
    (* We must create a fresh vector every time. *) \
    let v = of_array (get_data n) in \
    fun () -> \
      sort Int.compare v \
  in \
  B.benchmark ~name ~quota ~basis ~run \
)

#define SORTED(candidate,of_array,sort,n) \
( \
  let a = Array.init n (fun i -> i) in \
  let v = of_array a in \
  let basis = n \
  and name = sprintf "sort (sorted data, size %d) (%s)" n candidate \
  and run () () = \
    sort Int.compare v \
  in \
  B.benchmark ~name ~quota ~basis ~run \
)

let sorts n =
  [
    (* Sorting random data: *)
    SORT("array", Array.copy, Array.stable_sort, n);
    SORT("poly", P.of_array, P.stable_sort, n);
    SORT("mono", M.of_array, M.stable_sort, n);
    SORT("int", I.of_array, I.stable_sort, n);
    (* Sorting an already-sorted array: *)
    SORTED("array", Array.copy, Array.stable_sort, n);
    SORTED("poly", P.of_array, P.stable_sort, n);
    SORTED("mono", M.of_array, M.stable_sort, n);
    SORTED("int", I.of_array, I.stable_sort, n);
  ]

(* -------------------------------------------------------------------------- *)

(* Read the command line. *)

let push, pop, get, set, iter, iteri, pusha, pushl, sort =
  ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0, ref 0

let () =
  Arg.parse [
    "--push", Arg.Set_int push, " <n> Benchmark push";
    "--pop", Arg.Set_int pop, " <n> Benchmark pop";
    "--get", Arg.Set_int get, " <n> Benchmark get";
    "--set", Arg.Set_int set, " <n> Benchmark set";
    "--iter", Arg.Set_int iter, " <n> Benchmark iter (and get)";
    "--iteri", Arg.Set_int iteri, " <n> Benchmark iteri (and set)";
    "--pusha", Arg.Set_int pusha, " <n> Benchmark push_array";
    "--pushl", Arg.Set_int pushl, " <n> Benchmark push_list";
    "--sort", Arg.Set_int sort, " <n> Benchmark sort";
  ] (fun _ -> ()) "Invalid usage"

let push, pop, get, set, iter, iteri, pusha, pushl, sort =
  !push, !pop, !get, !set, !iter, !iteri, !pusha, !pushl, !sort

let possibly n (benchmarks : int -> B.benchmark list) =
  if n > 0 then run (benchmarks n)

(* -------------------------------------------------------------------------- *)

(* Main. *)

let () =
  possibly push pushes;
  possibly pop pops;
  possibly get gets;
  possibly set sets;
  possibly iter iters;
  possibly iteri iteris;
  possibly pusha (let k = 10 in pushas k);
  possibly pushl (let k = 10 in pushls k);
  possibly sort sorts;
  ()
