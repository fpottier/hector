open Printf

(* -------------------------------------------------------------------------- *)

(* The implementation that we wish to benchmark. *)

(* module P = Hector.Poly *)
(* module M = Hector.Mono.Make(struct type t = int let make = Array.make end) *)
open Hector.Int

(* -------------------------------------------------------------------------- *)

(* Set. *)

let benchmark_set n =
  (* Initialization: *)
  let v = create () in
  for i = 0 to n-1 do
    let dummy = i in
    push v dummy
  done;
  for _ = 0 to 100 do
    (* Benchmark: *)
    for i = 0 to n-1 do
      let dummy = 2 * i in
      set v i dummy
    done
  done

let benchmark_unsafe_set n =
  (* Initialization: *)
  let v = create () in
  for i = 0 to n-1 do
    let dummy = i in
    push v dummy
  done;
  for _ = 0 to 100 do
    (* Benchmark: *)
    for i = 0 to n-1 do
      let dummy = 2 * i in
      unsafe_set v i dummy
    done
  done

(* -------------------------------------------------------------------------- *)

(* Main. *)

let n =
  100_000

let unsafe =
  let unsafe = ref false in
  Arg.parse [
    "--unsafe", Arg.Set unsafe, " Test unsafe_set."
  ] (fun _ -> ()) "Usage: main.exe [--unsafe]";
  !unsafe

let () =
  if unsafe then
    benchmark_unsafe_set n
  else
    benchmark_set n
