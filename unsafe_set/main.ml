(* -------------------------------------------------------------------------- *)

(* The implementation that we wish to benchmark. *)

(* open Hector.Poly *)
(* open Hector.Mono.Make(struct type t = int let make = Array.make end) *)
open Hector.Int

(* -------------------------------------------------------------------------- *)

(* Set. *)

let init n =
  let v = create () in
  for i = 0 to n-1 do
    let dummy = i in
    push v dummy
  done;
  v

let sum v n =
  let s = ref 0 in
  for i = 0 to n-1 do
    s := !s + get v i
  done;
  Printf.printf "%d\n" !s

let repetitions =
  100

let benchmark_set n =
  (* Initialization: *)
  let v = init n in
  for _ = 1 to repetitions do
    (* Benchmark: *)
    for i = 0 to n-1 do
      let dummy = 2 * i in
      set v i dummy
    done
  done;
  (* Dummy final read: *)
  sum v n

let benchmark_unsafe_set n =
  (* Initialization: *)
  let v = init n in
  for _ = 1 to repetitions do
    (* Benchmark: *)
    for i = 0 to n-1 do
      let dummy = 2 * i in
      unsafe_set v i dummy
    done
  done;
  (* Dummy final read: *)
  sum v n

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
