(* -------------------------------------------------------------------------- *)

(* The implementation that we wish to benchmark. *)

open Array

(* -------------------------------------------------------------------------- *)

(* Set. *)

let chrono msg f x =
  let start = Unix.gettimeofday() in
  let y = f x in
  let stop = Unix.gettimeofday() in
  Printf.eprintf "%s = %.6f seconds\n%!" msg (stop -. start);
  y

let init n =
  let dummy = 42 in
  let v = make n dummy in
  v

let init n =
  chrono "initialization time" init n

let sum v n =
  let s = ref 0 in
  for i = 0 to n-1 do
    s := !s + get v i
  done;
  Printf.printf "%d\n" !s

let sum v n =
  chrono "  finalization time" (sum v) n

let repetitions =
  100

let benchmark_set n =
  (* Initialization: *)
  let v = init n in
  for _ = 1 to repetitions do
    (* Benchmark: *)
    let i = ref 0 in
    while !i < n do
      let dummy = 2 * !i in
      set v !i dummy;
      i := !i + 1
    done
  done;
  (* Dummy final read: *)
  sum v n

let benchmark_unsafe_set n =
  (* Initialization: *)
  let v = init n in
  for _ = 1 to repetitions do
    (* Benchmark: *)
    let i = ref 0 in
    while !i < n do
      let dummy = 2 * !i in
      unsafe_set v !i dummy;
      i := !i + 1
    done
  done;
  (* Dummy final read: *)
  sum v n

let benchmark_set_unrolled_5 n =
  (* Initialization: *)
  let v = init n in
  for _ = 1 to repetitions do
    (* Benchmark: *)
    let i = ref 0 in
    while !i < n do
      let dummy = 2 * !i in
      set v !i dummy;
      i := !i + 1;
      let dummy = 2 * !i in
      set v !i dummy;
      i := !i + 1;
      let dummy = 2 * !i in
      set v !i dummy;
      i := !i + 1;
      let dummy = 2 * !i in
      set v !i dummy;
      i := !i + 1;
      let dummy = 2 * !i in
      set v !i dummy;
      i := !i + 1;
    done
  done;
  (* Dummy final read: *)
  sum v n

let benchmark_unsafe_set_unrolled_5 n =
  (* Initialization: *)
  let v = init n in
  for _ = 1 to repetitions do
    (* Benchmark: *)
    let i = ref 0 in
    while !i < n do
      let dummy = 2 * !i in
      unsafe_set v !i dummy;
      i := !i + 1;
      let dummy = 2 * !i in
      unsafe_set v !i dummy;
      i := !i + 1;
      let dummy = 2 * !i in
      unsafe_set v !i dummy;
      i := !i + 1;
      let dummy = 2 * !i in
      unsafe_set v !i dummy;
      i := !i + 1;
      let dummy = 2 * !i in
      unsafe_set v !i dummy;
      i := !i + 1;
    done
  done;
  (* Dummy final read: *)
  sum v n

(* -------------------------------------------------------------------------- *)

(* Main. *)

let n =
  100_000

let unsafe, unrolled =
  let unsafe, unrolled = ref false, ref false in
  Arg.parse [
    "--unsafe", Arg.Set unsafe, " Test unsafe_set.";
    "--unrolled", Arg.Set unrolled, " Test unrolled loop.";
  ] (fun _ -> ()) "Usage: main.exe [--unsafe]";
  !unsafe, !unrolled

let () =
  if unsafe && unrolled then
    benchmark_unsafe_set_unrolled_5 n
  else if unsafe then
    benchmark_unsafe_set n
  else if unrolled then
    benchmark_set_unrolled_5 n
  else
    benchmark_set n
