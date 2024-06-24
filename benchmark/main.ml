open Printf

module B = Common.Benchmark
let run benchmarks =
  List.iter B.drive_and_display benchmarks

let quota =
  "7.0s"

(* -------------------------------------------------------------------------- *)

(* The implementations that we wish to benchmark. *)

module R = Dynarray
module P = Hector.Poly
module M = Hector.Mono.Make(struct type t = int let make = Array.make end)
module I = Hector.Int

(* -------------------------------------------------------------------------- *)

(* Push. *)

let pushes candidate create push n =
  let basis = 1
  and name = sprintf "pushes (size %d) (%s)" n candidate
  and run () () =
    let v = create() in
    for i = 0 to n-1 do
      let dummy = 42 in
      push v dummy;
    done
  in
  B.benchmark ~name ~quota ~basis ~run

let pushes n =
  [
    R.(pushes "dynarray" create add_last n);
    P.(pushes "poly" create push n);
    M.(pushes "mono" create push n);
    I.(pushes "int" create push n);
  ]

(* -------------------------------------------------------------------------- *)

(* Get and set. *)

let getset candidate create push get set n =
  let basis = 1
  and name = sprintf "get/set (size %d) (%s)" n candidate
  and run () =
    (* Initialization: *)
    let v = create() in
    for i = 0 to n-1 do
      let dummy = i in
      push v dummy;
    done;
    fun () ->
      (* Benchmark: *)
      for i = 0 to n-1 do
        set v i (get v (n-1-i))
      done
  in
  B.benchmark ~name ~quota ~basis ~run

let getset n =
  [
    R.(getset "dynarray" create add_last get set n);
    P.(getset "poly" create push get set n);
    M.(getset "mono" create push get set n);
    I.(getset "int" create push get set n);
    P.(getset "poly/unsafe" create push unsafe_get unsafe_set n);
    M.(getset "mono/unsafe" create push unsafe_get unsafe_set n);
    I.(getset "int/unsafe" create push unsafe_get unsafe_set n);
  ]

(* -------------------------------------------------------------------------- *)

(* Main. *)

let sizes =
  [ 1_000_000; 10_000_000; 100_000_000 ]

let () =

  if false then begin
    eprintf "*** pushes\n";
    eprintf "\n";
    sizes |> List.iter @@ fun n ->
    run (pushes n);
    eprintf "\n"
  end;

  if true then begin
    eprintf "*** get/set\n";
    eprintf "\n";
    sizes |> List.iter @@ fun n ->
    run (getset n);
    eprintf "\n"
  end;

  ()
