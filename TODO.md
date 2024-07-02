# To Do

## Soon

* Improve the compatibility of our `Vector` modules with OCaml's `Dynarray`.

* Test using `Dynarray` as a reference implementation.

* Test all three implementations (`Poly`, `Mono`, `Int`) in parallel.

* Make sure every interface file is commented. Write documentation.

* Benchmark more operations.

* Release.

## Maybe Later

* Our `for` loops can suffer from the slow-memory-barrier issue on Apple
  processors. (See for example the benchmark `iteri (int/unsafe)`, when
  compiled with OCaml 5 with `flambda`.) Should we unroll our loops?
  + In `MonoBody`, `init`.
  + In `Common`, `iter` and `iteri`.

* Benchmark a real-world application of integer vectors and determine
  whether it is worthwhile to use an unscanned integer array.
