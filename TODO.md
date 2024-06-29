# To Do

* Unroll more loops? E.g., `iter`. (Benchmark.)

* Fix `set` benchmark so that it is not slow under OCaml 5?

* Improve the compatibility of our `Vector` modules with OCaml's `Dynarray`.

* Test using `Dynarray` as a reference implementation.

* Test all three implementations (`Poly`, `Mono`, `Int`) in parallel.

* Make sure every interface file is commented. Write documentation.

* Benchmark.
  + `push` benchmark.
  + `get` and `set` benchmark. (Benchmark also the `unsafe` variants.)
  + memory allocation while large vectors exist.

* Release.
