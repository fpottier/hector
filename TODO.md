# To Do

## Soon

* For optimum speed, in integer vectors, we need a way of initializing
  a freshly allocated array partly by copying data from an existing
  array, partly by filling the new array with arbitrary data (memset).

* Improve the compatibility with OCaml's `Dynarray`.
  In the documentation, say something about compatibility.

* Test using `Dynarray` as a reference implementation.

* Test all three implementations (`Poly`, `Mono`, `Int`) in parallel.

* Make sure every interface file is commented. Write documentation.

* Benchmark more operations, if necessary.
  Double-check the performance claims
  found in the documentation.

* Release.

## Maybe Later

* Document `Make_`?

* Can (should) the types `'a vector` and `'a t` be declared injective?

* Benchmark a real-world application of integer vectors and determine
  whether it is worthwhile to use an unscanned integer array.
