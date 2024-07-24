# To Do

## Soon

* `next_capacity` should avoid exceeding the maximum array length
  (which should be a field in the structure `A`).

* Remove the comment that talks about hesitation.
  Use macros to offer a choice between multiple implementations of
  `blit`, if necessary.

* Publish `IntArray` as `Arrays.Int`.

* Improve the compatibility with OCaml's `Dynarray`.
  In the documentation, say something about compatibility.
  Our `compare` has the semantics of `List.compare`,
  not `Dynarray.compare`.

* Check compatibility with the `Stack` API.
  Also, benchmark against `Stack`.

* Test using `Dynarray` as a reference implementation.

* Test all implementations (`Poly`, `Mono`, `Int`) in parallel.

* Make sure every interface file is commented. Review the documentation.
  Document the fact that a vector must not be modified while iteration is
  ongoing.

* Benchmark more operations, if necessary.
  Double-check the performance claims
  found in the documentation.

* Use the new release of `cppo`
  to write multi-line macros without backslashes,
  remove all uses of `#undef`,
  use higher-order macros where possible.
  In particular, `ELEMENT`, `VECTOR`, and the signature itself
  can be higher-order macros;
  no need to simulate them via unhygienic `#include`.

* Reconsider the use of `LOOP5` in every loop.
  Some loops could use just `LOOP`; some loops could use `LOOPRW5`.
  If we are certain to use `LOOP`, then a `for` loop may be preferable.
  Also, `fold_right` uses `LOOP_DOWN`; `LOOP5_DOWN` is still missing.

* Release.

## Maybe Later

* Generalize the signature `MONOARRAY` so that the type of arrays is
  abstract. This requires distinguishing several variants of `sub`
  and `blit_disjoint`. Then, implement `CharArray` using `bytes`.
  Implement a character vector, and compare its efficiency with `Buffer`.

* Expose `MonoArray.frag.ml` as a functor that builds a complete
  implementation of `MONOARRAY` out of just `empty`, `alloc`, and
  `make`?

* Can (should) the types `'a vector` and `'a t` be declared injective?

* Benchmark a real-world application of integer vectors and determine
  whether it is worthwhile to use an unscanned integer array.
