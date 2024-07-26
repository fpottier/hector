# To Do

## Soon

* Maybe remove all calls to `validate length data`,
  which are not placed consistently.
  Or ensure that they are used consistently.

* Publish `IntArray` as `Arrays.Int`.

* In the documentation, describe Hector's compatibility with `Dynarray`.
  Our `append` allows `append v v`.
  Our `get_last` can be applied to an empty vector.
  Our `compare` has the semantics of `List.compare`,
  not `Dynarray.compare`.
  `to_seq_reentrant` and `to_seq_rev_reentrant`
  are intentionally not supported.
  `capacity` is intentionally not supported.
  Features not supported by `Dynarray`:
  `find`, `unsafe_get`, `unsafe_set`, `unsafe_borrow`, the Stack API.

* Add a benchmark that mixes `push` and `pop`.
  Include `Stack` in the benchmark.

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
  Also, some functions use `LOOP_DOWN`; `LOOP5_DOWN` is still missing.

* Once a new version of Monolith is released,
  use its new combinators `naive_array` and `naive_seq`.

* Release.

## Maybe Later

* Add `of_array_segment`.

* Isolate a submodule `ArraySegment` containing all of the operations
  on array segments that we need. Do it *inside* `Vector.frag.ml` so
  that it depends only on `A` and is specialized for integers.

* Add new operations taken from the `Array` API:
  `concat`,
  `sub`,
  `fill`,
  `blit`,
  `map_inplace`,
  `mapi_inplace`,
  `fold_left_map`,
  `iter2`,
  `map2`,
  `for_all2`,
  `exists2`,
  `mem` and `memq` (?),
  `find_opt`,
  `find_index`,
  `find_map`,
  `find_mapi`,
  `split`,
  `combine`,
  `sort`,
  `stable_sort`,
  `fast_sort`,
  `shuffle`,
  `to_seqi`.

* Publish `iter_down`.

* Add an in-place `reverse` function,
  as well as `rev` (an instance of `mapi`).

* Add `push_list_segment` and `push_seq_segment`
  to push a sequence whose length is known in advance.

* Add `push_rev_*` variants to reverse the extra sequence
  on the fly before appending it.

* It may seem tempting to generalize our implementation of vectors so that it
  does not rely on the knowledge that the data array is an array. For example,
  we may want to implement bit vectors and character vectors, where the data
  array actually has type `bytes`.

  Perhaps, instead of generalizing our implementation, we could develop a
  separate implementation (with some shared code) where the type `A.t` is
  abstract. It would be necessary to distinguish several variants of `A.sub`
  and `A.blit_disjoint`. it would be necessary to remove the public operation
  `unsafe_borrow`.

  Then, implement `BitArray` and `CharArray` using a compact representation,
  based on the type `bytes`. Implement a bit vector and a character vector.
  Compare the efficiency of the character vector with `Buffer`.

* Expose `MonoArray.frag.ml` as a functor that builds a complete
  implementation of `MONOARRAY` out of just `empty`, `alloc`, and
  `make`?

* Can (should) the types `'a vector` and `'a t` be declared injective?

* Benchmark a real-world application of integer vectors and determine
  whether it is worthwhile to use an unscanned integer array.
