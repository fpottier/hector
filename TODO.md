# To Do

## Soon

* Reconsider the use of `LOOP5` in every loop.
  Some loops could use just `LOOP`; some loops could use `LOOPRW5`.
  If we are certain to use `LOOP`, then a `for` loop may be preferable.
  Also, some functions use `LOOP_DOWN`; `LOOP5_DOWN` is still missing.

* Publish the module `IntArray`.

* Use the new release of `cppo`
  to write multi-line macros without backslashes,
  remove all uses of `#undef`,
  use higher-order macros where possible.
  In particular, `ELEMENT`, `VECTOR`, and the signature itself
  can be higher-order macros;
  no need to simulate them via unhygienic `#include`.

* Once a new version of Monolith is released,
  use its new combinators `naive_array` and `naive_seq`.

## Maybe Later

* We need functions to extend the length of a segment and initialize
  every new slot with a value `x`, or with a function `f i`. In other
  words, combinations of `push_vector` and `make` or `init`.

* Document and publish `ArraySegment`?
  These operations would allow operating not only on array segments,
  but also (through `unsafe_borrow`) on vector segments.

* Benchmark `map`.
  Examine the generated assembly code for `map` and `mapi`.
  Do we pay two function calls per loop iteration?
    (one call to `f` plus one call to `fun i -> ...`)
  If so, consider exporting `A.init` as a macro.

* Add `of_array_segment`.

* Add new operations taken from the `Array` API:
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
  `filteri`, -- from List
  `split`,
  `combine`,
  `compress`, -- from Ogre.ArrayExtra
  `shuffle`,
  `to_seqi`.

* Define `iteri_down`, `foldi_left`, `foldi_right`.

* Add `rev` (an instance of `init`) and `rev_inplace`.

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
  and `A.blit`. it would be necessary to remove the public operation
  `unsafe_borrow`.

  Then, implement `BitArray` and `CharArray` using a compact representation,
  based on the type `bytes`. Implement a bit vector and a character vector.
  Compare the efficiency of the character vector with `Buffer`.

* Benchmark a real-world application of integer vectors and determine
  whether it is worthwhile to use an unscanned integer array.
