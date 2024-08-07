It is my pleasure to announce the first release of `hector`, an OCaml library that offers *vectors* (also known as dynamic arrays, or resizeable arrays).

To install it, type `opam update && opam install hector`.

`hector` offers **polymorphic vectors**, where the type of the elements is a parameter, **monomorphic vectors**, where the type of the elements is fixed, and **integer vectors**, a special case of monomorphic vectors.

`hector`'s vectors are **not thread-safe** and **do not include a protection against memory leaks**. Compared with the `Dynarray` module in OCaml's standard library, `hector`'s polymorphic and monomorphic vectors are **slightly faster**, and `hector`'s integer vectors are **significantly faster**. `hector` offers fast (but dangerous) **unsafe access operations**, namely `unsafe_get`, `unsafe_set`, and `unsafe_borrow`. For a more detailed overview, see the [documentation](https://cambium.inria.fr/~fpottier/hector/doc/hector/).
