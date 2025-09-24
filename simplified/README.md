This directory contains a simplified version of Hector:

* Macros are avoided, so the dependency on `cppo` is removed.
* No external C code is used.
* The module `Obj` is not used; this is vanilla OCaml code.

Only one variant of vectors is offered, namely polymorphic vectors.

This simplified version of Hector is currently not packaged as part of Hector,
because there is no need to. It is copied inside Menhir, where we do not want
to rely on `cppo` or on external C code.
