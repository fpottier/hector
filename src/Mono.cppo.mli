module Make (X : sig type t end) : sig

  type element = X.t

  #define CONTAINER vector
  #define ELEMENT   element

  #include "Signature.frag.mli"

end
