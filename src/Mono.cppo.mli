module Make (X : sig
  type t
  val make : int -> t -> t array
end) : sig

  type element = X.t

  #define CONTAINER vector
  #define ELEMENT   element

  #include "Signature.frag.mli"

end
