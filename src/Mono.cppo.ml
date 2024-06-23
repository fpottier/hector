module[@inline] Make (X : sig
  type t
  val make : int -> t -> t array
end) = struct

  #include "MonoBody.frag.ml"

end (* Make *)
