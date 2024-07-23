type element = int
type dummy = element
type t = element array
type length = int
type index = int
val length : t -> length
val unsafe_get : t -> index -> element
val unsafe_set : t -> index -> element -> unit
val alloc : length -> dummy -> t
val make : length -> element -> t
val init : length -> (index -> element) -> t
val sub : t -> index -> length -> t
val blit_disjoint : t -> index -> t -> index -> length -> unit
