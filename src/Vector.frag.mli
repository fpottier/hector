(******************************************************************************)
(*                                                                            *)
(*                                   Hector                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2024--2024 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers an implementation of vectors. A vector is a mutable
   data structure, which stores a sequence of values. *)

(* -------------------------------------------------------------------------- *)

(* Types. *)

(**The type of a vector. *)
type VECTOR

(**A synonym for the type of a vector. *)
type SYNONYM = VECTOR

(**An integer value of type [length] represents the length of a sequence.
   For example, it can be the length of an array, the length of a vector,
   or the length of a segment of an array of vector. A length is nonnegative. *)
type length = int

(**An integer value of type [capacity] represents the capacity of a vector.
   A capacity is nonnegative. *)
type capacity = int

(**An integer value of type [index] represents an index into a sequence of
   elements.*)
type index = int

(* -------------------------------------------------------------------------- *)

(** {1:creating Creating} *)

(**[create()] creates a new vector of length 0 and capacity 0. *)
val create : unit -> VECTOR

(**[make n x] creates a new vector of length [n] and capacity [n]
   and initializes this vector by storing the value [x] everywhere.
   [n] must be a valid capacity. *)
val make : length -> ELEMENT -> VECTOR

(**[init n f] creates a new vector of length [n] and capacity [n]
   and initializes it, from left to right, by computing and storing
   the value [f i] at index [i]. [n] must be a valid capacity. *)
val init : length -> (index -> ELEMENT) -> VECTOR

(**[copy v] creates a new vector whose length and capacity are [length v]
   and initializes it with a copy of the data stored in the vector [v]. *)
val copy : VECTOR -> VECTOR

(**[sub v ofs len] produces a new vector whose elements are the elements of
   the vector segment determined by vector [v], offset [ofs], and length
   [len].
   [ofs] and [len] must describe a valid segment of the vector [v]. *)
val sub : VECTOR -> index -> length -> VECTOR

(**[concat vs] produces a new vector whose sequence of elements is
   the concatenation of the sequences of elements of the vectors
   in the list [vs]. *)
val concat : VECTOR list -> VECTOR

(* -------------------------------------------------------------------------- *)

(** {1:access Reading and writing} *)

(**[length v] is the (logical) length of the vector [v]. *)
val length : VECTOR -> length

(**[s_empty v] is equivalent to [length v = 0]. *)
val is_empty : VECTOR -> bool

(**[get v i] fetches the element that lies at index [i] in the vector [v].
   [i] must be comprised in the semi-open interval [\[0, length v)]. *)
val get : VECTOR -> index -> ELEMENT

(**[set v i x] overwrites the element that lies at index [i] in the vector
   [v] with the value [x]. [i] must be comprised in the semi-open interval
   [\[0, length v)]. *)
val set : VECTOR -> index -> ELEMENT -> unit

(**If the vector [v] is nonempty, [top v] returns its last element.
   If the vector [v] is empty, [top v] raises [Not_found]. *)
val top : VECTOR -> ELEMENT

(**[get_last] is a synonym for [top]. *)
val get_last : VECTOR -> ELEMENT (* synonym *)

(**If the vector [v] is nonempty, [top_opt v] returns its last element.
   If the vector [v] is empty, [top_opt v] returns [None]. *)
val top_opt : VECTOR -> ELEMENT option

(**[find_last] is a synonym for [top_opt]. *)
val find_last : VECTOR -> ELEMENT option (* synonym *)

(**[fill v ofs len x] writes the value [x] into every slot of the vector
   segment determined by vector [v], offset [ofs], and length [len].
   [ofs] and [len] must describe a valid segment of the vector [v]. *)
val fill : VECTOR -> index -> length -> ELEMENT -> unit

(** {2:access_unsafe Unsafe access} *)

(**[unsafe_get v i] fetches the element that lies at index [i] in the vector
   [v]. [i] must be comprised in the semi-open interval [\[0, length v)]. {b
   No bounds check is performed.} If the index [i] is out of bounds, memory
   safety can be compromised. Use at your own risk! *)
val unsafe_get : VECTOR -> index -> ELEMENT

(**[unsafe_set v i x] overwrites the element that lies at index [i]
   in the vector [v] with the value [x].
   [i] must be comprised in the semi-open interval [\[0, length v)].
   {b No bounds check is performed.} If the index [i] is out of bounds,
   memory safety can be compromised. Use at your own risk! *)
val unsafe_set : VECTOR -> index -> ELEMENT -> unit

(**[unsafe_borrow v] returns the data array that is part of the internal
   representation of the vector [v]. The length of this data array is at
   least [length v], and can be greater than [length v]. Beyond this
   guarantee, the length of this data array is unspecified; it is not
   necessarily the capacity of the vector. {b As long as the vector [v] is
   not modified,} the segment of the data array delimited by the semi-open
   interval [\[0, length v)] can be safely read and written. *)
val unsafe_borrow : VECTOR -> ELEMENT array

(* -------------------------------------------------------------------------- *)

(** {1:pushing Pushing} *)

(**[push v x] extends the vector [v] with the element [x]. The length of the
   vector [v] is increased by one. If necessary, the capacity of the vector
   [v] is increased. *)
val push : VECTOR -> ELEMENT -> unit

(**[add_last] is a synonym for [push]. *)
val add_last : VECTOR -> ELEMENT -> unit (* synonym *)

(**[push_array v a] extends the vector [v] with the elements of the array [a].
   The length of the vector [v] is increased by the length of the array [a].
   If necessary, the capacity of the vector [v] is increased. *)
val push_array : VECTOR -> ELEMENT array -> unit

(**[append_array] is a synonym for [push_array]. *)
val append_array : VECTOR -> ELEMENT array -> unit (* synonym *)

(**[push_array_segment v a ofs len] extends the vector [v] with the elements
   of the array segment determined by array [a], offset [ofs], and length
   [len]. The length of the vector [v] is increased by [len]. If necessary,
   the capacity of the vector [v] is increased. [ofs] and [len] must describe
   a valid segment of the array [a]. *)
val push_array_segment : VECTOR -> ELEMENT array -> index -> length -> unit

(**[append_array_segment] is a synonym for [push_array_segment]. *)
val append_array_segment : VECTOR -> ELEMENT array -> index -> length -> unit (* synonym *)

(**[push_vector v v'] extends the vector [v] with the elements of the vector
   [v']. The length of the vector [v] is increased by the length of the array
   [v']. If necessary, the capacity of the vector [v] is increased. *)
val push_vector : VECTOR -> VECTOR -> unit

(**[append] is a synonym for [push_vector]. *)
val append : VECTOR -> VECTOR -> unit (* synonym *)

(**[push_list v xs] extends the vector [v] with the elements of the list [xs].
   The length of the vector [v] is increased by the length of the list [xs].
   If necessary, the capacity of the vector [v] is increased. *)
val push_list : VECTOR -> ELEMENT list -> unit

(**[append_list] is a synonym for [push_list]. *)
val append_list : VECTOR -> ELEMENT list -> unit (* synonym *)

(**[push_seq v xs] extends the vector [v] with the elements of the sequence
   [xs]. The length of the vector [v] is increased by the length of the
   sequence [xs]. If necessary, the capacity of the vector [v] is increased.
   The sequence [xs] is demanded just once. *)
val push_seq : VECTOR -> ELEMENT Seq.t -> unit

(**[append_seq] is a synonym for [push_seq]. *)
val append_seq : VECTOR -> ELEMENT Seq.t -> unit (* synonym *)

(** [push_iter v iter c] pushes each element of the collection [c]
    in turn onto the vector [v]. The function [iter] is used to
    iterate over the elements of [c]. In other words,
    [push_iter v iter c] is equivalent to [iter (push v) c]. *)
val push_iter :
  VECTOR ->
  ((ELEMENT -> unit) -> 'c -> unit) ->
  'c -> unit

(**[append_iter] is a synonym for [push_iter]. *)
val append_iter : (* synonym *)
  VECTOR ->
  ((ELEMENT -> unit) -> 'c -> unit) ->
  'c -> unit

(* -------------------------------------------------------------------------- *)

(** {1:popping Popping} *)

(**If the vector [v] is nonempty, [pop v] removes and returns its last
   element. The length of the vector is decreased by one; its capacity is
   unchanged. If the vector [v] is empty, [pop v] raises [Not_found]. *)
val pop : VECTOR -> ELEMENT

(**[pop_last] is a synonym for [pop]. *)
val pop_last : VECTOR -> ELEMENT (* synonym *)

(**If the vector [v] is nonempty, [pop_opt v] removes and returns its last
   element. The length of the vector is decreased by one; its capacity is
   unchanged. If the vector [v] is empty, [pop_opt v] returns [None]. *)
val pop_opt : VECTOR -> ELEMENT option

(**[pop_last_opt] is a synonym for [pop_opt]. *)
val pop_last_opt : VECTOR -> ELEMENT option (* synonym *)

(**If the vector [v] is nonempty, [drop v] removes its last element.
   If the vector [v] is empty, [drop v] has no effect.
   [drop v] is equivalent to [if is_empty v then () else ignore (pop v)]. *)
val drop : VECTOR -> unit

(**[remove_last] is a synonym for [drop]. *)
val remove_last : VECTOR -> unit (* synonym *)

(* -------------------------------------------------------------------------- *)

(** {1:managing Length and capacity} *)

(**If [n] is less than [length v], then [truncate v n] sets the length of the
   vector [v] to [n]. Otherwise, nothing happens. In either case, the capacity
   of the vector [v] is unchanged. This is a constant-time operation. *)
val truncate : VECTOR -> length -> unit

(**[clear v] is equivalent to [truncate v 0]. The length of the vector [v]
   becomes zero. Its capacity is unchanged. *)
val clear : VECTOR -> unit

(**[reset v] sets the length and the capacity of the vector [v] to zero. *)
val reset : VECTOR -> unit

(**[ensure_capacity v c] ensures that the capacity of the vector [v] is at
   least [c]. If necessary, the capacity of the vector [v] is increased. *)
val ensure_capacity : VECTOR -> capacity -> unit

(**[ensure_extra_capacity v delta] ensures that the capacity of the vector
   [v] is at least [length v + delta]. If necessary, the capacity of the
   vector [v] is increased. The increment [delta] must be nonnegative. *)
val ensure_extra_capacity : VECTOR -> capacity -> unit

(**[fit_capacity v] ensures that the capacity of the vector [v] matches its
   length. If necessary, the capacity of the vector [v] is decreased. *)
val fit_capacity : VECTOR -> unit

(**[set_capacity v c] ensures that the capacity of the vector [v] is exactly
   [c]. If [c] is less than [length v], then the vector [v] is truncated:
   some elements are lost. Otherwise, the elements of the vector [v] are
   preserved, and its capacity is decreased or increased as necessary. *)
val set_capacity : VECTOR -> capacity -> unit

(* -------------------------------------------------------------------------- *)

(** {1:iterating Iterating} *)

(**[iter f v] applies the function [f] in turn, from left to right, to each
   element [x] of the vector [v]. *)
val iter : (ELEMENT -> unit) -> VECTOR -> unit

(**[iter_down f v] applies the function [f] in turn, from right to left, to
   each element [x] of the vector [v]. *)
val iter_down : (ELEMENT -> unit) -> VECTOR -> unit

(**[iteri f v] applies the function [f] in turn, from left to right, to each
   index [i] and corresponding element [x] of the vector [v]. *)
val iteri : (int -> ELEMENT -> unit) -> VECTOR -> unit

(**[fold_left f s v] applies the function [f] in turn, from left to right, to
   each element [x] of the vector [v]. A state, whose initial value is [s], is
   threaded through this sequence of function invocations, and is eventually
   returned. *)
val fold_left : ('s -> ELEMENT -> 's) -> 's -> VECTOR -> 's

(**[fold_right f v s] applies the function [f] in turn, from right to left, to
   each element [x] of the vector [v]. A state, whose initial value is [s], is
   threaded through this sequence of function invocations, and is eventually
   returned. *)
val fold_right : (ELEMENT -> 's -> 's) -> VECTOR -> 's -> 's

(* -------------------------------------------------------------------------- *)

(** {1:transforming Transforming} *)

(**[map f v] applies the function [f] in turn, from left to right, to each
   element [x] of the vector [v], and constructs a new vector of the results
   of these calls. *)
val map : (ELEMENT -> ELEMENT') -> VECTOR -> VECTOR'

(**[mapi f v] applies the function [f] in turn, from left to right, to each
   index [i] and corresponding element [x] of the vector [v], and constructs a
   new vector of the results of these calls. *)
val mapi : (index -> ELEMENT -> ELEMENT') -> VECTOR -> VECTOR'

(**[filter f v] applies the function [f] in turn, from left to right, to each
   element [x] of the vector [v], and constructs a new vector containing just
   the elements [x] such that [f x] returned [true]. *)
val filter : (ELEMENT -> bool) -> VECTOR -> VECTOR

(**[filter_map f v] applies the function [f] in turn, from left to right, to
   each element [x] of the vector [v], and constructs a new vector containing
   just the values [y] such that [f x] returned [Some y]. *)
val filter_map : (ELEMENT -> ELEMENT' option) -> VECTOR -> VECTOR'

(* -------------------------------------------------------------------------- *)

(** {1:searching Searching} *)

(**[exists f v] determines whether at least one element [x] of the vector [v]
   satisfies the predicate [f]. The vector is scanned from left to right. *)
val exists : (ELEMENT -> bool) -> VECTOR -> bool

(**[for_all f v] determines whether all elements [x] of the vector [v] satisfy
   the predicate [f]. The vector is scanned from left to right. *)
val for_all : (ELEMENT -> bool) -> VECTOR -> bool

(**[find f v] finds the leftmost element [x] of the vector [v] such that
   [f x] is true, and returns its index. If no such element exists, then
   [find f v] raises [Not_found]. *)
val find : (ELEMENT -> bool) -> VECTOR -> int

(* -------------------------------------------------------------------------- *)

(** {1:comparing Comparing} *)

(**Provided [eq] is an equality on elements, [equal eq] is the pointwise
   equality of vectors. In other words, [equal eq v v'] determines whether
   the sequences of elements of the vectors [v] and [v'] are pointwise
   equal, using the function [eq] to test whether two elements are equal. *)
val equal : (ELEMENT -> ELEMENT -> bool) -> VECTOR -> VECTOR -> bool

(**Provided [cmp] is a preorder on elements, [compare cmp] is the
   lexicographic preorder on vectors. In other words, [compare cmp v v']
   compares the sequences of elements of the vectors [v] and [v'], according
   to the lexicographic preorder, and using the function [cmp] to compare
   two elements.

   {b Caution:} [compare] behaves like [List.compare],
   not like [Dynarray.compare].
   [Dynarray.compare] implements a preorder on vectors
   that is not is the lexicographic preorder. *)
val compare : (ELEMENT -> ELEMENT -> int) -> VECTOR -> VECTOR -> int

(* -------------------------------------------------------------------------- *)

(** {1:converting Converting} *)

(**[of_array a] returns a new vector whose elements are the elements of
   the array [a]. The length and capacity of the new vector are the length
   of the array [a]. *)
val of_array : ELEMENT array -> VECTOR

(**[of_list xs] returns a new vector whose elements are the elements of
   the list [xs]. The length and capacity of the new vector are the length
   of the list [xs]. *)
val of_list : ELEMENT list -> VECTOR

(**[of_seq xs] returns a new vector whose elements are the elements of the
   sequence [xs]. The length and capacity of the new vector are the length
   of the sequence [xs]. *)
val of_seq : ELEMENT Seq.t -> VECTOR

(**[to_array v] creates a new array whose elements are the elements of the
   vector [v]. The length of the new array is the length of the vector [v]. *)
val to_array : VECTOR -> ELEMENT array

(**[to_list v] creates a new list whose elements are the elements of the
   vector [v]. The length of the new list is the length of the vector [v]. *)
val to_list : VECTOR -> ELEMENT list

(**[to_seq v] creates a sequence whose elements are the elements of the
   vector [v]. The length of this sequence is the length of the vector
   [v]. This sequence can be demanded as many times as one wishes.
   However, this sequence is valid only as long as the vector [v] is not
   modified. As soon as [v] is modified, this sequence must no longer be
   used. *)
val to_seq : VECTOR -> ELEMENT Seq.t

(**[to_seq_rev v] creates a sequence whose elements are the elements of
   the vector [v], in reverse order. The length of this sequence is the
   length of the vector [v]. This sequence can be demanded as many times
   as one wishes. However, this sequence is valid only as long as the
   vector [v] is not modified. As soon as [v] is modified, this sequence
   must no longer be used. *)
val to_seq_rev : VECTOR -> ELEMENT Seq.t

(* -------------------------------------------------------------------------- *)

(** {1:showing Showing} *)

(**[show f v] returns a textual representation of the contents of the
   vector [v]. The user-supplied function [f] is used to obtain a
   textual representation of each element. *)
val show : (ELEMENT -> string) -> VECTOR -> string

(**/**)
(**[check] is used only during testing. *)
val check : VECTOR -> unit
(**/**)

(* -------------------------------------------------------------------------- *)

(** {1:stack The Stack API} *)

(**This module offers the same API as the standard library module
   [Stdlib.Stack], but is implemented using vectors. *)
module Stack : sig
  type SYNONYM = VECTOR
  exception Empty
  val create : unit -> SYNONYM
  val push : ELEMENT -> SYNONYM -> unit
  val pop : SYNONYM -> ELEMENT
  val pop_opt : SYNONYM -> ELEMENT option
  val drop : SYNONYM -> unit
  val top : SYNONYM -> ELEMENT
  val top_opt : SYNONYM -> ELEMENT option
  val clear : SYNONYM -> unit
  val copy : SYNONYM -> SYNONYM
  val is_empty : SYNONYM -> bool
  val length : SYNONYM -> int
  val iter : (ELEMENT -> unit) -> SYNONYM -> unit
  val fold : ('s -> ELEMENT -> 's) -> 's -> SYNONYM -> 's
  val to_seq : SYNONYM -> ELEMENT Seq.t
  val add_seq : SYNONYM -> ELEMENT Seq.t -> unit
  val of_seq : ELEMENT Seq.t -> SYNONYM
end
