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

(**A vector is a mutable abstract data structure, which stores a sequence of
   values. *)
type VECTOR

type SYNONYM = VECTOR

type length = int
type capacity = int
type index = int

(**[length v] is the (logical) length of the vector [v]. *)
val length : VECTOR -> length

(**[unsafe_borrow v] returns the data array that is part of the internal
   representation of the vector [v]. The length of this data array is at
   least [length v], and can be greater than [length v]. As long as the
   vector [v] is not modified, the segment of the data array delimited by
   the semi-open interval [\[0, length v)] can be safely read and
   written. *)
val unsafe_borrow : VECTOR -> ELEMENT array

(**[s_empty v] is equivalent to [length v = 0]. *)
val is_empty : VECTOR -> bool

(**[create()] creates a new vector of length 0 and capacity 0. *)
val create : unit -> VECTOR

(**[make n x] creates a new vector of length and capacity [n]
   and initializes it by storing the value [x] everywhere.
   [n] must be a valid capacity. *)
val make : length -> ELEMENT -> VECTOR

(**[init n f] creates a new vector of length and capacity [n]
   and initializes it, from left to right, by storing
   the value [f i] at each index [i].
   [n] must be a valid capacity. *)
val init : length -> (index -> ELEMENT) -> VECTOR

(**[copy v] creates a new vector of length and capacity [length v]
   and initializes it with a copy of the data stored in [v]. *)
val copy : VECTOR -> VECTOR

(**[to_array v] creates a new array of [length v] whose elements are the
   elements of the vector [v]. The vector [v] is unaffected. *)
val to_array : VECTOR -> ELEMENT array

(**[get v i] fetches the element at index [i] in vector [v].
   [i] must be comprised in the semi-open interval of 0 to [length v]. *)
val get : VECTOR -> index -> ELEMENT

(**[unsafe_get v i] fetches the element at index [i] in vector [v].
   [i] must be comprised in the semi-open interval of 0 to [length v].
   {b No bounds check is performed.} If the index [i] is out of bounds,
   memory safety can be compromised. Use at your own risk! *)
val unsafe_get : VECTOR -> index -> ELEMENT

(**[set v i x] overwrites the element at index [i] in vector [v] with the
   value [x]. [i] must be comprised in the semi-open interval of 0 to
   [length v]. *)
val set : VECTOR -> index -> ELEMENT -> unit

(**[unsafe_set v i x] overwrites the element at index [i] in vector
   [v] with the value [x]. [i] must be comprised in the semi-open
   interval of 0 to [length v].
   {b No bounds check is performed.} If the index [i] is out of bounds,
   memory safety can be compromised. Use at your own risk! *)
val unsafe_set : VECTOR -> index -> ELEMENT -> unit

(**[push v x] appends the element [x] at the end of the vector [v],
   that is, at offset [length v]. If necessary, the capacity of the
   vector is increased. *)
val push : VECTOR -> ELEMENT -> unit

(**[add_last] is a synonym for [push]. *)
val add_last : VECTOR -> ELEMENT -> unit (* synonym *)

(**[pop_opt v] removes and returns the last element of the vector [v].
   If the vector is empty, [None] is returned. *)
val pop_opt : VECTOR -> ELEMENT option

(**[pop_last_opt] is a synonym for [pop_opt]. *)
val pop_last_opt : VECTOR -> ELEMENT option (* synonym *)

(**[pop v] removes and returns the last element of the vector [v].
   If the vector is empty, [Not_found] is raised. *)
val pop : VECTOR -> ELEMENT

(**[pop_last] is a synonym for [pop]. *)
val pop_last : VECTOR -> ELEMENT (* synonym *)

(**[drop v] removes the last element of the vector [v].
   If the vector [v] is empty, [drop v] has no effect. *)
val drop : VECTOR -> unit

(**[remove_last] is a synonym for [drop]. *)
val remove_last : VECTOR -> unit (* synonym *)

(**[top v] returns the last element of the vector [v].
   If the vector is empty, [Not_found] is raised. *)
val top : VECTOR -> ELEMENT

(**[get_last] is a synonym for [peek]. *)
val get_last : VECTOR -> ELEMENT (* synonym *)

(**[peek_opt v] returns the last element of the vector [v].
   If the vector is empty, [None] is returned. *)
val top_opt : VECTOR -> ELEMENT option

(**[find_last] is a synonym for [peek_opt]. *)
val find_last : VECTOR -> ELEMENT option (* synonym *)

(**TODO*)
val push_array : VECTOR -> ELEMENT array -> unit

(**[append_array] is a synonym for [push_array]. *)
val append_array : VECTOR -> ELEMENT array -> unit (* synonym *)

(**TODO*)
val push_array_segment : VECTOR -> ELEMENT array -> index -> length -> unit

(**[append_array_segment] is a synonym for [push_array_segment]. *)
val append_array_segment : VECTOR -> ELEMENT array -> index -> length -> unit (* synonym *)

(**TODO*)
val push_vector : VECTOR -> VECTOR -> unit

(**[append] is a synonym for [push_vector]. *)
val append : VECTOR -> VECTOR -> unit (* synonym *)

(**TODO*)
val push_list : VECTOR -> ELEMENT list -> unit

(**[append_list] is a synonym for [push_list]. *)
val append_list : VECTOR -> ELEMENT list -> unit (* synonym *)

(**TODO*)
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

(**TODO*)
val concat : VECTOR list -> VECTOR

(**If [n] is less than [length v], then [truncate v n] sets the length of the
   vector [v] to [n]. Otherwise, nothing happens. In either case, the capacity
   of the vector is unchanged. This is a constant-time operation. *)
val truncate : VECTOR -> length -> unit

(**[clear v] is equivalent to [truncate v 0]. The length of the vector becomes
   zero; its capacity remains unchanged. *)
val clear : VECTOR -> unit

(**[reset v] sets both the length and the capacity of the vector [v] to zero. *)
val reset : VECTOR -> unit

(**[ensure_capacity v c] ensures that the capacity of the vector [v]
   is at least [c]. *)
val ensure_capacity : VECTOR -> capacity -> unit

(**[ensure_capacity v delta] ensures that the capacity of the vector [v] is at
   least [length v + delta]. The increment [delta] must be nonnegative. *)
val ensure_extra_capacity : VECTOR -> capacity -> unit

(**[fit_capacity v] ensures that the capacity of the vector [v] matches
   its length. If necessary, the capacity of the vector is decreased. *)
val fit_capacity : VECTOR -> unit

(**[set_capacity v c] ensures that the capacity of the vector [v] is exactly
   [c]. If [c] is less than [length v], then the vector is truncated: that is,
   some elements are lost. Otherwise, the elements of the vector are
   preserved, and the capacity of the vector is decreased or increased as
   necessary. *)
val set_capacity : VECTOR -> capacity -> unit

(**[iter f v] applies the function [f] in turn, from left to right, to each
   element [x] of the vector [v]. *)
val iter : (ELEMENT -> unit) -> VECTOR -> unit

(**[iter f v] applies the function [f] in turn, from left to right, to each
   index [i] and element [x] in the vector [v]. *)
val iteri : (int -> ELEMENT -> unit) -> VECTOR -> unit

(**TODO*)
val map : (ELEMENT -> ELEMENT') -> VECTOR -> VECTOR'

(**TODO*)
val mapi : (index -> ELEMENT -> ELEMENT') -> VECTOR -> VECTOR'

(**TODO*)
val fold_left : ('s -> ELEMENT -> 's) -> 's -> VECTOR -> 's

(**TODO*)
val fold_right : (ELEMENT -> 's -> 's) -> VECTOR -> 's -> 's

(**TODO*)
val exists : (ELEMENT -> bool) -> VECTOR -> bool

(**TODO*)
val for_all : (ELEMENT -> bool) -> VECTOR -> bool

(**TODO*)
val filter : (ELEMENT -> bool) -> VECTOR -> VECTOR

(**TODO*)
val filter_map : (ELEMENT -> ELEMENT' option) -> VECTOR -> VECTOR'

(** [equal (=) a b] holds when [a] and [b] have the same length,
    and for every index [i], the equality [get a i = get b i] holds. *)
val equal : (ELEMENT -> ELEMENT -> bool) -> VECTOR -> VECTOR -> bool

(**TODO*)
val compare : (ELEMENT -> ELEMENT -> int) -> VECTOR -> VECTOR -> int

(**TODO*)
val of_array : ELEMENT array -> VECTOR

(**TODO*)
val of_list : ELEMENT list -> VECTOR

(**TODO*)
val to_list : VECTOR -> ELEMENT list

(**TODO*)
val of_seq : ELEMENT Seq.t -> VECTOR

  (* [to_seq] and [to_seq_rev] produce sequences which are valid only as
     long as the vector is not mutated. *)

(**TODO*)
val to_seq : VECTOR -> ELEMENT Seq.t

(**TODO*)
val to_seq_rev : VECTOR -> ELEMENT Seq.t

(**[find f v] finds the leftmost element [x] of the vector [v] such that
   [f x] is true, and returns its index. If no such element exists, then
   [Not_found] is raised. *)
val find : (ELEMENT -> bool) -> VECTOR -> int

(**[show f v] returns a textual representation of the contents of the
   vector [v]. The user-supplied function [f] is used to obtain a
   textual representation of each element. *)
val show : (ELEMENT -> string) -> VECTOR -> string

(**/**)
(**[check] is used only during testing. *)
val check : VECTOR -> unit
(**/**)

module Stack : sig
  type SYNONYM
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
