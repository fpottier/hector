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
   values. The capacity of a vector cannot exceed [Sys.max_array_length]. *)
type VECTOR

type SYNONYM = VECTOR

type length = int
type capacity = int
type index = int

(***[length v] is the (logical) length of the vector [v]. *)
val length : VECTOR -> length

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

(**[elements v] creates a new array of [length v] whose elements are the
   elements of the vector [v]. The vector [v] is unaffected. *)
val elements : VECTOR -> ELEMENT array

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
val add_last : VECTOR -> ELEMENT -> unit

(**[pop_opt v] removes and returns the last element of the vector [v].
   If the vector is empty, [None] is returned. *)
val pop_opt : VECTOR -> ELEMENT option

(**[pop_last_opt] is a synonym for [pop_opt]. *)
val pop_last_opt : VECTOR -> ELEMENT option

(**[pop v] removes and returns the last element of the vector [v].
   If the vector is empty, [Not_found] is raised. *)
val pop : VECTOR -> ELEMENT

(**[pop_last] is a synonym for [pop]. *)
val pop_last : VECTOR -> ELEMENT

(**[drop v] removes the last element of the vector [v].
   If the vector [v] is empty, [drop v] has no effect. *)
val drop : VECTOR -> unit

(**[remove_last] is a synonym for [drop]. *)
val remove_last : VECTOR -> unit

(**[peek v] returns the last element of the vector [v].
   If the vector is empty, [Not_found] is raised. *)
val peek : VECTOR -> ELEMENT

(**[get_last] is a synonym for [peek]. *)
val get_last : VECTOR -> ELEMENT

(**[peek_opt v] returns the last element of the vector [v].
   If the vector is empty, [None] is returned. *)
val peek_opt : VECTOR -> ELEMENT option

(**[find_last] is a synonym for [peek_opt]. *)
val find_last : VECTOR -> ELEMENT option

(**TODO*)
val push_array : VECTOR -> ELEMENT array -> unit

(**If [n] is less than [length v], then [truncate v n] sets the length of the
   vector [v] to [n]. Otherwise, nothing happens. In either case, the capacity
   of the vector is unchanged. This is a constant-time operation. *)
val truncate : VECTOR -> length -> unit

(**[clear v] is equivalent to [truncate 0 v]. The length of the vector becomes
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
