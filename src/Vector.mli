(**A vector holds a sequence of values. The length of the vector is the
   length of this sequence. The capacity of the vector is at least equal
   to its length, and can be greater than its length.

   This data structure does {i not} include a protection against memory
   leaks. If a vector's capacity is greater than its length, then the
   logically empty slots in the data array can contain stale values, which
   in the eyes of the GC remain reachable. This problem can be avoided by
   explicitly calling [reset] or [fit_capacity].

   This data structure is {i not} thread-safe. Concurrent read accesses by
   multiple threads are safe. Concurrent accesses, where at least one thread
   attempts to modify the data structure, are unsafe and can compromise memory
   safety. *)
type 'a vector

type length = int
type capacity = int
type index = int

(***[length v] is the (logical) length of the vector [v]. *)
val length : 'a vector -> length

(**[s_empty v] is equivalent to [length v = 0]. *)
val is_empty : 'a vector -> bool

(**[make capacity] creates a new vector of length 0 and capacity
   [capacity]. *)
val make : capacity -> 'a vector

(**[create()] creates a new vector of length 0 and capacity 0.
   It is equivalent to [make 0]. *)
val create : unit -> 'a vector

(**[init n f] creates a new vector of length and capacity [n]
   and initializes it by storing the value [f i] at each index [i]. *)
val init : length -> (index -> 'a) -> 'a vector

(**[copy v] creates a new vector of length and capacity [length v]
   and initializes it with a copy of the data stored in [v]. *)
val copy : 'a vector -> 'a vector

(**[elements v] creates a new array of [length v] whose elements are the
   elements of the vector [v]. The vector [v] is unaffected. *)
val elements : 'a vector -> 'a array

(**[get v i] fetches the element at index [i] in vector [v].
   [i] must be comprised in the semi-open interval of 0 to [length v]. *)
val get : 'a vector -> index -> 'a

(**[set v i x] overwrites the element at index [i] in vector [v] with the
   value [x]. [i] must be comprised in the semi-open interval of 0 to
   [length v]. *)
val set : 'a vector -> index -> 'a -> unit

(**[push v x] appends the element [x] at the end of the vector [v],
   that is, at offset [length v]. If necessary, the capacity of the
   vector is increased. *)
val push : 'a vector -> 'a -> unit

(**[pop_opt v] removes and returns the last element of the vector [v].
   If the vector is empty, [None] is returned. *)
val pop_opt : 'a vector -> 'a option

(**[Empty] is raised by [pop] and [drop]. *)
exception Empty

(**[pop v] removes and returns the last element of the vector [v].
   If the vector is empty, [Empty] is raised. *)
val pop : 'a vector -> 'a

(**[drop v] removes the last element of the vector [v].
   If the vector is empty, [Empty] is raised. *)
val drop : 'a vector -> unit

(**If [n] is less than [length v], then [truncate v n] sets the length of the
   vector [v] to [n]. Otherwise, nothing happens. In either case, the capacity
   of the vector is unchanged. This is a constant-time operation. *)
val truncate : 'a vector -> length -> unit

(**[clear v] is equivalent to [truncate 0 v]. The length of the vector becomes
   zero; its capacity remains unchanged. *)
val clear : 'a vector -> unit

(**[reset v] sets both the length and the capacity of the vector [v] to zero. *)
val reset : 'a vector -> unit

(**[ensure_capacity v c] ensures that the capacity of the vector [v]
   is at least [c]. *)
val ensure_capacity : 'a vector -> capacity -> unit

(**[ensure_capacity v delta] ensures that the capacity of the vector [v] is at
   least [length v + delta]. The increment [delta] must be nonnegative. *)
val ensure_extra_capacity : 'a vector -> capacity -> unit

(**[fit_capacity v] ensures that the capacity of the vector [v] matches
   its length. If necessary, the capacity of the vector is decreased. *)
val fit_capacity : 'a vector -> unit

(**[set_capacity v c] ensures that the capacity of the vector [v] is exactly
   [c]. If [c] is less than [length v], then the vector is truncated: that is,
   some elements are lost. Otherwise, the elements of the vector are
   preserved, and the capacity of the vector is decreased or increased as
   necessary. *)
val set_capacity : 'a vector -> capacity -> unit

(**[iter f v] applies the function [f] in turn, from left to right, to each
   element of the vector [v]. *)
val iter : ('a -> unit) -> 'a vector -> unit

(**[find f v] finds the leftmost element [x] of the vector [v] such that
   [f x] is true, and returns its index. If no such element exists, then
   [Not_found] is raised. *)
val find : ('a -> bool) -> 'a vector -> int

(**[show f v] returns a textual representation of the contents of the
   vector [v]. The user-supplied function [f] is used to obtain a
   textual representation of each element. *)
val show : ('a -> string) -> 'a vector -> string

(**/**)
(**[check] is used only during testing. *)
val check : 'a vector -> unit
(**/**)
