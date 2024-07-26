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

(* We use a subset of the functionality of the [Array] module, namely
   [empty], [length], [unsafe_get], [unsafe_set], [grow], [make],
   [init], [sub], [blit_disjoint]. We take these functions from a
   module named [A], and do not make any reference to the standard
   library module [Array]. This makes our code independent of the type
   of arrays that is used as a basis for our vectors. *)

(* We assume that the macros [VECTOR], [SYNONYM], and [ARRAY] are defined.

   For polymorphic arrays, [VECTOR] is ['a vector], [SYNONYM] is ['a t],
   and [ARRAY] is ['a A.t].

   For monomorphic array, [VECTOR] is [vector], [SYNONYM] is [t],
   and [ARRAY] is [A.t]. *)

(* -------------------------------------------------------------------------- *)

(* Types. *)

type length = int
type capacity = int
type index = int

(* In [create] and in [set_higher_capacity], the allocation of an array of
   size [capacity] is delayed, because we do not have an array element at
   hand. To tolerate this, we accept the possibility that, sometimes,
   [capacity] is nonzero, while the [data] array is still empty. *)

type VECTOR = {

  (* The logical length of the vector. *)
  mutable length   : int;

  (* The desired physical capacity of the vector. We impose the invariant
     [length = 0 || A.length data = capacity]. That is, unless the vector
     is logically empty, [capacity] is the length of the [data] array. *)
  mutable capacity : int;

  (* The data array. *)
  mutable data     : ARRAY;

}

type SYNONYM =
  VECTOR

(* -------------------------------------------------------------------------- *)

(* Local copies of [min] and [max], with annotations. *)

let[@inline] min (x : int) (y : int) = if x <= y then x else y
let[@inline] max (x : int) (y : int) = if x >= y then x else y

(* -------------------------------------------------------------------------- *)

(* [check v] checks that the invariant holds. *)

let (* public *) check v =
  let { length; capacity; data } = v in
  assert (0 <= length);
  assert (length <= capacity);
  assert (length = 0 || A.length data = capacity);
  assert (A.length data = 0 || A.length data = capacity);
  (* The following assertion follows from the previous ones: *)
  assert (length <= A.length data)

(* -------------------------------------------------------------------------- *)

(* Error messages for our defensive checks. *)

(* We set [defensive] unconditionally to [true]. (We could make [defensive]
   a parameter of this module, but that would add overhead and complication.
   We could also offer two variants of the module, an optimistic one and a
   defensive one, but that would also add complication.) *)

(* Being defensive allows us to use [A.unsafe_get] and [A.unsafe_set], thus
   bypassing array bounds checks. In most places, this is safe, because our
   defensive checks are strong enough. In [get] and [set], our defensive
   checks are strong enough if the data structure is used sequentially, but
   can be insufficient if there is a data race. Thus, racy accesses to a
   vector can break memory safety! but we accept this risk. *)

let defensive =
  true

let[@inline] fail format =
  Printf.ksprintf invalid_arg format

let[@inline never] capacity_failure capacity =
  fail "invalid capacity %d" capacity

let[@inline never] length_failure n =
  fail "invalid length %d" n

let[@inline never] index_failure v i =
  fail "index %d is out of range [0, %d)" i v.length

let[@inline never] array_segment_base_failure xs ofs =
  fail "segment base index %d is out of range [0, %d]" ofs (A.length xs)

let[@inline never] array_segment_end_failure xs ofs len =
  fail "segment end index %d+%d = %d is out of range [0, %d]"
    ofs len (ofs+len) (A.length xs)

(* [validate length data] checks [length <= A.length data]. This property is
   part of our invariant, and can be violated only through racy accesses. *)

(* Every time we read both the [length] and [data] fields of a vector, we
   call [validate length data], so as to protect the user against violations
   caused by data races. (That said, [unsafe_borrow], [unsafe_get], and
   [unsafe_set], which read just the [data] field, are still unsafe.) *)

let[@inline never] violation length data =
  fail "length is %d, but data array has length %d (racy access?)"
    length (A.length data)

let[@inline] validate length data =
  if defensive && not (length <= A.length data) then
    violation length data

(* -------------------------------------------------------------------------- *)

(* Construction. *)

let[@inline] (* public *) create () =
  let length = 0
  and capacity = 0
  and data = A.empty in
  { length; capacity; data }

let[@inline] (* private *) init n f =
  let length = n
  and capacity = n in
  let data = A.init capacity f in
  { length; capacity; data }

let[@inline] (* private *) make n x =
  let length = n
  and capacity = n in
  let data = A.make capacity x in
  { length; capacity; data }

let[@inline] (* private *) unsafe_of_array_segment a o k =
  assert (0 <= o && 0 <= k && o + k <= A.length a);
  (* The length of the array segment is the capacity of the new vector. *)
  let length = k in
  let capacity = length in
  let data = A.sub a o k in
  { length; capacity; data }

let[@inline] (* public *) of_array a =
  (* The length of the original array is the capacity of the new vector. *)
  unsafe_of_array_segment a 0 (A.length a)

let[@inline] (* public *) copy v =
  (* The length of the original vector is the capacity of the new vector. *)
  let { length; data; _ } = v in
  validate length data;
  unsafe_of_array_segment data 0 length

let[@inline] (* private *) unsafe_steal_array a =
  let k = A.length a in
  let length = k in
  let capacity = length in
  let data = a in (* no copy *)
  { length; capacity; data }

let[@inline] (* public *) of_list xs =
  unsafe_steal_array (Array.of_list xs)

(* -------------------------------------------------------------------------- *)

(* Access. *)

let[@inline] (* public *) length v =
  v.length

let[@inline] (* public *) is_empty v =
  length v = 0

let[@inline] (* public *) unsafe_borrow v =
  v.data

let (* public *) to_array v =
  let { length; data; _ } = v in
  validate length data;
  A.sub data 0 length

(* In [unsafe_get] and [unsafe_set], our use of [A.unsafe_get] and
   [A.unsafe_set] is NOT completely safe. We have validated the
   index [i], but, if there is a data race, then by the time we read
   [v.data], we might find that [i] is not a valid index in this
   array. That would break memory safety! but we accept this risk. *)

(* [get] and [set] inherit this risk. *)

let[@inline] (* private *) unsafe_get v i =
  A.unsafe_get v.data i (* not entirely safe *)

let[@inline] (* private *) unsafe_set v i x =
  A.unsafe_set v.data i x (* not entirely safe *)

(* -------------------------------------------------------------------------- *)

(* Popping, peeking, truncating, clearing. *)

let (* public *) pop v =
  let { length; data; _ } = v in
  if length > 0 then begin
    validate length data;
    let i = length - 1 in
    v.length <- i;
    A.unsafe_get data i (* safe *)
  end
  else
    raise Not_found

let (* public *) pop_last =
  pop

let (* public *) pop_opt v =
  let { length; data; _ } = v in
  if length > 0 then begin
    validate length data;
    let i = length - 1 in
    v.length <- i;
    Some (A.unsafe_get data i)
  end
  else
    None

let (* public *) pop_last_opt =
  pop_opt

let (* public *) drop v =
  let { length; _ } = v in
  if length > 0 then
    let i = length - 1 in
    v.length <- i

let (* public *) remove_last =
  drop

let (* public *) top v =
  let { length; data; _ } = v in
  if length > 0 then begin
    validate length data;
    A.unsafe_get data (length - 1) (* safe *)
  end
  else
    raise Not_found

let (* public *) get_last =
  top

let (* public *) top_opt v =
  let { length; data; _ } = v in
  if length > 0 then begin
    validate length data;
    Some (A.unsafe_get data (length - 1))
  end
  else
    None

let (* public *) find_last =
  top_opt

let[@inline] (* private *) truncate v n =
  let { length; _ } = v in
  if n < length then
    v.length <- n

let[@inline] (* public *) clear v =
  v.length <- 0

let (* public *) reset v =
  v.length <- 0;
  v.capacity <- 0;
  v.data <- A.empty

(* -------------------------------------------------------------------------- *)

(* Changing the vector's capacity and/or re-allocating the [data]
   array to match its capacity, are the most tricky aspects of this
   data structure. *)

(* One must keep in mind the invariant that if [v.length] is nonzero
   then [v.capacity] is the length of the [data] array. *)

(* [set_lower_capacity] decreases the vector's capacity. *)

let[@inline] set_lower_capacity v new_capacity =
  assert (new_capacity < v.capacity);
  v.capacity <- new_capacity;
  (* If the [data] array is nonempty, then it must be truncated so as to
     match the new capacity. If it is empty, then [v.length] must be zero,
     so neither [v.length] nor [v.data] needs to be updated. *)
  let { length; data; _ } = v in
  (* No need to call [validate], as we do not access the [data] array. *)
  if 0 < A.length data then begin
    v.length <- min length new_capacity;
    v.data <- A.sub data 0 new_capacity
  end

(* [really_set_higher_capacity] increases the vector's capacity and
   immediately re-allocates the [data] array so as to match the new
   capacity. The value [dummy] is used to initialize unused slots. *)

let really_set_higher_capacity v new_capacity dummy =
  let { length; data; _ } = v in
  validate length data;
  assert (length <= new_capacity);
  v.capacity <- new_capacity;
  let new_data = A.grow new_capacity dummy data length in
  v.data <- new_data;
  new_data

(* [set_higher_capacity] increases the vector's capacity. The [data]
   array is re-allocated only if a dummy value is at hand. *)

let set_higher_capacity v new_capacity =
  let { data; _ } = v in
  if A.length data = 0 then
    (* The allocation of an array of size [capacity] is delayed,
       because we do not have a value of type ['a] at hand. *)
    v.capacity <- new_capacity
  else
    let dummy = A.unsafe_get data 0 in (* safe *)
    let _data = really_set_higher_capacity v new_capacity dummy in
    ()

let[@inline] (* private *) set_capacity v new_capacity =
  let { capacity; _ } = v in
  if new_capacity < capacity then
    set_lower_capacity v new_capacity
  else if new_capacity > capacity then
    set_higher_capacity v new_capacity

(* [next_capacity] decides by how much to increase the vector's capacity. *)

(* We jump to size at least 8 in all situations; we grow by a factor of 2
   under a certain threshold, and by a factor of 3/2 beyond this threshold. *)

(* This strategy is taken from the standard library's [Dynarray] module. *)

(* Whereas [Dynarray] ensures that the result of [next_capacity] is at most
   [Sys.max_array_length], we do not. On 64 bit machines, the value of
   [Sys.max_array_length] is so high (about 128,000 terabytes) that this
   limit is unlikely to be exceeded. If (in the future) we wanted to take
   explicit precautions about this limit, we should make it a field in the
   module [A], so as to remain independent of the details of OCaml's arrays. *)

let[@inline] next_capacity capacity =
  max 8 (
    if capacity <= 512 then capacity * 2
    else capacity + capacity / 2
  )

let[@inline] (* private *) ensure_capacity v request =
  let { capacity; _ } = v in
  if request > capacity then begin
    (* Ensure that the vector's capacity is at least [request]. We use
       [set_higher_capacity], so the [data] array is not necessarily grown. *)
    let new_capacity = max (next_capacity capacity) request in
    assert (new_capacity >= request);
    set_higher_capacity v new_capacity
  end

let[@inline] (* private *) ensure_extra_capacity v delta =
  ensure_capacity v (length v + delta)

let (* public *) fit_capacity v =
  let { length; capacity; _ } = v in
  if length < capacity then
    set_lower_capacity v length

(* [really_ensure_capacity v request dummy] ensures that the requested
   capacity [request] really is available now in the [data] array, not
   just recorded in the [capacity] field. *)

let[@inline never] (* private *) really_ensure_capacity v request dummy =
  let { capacity; _ } = v in
  let new_capacity =
    if request <= capacity then capacity
    else max (next_capacity capacity) request
  in
  assert (new_capacity >= request);
  really_set_higher_capacity v new_capacity dummy

(* -------------------------------------------------------------------------- *)

(* Pushing. *)

(* We separate the slow path (the unusual case) so that the fast path
   (the common case) can be marked [@inline]. On the fast path, one test
   suffices. *)

(* The macro [DATA(DUMMY)] returns a [data] array whose length is at
   least [new_length]. The expression [DUMMY] is evaluated only if
   there is insufficient space in the current [data] array. *)

#undef  DATA
#define DATA(DUMMY) ( \
  if new_length <= A.length data then data \
  else really_ensure_capacity v new_length (DUMMY) \
)

(* Calling [validate] is not necessary here; the code is written in
   such a way that our accesses to the [data] array are safe. *)

let[@inline] (* public *) push v x =
  let { length; data; _ } = v in
  (* Ensure that sufficient space exists in the [data] array. *)
  (* [x] is used as a dummy value. *)
  let new_length = length + 1 in
  let data = DATA(x) in
  (* A physical array slot now exists. *)
  A.unsafe_set data (new_length - 1) x; (* safe *)
  v.length <- new_length

let (* public *) add_last =
  push

let[@inline] (* private *) unsafe_push_array_segment v xs ofs len =
  assert (0 <= ofs && 0 <= len && ofs + len <= A.length xs);
  let { length; data; _ } = v in
  let new_length = length + len in
  (* Ensure that sufficient space exists in the [data] array. *)
  (* If there is insufficient space, then it must be the case
     that [len] is nonzero, so reading [xs.(0)] is safe. *)
  let data = DATA(assert (0 < len); A.unsafe_get xs 0 (* safe *)) in
  (* Physical array slots now exist. *)
  v.length <- new_length;
  A.blit_disjoint xs ofs data length len

let[@inline] (* public *) push_array v xs =
  let ofs = 0
  and len = A.length xs in
  unsafe_push_array_segment v xs ofs len

let (* public *) append_array =
  push_array

let[@inline] (* public *) push_vector v v' =
  let { data = xs; length = len; _ } = v' in
  let ofs = 0 in
  unsafe_push_array_segment v xs ofs len
    (* This works even if [v] and [v'] are the same vector. In all cases, we
       are reading from a data array and writing to a data array (which may
       or may not be the same array), and the source and destination ranges
       are disjoint. *)

let (* public *) append =
  push_vector

let (* public *) push_list v xs =
  let len = List.length xs in
  let { length; data; _ } = v in
  let new_length = length + len in
  (* Ensure that sufficient space exists in the [data] array. *)
  (* If there is insufficient space, then it must be the case
     that [len] is nonzero, so calling [List.hd xs] is safe. *)
  let data = DATA(assert (0 < len); List.hd xs (* safe *)) in
  (* Physical array slots now exist. *)
  v.length <- new_length;
  (* We want to blit the list [xs], whose length is [len], into the array
     [data] at offset [length]. This can be done with a loop. *)
  let xs = ref xs
  and dst = ref length in
  LOOP5(_, 0, len,
    match !xs with [] -> assert false | x :: rest ->
    A.unsafe_set data !dst x; (* safe *)
    dst := !dst + 1;
    xs := rest
  );
  assert (!xs = [])

let (* public *) append_list =
  push_list

let (* public *) push_seq v xs =
  push_list v (List.of_seq xs)
    (* I do not feel the need to optimize this function for speed. *)

let (* public *) append_seq =
  push_seq

let (* public *) of_seq xs =
  let v = create() in push_seq v xs; v
    (* I do not feel the need to optimize this function for speed. *)

(* In [push_iter], assuming that we are not allowed to call [iter] twice,
   the best and simplest implementation is to just iterate [push]. Using
   temporary storage would not help. *)

(* If we are allowed to call [iter] twice, then we can call it once to
   count how many elements must be appended, and call it again to
   actually write these elements into the vector. This might be more
   efficient in some cases, but that is not entirely clear. *)

(* We want to be compatible with [Dynarray], whose documentation does
   not indicate whether [push_iter] may call [iter] more than once. For
   maximum compatibility, we must assume that this is not permitted. *)

let[@inline] (* public *) push_iter v iter c =
  iter (push v) c

let (* public *) append_iter =
  push_iter

(* -------------------------------------------------------------------------- *)

(* Operations on array segments. *)

module ArraySegment = struct
  #include "ArraySegment.frag.ml"
end

(* -------------------------------------------------------------------------- *)

(* Iterating, searching, showing. *)

(* Calling [validate] ensures that our use of [unsafe_get] is safe. *)

let (* public *) iter f v =
  let { length; data; _ } = v in
  validate length data;
  ArraySegment.iter f data 0 length

let (* public *) iter_down f v =
  let { length; data; _ } = v in
  validate length data;
  ArraySegment.iter_down f data 0 length

let (* public *) iteri f v =
  let { length; data; _ } = v in
  validate length data;
  ArraySegment.iteri f data 0 length

let (* public *) map f v =
  let { length; data; _ } = v in
  validate length data;
  init length (fun i -> f (A.unsafe_get data i) (* safe *))

let (* public *) mapi f v =
  let { length; data; _ } = v in
  validate length data;
  init length (fun i -> f i (A.unsafe_get data i) (* safe *))

let (* public *) fold_left f accu v =
  let { length; data; _ } = v in
  validate length data;
  ArraySegment.fold_left f accu data 0 length

let (* public *) fold_right f v accu =
  let { length; data; _ } = v in
  validate length data;
  ArraySegment.fold_right f data 0 length accu

let (* public *) to_list v =
  let { length; data; _ } = v in
  validate length data;
  ArraySegment.to_list data 0 length

let (* public *) to_seq v =
  let { length; data; _ } = v in
  validate length data;
  ArraySegment.to_seq data 0 length

let (* public *) to_seq_rev v =
  let { length; data; _ } = v in
  validate length data;
  ArraySegment.to_seq_rev data 0 length

let (* public *) exists f v =
  let { length; data; _ } = v in
  validate length data;
  ArraySegment.exists f data 0 length

let (* public *) for_all f v =
  let { length; data; _ } = v in
  validate length data;
  ArraySegment.for_all f data 0 length

let (* public *) filter f v =
  let { length; data; _ } = v in
  validate length data;
  let v' = create() in
  LOOP5(i, 0, length,
    let x = A.unsafe_get data i (* safe *) in
    if f x then push v' x);
  v'

let (* public *) filter_map f v =
  let { length; data; _ } = v in
  validate length data;
  let v' = create() in
  LOOP5(i, 0, length,
    let x = A.unsafe_get data i (* safe *) in
    match f x with Some y -> push v' y | None -> ());
  v'

let (* public *) equal equal v1 v2 =
  let { length = length1; data = data1; _ } = v1
  and { length = length2; data = data2; _ } = v2 in
  length1 = length2 &&
  let () = validate length1 data1
  and () = validate length2 data2 in
  ArraySegment.equal equal data1 0 length1 data2 0 length2

let (* public *) compare compare v1 v2 =
  let { length = length1; data = data1; _ } = v1
  and { length = length2; data = data2; _ } = v2 in
  validate length1 data1;
  validate length2 data2;
  ArraySegment.compare compare data1 0 length1 data2 0 length2

let (* public *) find f v =
  let { length; data; _ } = v in
  validate length data;
  ArraySegment.find f data 0 length

let (* public *) show show v =
  let opening, separator, closing = "[|", "; ", "|]" in
  let b = Buffer.create 32 in
  Buffer.add_string b opening;
  let first = ref true in
  iter (fun x ->
    if not !first then Buffer.add_string b separator;
    Buffer.add_string b (show x);
    first := false
  ) v;
  Buffer.add_string b closing;
  Buffer.contents b

(* -------------------------------------------------------------------------- *)

(* Wrap the public functions that need defensive checks. *)

let (* public *) init n f =
  if defensive && n < 0 then length_failure n;
  init n f

let (* public *) make n x =
  if defensive && n < 0 then length_failure n;
  make n x

let[@inline] (* public *) get v i =
  if defensive && not (0 <= i && i < v.length) then index_failure v i;
  unsafe_get v i

let[@inline] (* public *) set v i x =
  if defensive && not (0 <= i && i < v.length) then index_failure v i;
  unsafe_set v i x

let (* public *) truncate v n =
  if defensive && n < 0 then length_failure n;
  truncate v n

let (* public *) set_capacity v new_capacity =
  if defensive && new_capacity < 0 then capacity_failure new_capacity;
  set_capacity v new_capacity

let (* public *) ensure_capacity v request =
  if defensive && request < 0 then capacity_failure request;
  ensure_capacity v request

let (* public *) ensure_extra_capacity v delta =
  if defensive && delta < 0 then capacity_failure delta;
  ensure_extra_capacity v delta

let[@inline] (* public *) push_array_segment v xs ofs len =
  if defensive && not (0 <= ofs && ofs <= A.length xs) then
    array_segment_base_failure xs ofs;
  if defensive && not (0 <= len) then
    length_failure len;
  if defensive && not (0 <= ofs+len && ofs+len <= A.length xs) then
    array_segment_end_failure xs ofs len;
  unsafe_push_array_segment v xs ofs len

let (* public *) append_array_segment =
  push_array_segment

(* -------------------------------------------------------------------------- *)

(* An emulation of the [Stack] API. *)

module Stack = struct

  type SYNONYM = VECTOR

  (* Our functions [pop] and [top] raise [Not_found] if the vector is empty. *)
  exception Empty = Not_found

  let create = create
  let[@inline] push x v = push v x (* reversed arguments *)
  let pop = pop
  let pop_opt = pop_opt

  (* Our [drop] does nothing if the vector is empty. *)
  let drop v = if is_empty v then raise Empty else drop v

  let top = top
  let top_opt = top_opt
  let clear = clear
  let copy = copy
  let is_empty = is_empty
  let length = length

  (* The stack API offers iteration from top to bottom. *)
  let iter = iter_down

  (* [Stack.fold] has the type of a [fold_left] function,
     but iterates from top to bottom. *)
  let fold f accu v =
    let { length; data; _ } = v in
    validate length data;
    let accu = ref accu in
    LOOP_DOWN(i, 0, length, accu := f !accu (A.unsafe_get data i));
    !accu

  (* [Stack.to_seq] takes a snapshot (at no cost) of the stack when
     it is called, so the stack can be modified, without affecting
     iteration. We simulate this (at a cost) by making a copy. *)
  let to_seq v = to_seq_rev (copy v)
  let add_seq = push_seq
  let of_seq = of_seq

end
