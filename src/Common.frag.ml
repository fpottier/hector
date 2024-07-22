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
   [length], [unsafe_get], [unsafe_set].

   Furthermore, we use [alloc], [make], [init], [sub], [blit]. We take
   these functions from a module named [A]. This allows them to be
   possibly redefined.

   We do *NOT* assume that [A.alloc n x] initializes every array slot
   with the value [x]. In fact, in this file, every call to [A.alloc]
   is of the form [A.alloc n dummy], where [dummy] is a dummy
   value. *)

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
  assert (length = 0 || Array.length data = capacity);
  assert (Array.length data = 0 || Array.length data = capacity);
  (* The following assertion follows from the previous ones: *)
  assert (length <= Array.length data)

(* -------------------------------------------------------------------------- *)

(* Error messages for our defensive checks. *)

(* We set [defensive] unconditionally to [true]. (We could make [defensive]
   a parameter of this module, but that would add overhead and complication.
   We could also offer two variants of the module, an optimistic one and a
   defensive one, but that would also add complication.) *)

(* Being defensive allows us to use [Array.unsafe_get] and [Array.unsafe_set], thus
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
  fail "segment base index %d is out of range [0, %d]" ofs (Array.length xs)

let[@inline never] array_segment_end_failure xs ofs len =
  fail "segment end index %d+%d = %d is out of range [0, %d]"
    ofs len (ofs+len) (Array.length xs)

(* [validate length data] checks [length <= Array.length data]. This property is
   part of our invariant, and can be violated only through racy accesses. *)

let[@inline never] violation length data =
  fail "length is %d, but data array has length %d (racy access?)"
    length (Array.length data)

let[@inline] validate length data =
  if defensive && not (length <= Array.length data) then
    violation length data

(* -------------------------------------------------------------------------- *)

(* Construction. *)

let[@inline] (* public *) create () =
  let length = 0
  and capacity = 0
  and data = [||] in
  { length; capacity; data }

let (* private *) init n f =
  let length = n
  and capacity = n in
  let data = A.init capacity f in
  { length; capacity; data }

let (* private *) make n x =
  let length = n
  and capacity = n in
  let data = A.make capacity x in
  { length; capacity; data }

let (* public *) copy v =
  (* The length of the original vector is the capacity of the new vector. *)
  let { length; data; _ } = v in
  let capacity = length in
  let data = A.sub data 0 length in
  { length; capacity; data }

(* -------------------------------------------------------------------------- *)

(* Access. *)

let[@inline] (* public *) length v =
  v.length

let[@inline] (* public *) is_empty v =
  length v = 0

let (* public *) elements v =
  let { length; data; _ } = v in
  A.sub data 0 length

(* In [unsafe_get] and [unsafe_set], our use of [Array.unsafe_get] and
   [Array.unsafe_set] is NOT completely safe. We have validated the
   index [i], but, if there is a data race, then by the time we read
   [v.data], we might find that [i] is not a valid index in this
   array. That would break memory safety! but we accept this risk. *)

(* [get] and [set] inherit this risk. *)

let[@inline] (* private *) unsafe_get v i =
  Array.unsafe_get v.data i (* not entirely safe *)

let[@inline] (* private *) unsafe_set v i x =
  Array.unsafe_set v.data i x (* not entirely safe *)

(* -------------------------------------------------------------------------- *)

(* Popping, peeking, truncating, clearing. *)

let (* public *) pop v =
  let { length; _ } = v in
  if length > 0 then
    let i = length - 1 in
    v.length <- i;
    unsafe_get v i
  else
    raise Not_found

let (* public *) pop_last =
  pop

let (* public *) pop_opt v =
  let { length; _ } = v in
  if length > 0 then
    let i = length - 1 in
    v.length <- i;
    Some (unsafe_get v i)
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

let (* public *) peek v =
  let { length; _ } = v in
  if length > 0 then
    unsafe_get v (length - 1)
  else
    raise Not_found

let (* public *) get_last =
  peek

let (* public *) peek_opt v =
  let { length; _ } = v in
  if length > 0 then
    Some (unsafe_get v (length - 1))
  else
    None

let (* public *) find_last =
  peek_opt

let[@inline] (* private *) truncate v n =
  let { length; _ } = v in
  if n < length then
    v.length <- n

let[@inline] (* public *) clear v =
  v.length <- 0

let (* public *) reset v =
  v.length <- 0;
  v.capacity <- 0;
  v.data <- [||]

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
  if 0 < Array.length data then begin
    v.length <- min length new_capacity;
    v.data <- A.sub data 0 new_capacity
  end

(* [really_set_higher_capacity] increases the vector's capacity and
   immediately re-allocates the [data] array so as to match the new
   capacity. The value [dummy] is used to initialize unused slots. *)

let really_set_higher_capacity v new_capacity dummy : ELEMENT array =
  let { length; data; _ } = v in
  assert (length <= new_capacity);
  let new_data = A.alloc new_capacity dummy in
  A.blit data 0 new_data 0 length;
  v.capacity <- new_capacity;
  v.data <- new_data;
  new_data

(* [set_higher_capacity] increases the vector's capacity. The [data]
   array is re-allocated only if a dummy value is at hand. *)

let set_higher_capacity v new_capacity =
  let { data; _ } = v in
  if Array.length data = 0 then
    (* The allocation of an array of size [capacity] is delayed,
       because we do not have a value of type ['a] at hand. *)
    v.capacity <- new_capacity
  else
    let dummy = Array.unsafe_get data 0 in (* safe *)
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

let[@inline never] (* private *) really_ensure_capacity v request dummy
: ELEMENT array =
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

let[@inline] (* public *) push v x =
  let { length; data; _ } = v in
  (* Ensure that sufficient space exists in the [data] array. *)
  let data =
    if length < Array.length data then
      data
    else
      let dummy = x in
      really_ensure_capacity v (length + 1) dummy
  in
  (* A physical array slot now exists. *)
  Array.unsafe_set data length x; (* safe *)
  v.length <- length + 1

let (* public *) add_last =
  push

let[@inline] (* private *) unsafe_push_array_segment v xs ofs len =
  assert (0 <= ofs && 0 <= len && ofs + len <= Array.length xs);
  let { length; data; _ } = v in
  let new_length = length + len in
  (* Ensure that sufficient space exists in the [data] array. *)
  let data =
    if new_length <= Array.length data then
      data
    else
      (* If there is insufficient space, then it must be the case
         that [len] is nonzero, so reading [xs.(0)] is safe. *)
      let dummy = assert (0 < len); Array.unsafe_get xs 0 (* safe *) in
      really_ensure_capacity v new_length dummy
  in
  (* Physical array slots now exist. *)
  v.length <- new_length;
  A.blit xs ofs data length len

let[@inline] (* public *) push_array v xs =
  let ofs = 0
  and len = Array.length xs in
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
  let data =
    if new_length <= Array.length data then
      data
    else
      (* If there is insufficient space, then it must be the case
         that [len] is nonzero, so calling [List.hd xs] is safe. *)
      let dummy = assert (0 < len); List.hd xs (* safe *) in
      really_ensure_capacity v new_length dummy
  in
  (* Physical array slots now exist. *)
  v.length <- new_length;
  (* We want to blit the list [xs], whose length is [len], into the array
     [data] at offset [length]. This can be done with a loop. *)
  let xs = ref xs
  and dst = ref length in
  for _ = 1 to len do
    match !xs with [] -> assert false | x :: rest ->
    Array.unsafe_set data !dst x; (* safe *)
    dst := !dst + 1;
    xs := rest
  done;
  assert (!xs = [])

let (* public *) append_list =
  push_list

(* -------------------------------------------------------------------------- *)

(* Iterating, searching, showing. *)

(* Calling [validate] ensures that our use of [unsafe_get] is safe. *)

let (* public *) iter f v =
  let { length; data; _ } = v in
  validate length data;
  LOOP5(i, 0, length, f (Array.unsafe_get data i) (* safe *))

let (* public *) iteri f v =
  let { length; data; _ } = v in
  validate length data;
  LOOP5(i, 0, length, f i (Array.unsafe_get data i) (* safe *))

let rec find f length (data : ELEMENT array) i =
  if i = length then
    raise Not_found
  else if f (Array.unsafe_get data i) (* safe *) then
    i
  else
    find f length data (i + 1)

let (* public *) find f v =
  let { length; data; _ } = v in
  validate length data;
  find f length data 0

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
  if defensive && not (0 <= ofs && ofs <= Array.length xs) then
    array_segment_base_failure xs ofs;
  if defensive && not (0 <= len) then
    length_failure len;
  if defensive && not (0 <= ofs+len && ofs+len <= Array.length xs) then
    array_segment_end_failure xs ofs len;
  unsafe_push_array_segment v xs ofs len

let (* public *) append_array_segment =
  push_array_segment
