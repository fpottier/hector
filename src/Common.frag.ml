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

(* Popping, truncating, clearing. *)

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

(* Changing the vector's capacity
   and/or re-allocating the vector's [data] array to match its capacity,
   are the most tricky aspects of this data structure. *)

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

let really_set_higher_capacity v new_capacity dummy =
  let { length; capacity; data } = v in
  assert (new_capacity > capacity);
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

(* -------------------------------------------------------------------------- *)

(* Pushing. *)

(* We separate the slow paths (that is, the unusual cases) so that the fast
   path (the common case) can be marked [@inline]. *)

let[@inline never] push_slow_path v x =
  let { length; capacity; data } = v in
  assert (not (length < Array.length data));
  if length < capacity then begin
    (* The length of the [data] array is less than [capacity], and
       must in fact be zero. The logical length of the vector must
       be zero, too. *)
    assert (Array.length data < capacity);
    assert (Array.length data = 0);
    assert (length = 0);
    (* Without changing the vector's capacity, allocate a new [data] array. *)
    let dummy = x in
    let data = A.alloc capacity dummy in
    v.data <- data;
    (* Try again. *)
    let length = 0 in
    Array.unsafe_set data length x; (* safe *)
    v.length <- length + 1
  end
  else begin
    (* The [data] array is full. *)
    assert (length = capacity);
    (* Ensure that the vector's capacity is at least [length + 1]. *)
    let request = length + 1 in
    let new_capacity = max (next_capacity capacity) request in
    assert (new_capacity > capacity);
    assert (length < new_capacity);
    let dummy = x in
    let data = really_set_higher_capacity v new_capacity dummy in
    (* Try again. *)
    assert (v.capacity = new_capacity);
    assert (Array.length data = v.capacity);
    Array.unsafe_set data length x; (* safe *)
    v.length <- length + 1
  end

let[@inline] (* public *) push v x =
  let { length; data; _ } = v in
  (* On the fast path, one test suffices. *)
  if length < Array.length data then begin
    (* A physical array slot exists. *)
    Array.unsafe_set data length x; (* safe *)
    v.length <- length + 1
  end
  else
    push_slow_path v x

let (* public *) add_last =
  push

(* -------------------------------------------------------------------------- *)

(* Iterating, searching, showing. *)

(* Calling [validate] ensures that our use of [unsafe_get] is safe. *)

let[@inline] (* public *) iter f v =
  let { length; data; _ } = v in
  validate length data;
  LOOP5(i, 0, length, f (Array.unsafe_get data i) (* safe *))

let[@inline] (* public *) iteri f v =
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
