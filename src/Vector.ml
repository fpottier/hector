module A = Array

(* -------------------------------------------------------------------------- *)

(* Types. *)

type length = int
type capacity = int
type index = int

(* In [make] and in [set_higher_capacity], the allocation of an array of
   size [capacity] is delayed, because we do not have a value of type ['a]
   at hand. *)

type 'a vector = {

  (* The logical length of the vector. *)
  mutable length   : int;

  (* The desired physical capacity of the vector. We impose the invariant
     [length = 0 || A.length data = capacity]. That is, unless the vector
     is logically empty, [capacity] is the length of the [data] array. *)
  mutable capacity : int;

  (* The data array. *)
  mutable data     : 'a A.t;

}

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

let[@inline never] capacity_failure f capacity =
  fail "Vector.%s: capacity %d is negative" f capacity

let[@inline never] length_failure f n =
  fail "Vector.%s: length %d is negative" f n

let[@inline never] index_failure f v i =
  fail "Vector.%s: index %d is out of range [0, %d)" f i v.length

let[@inline never] get_failure v i =
  index_failure "get" v i

let[@inline never] set_failure v i =
  index_failure "set" v i

(* [validate length data] checks [length <= A.length data]. This property is
   part of our invariant, and can be violated only through racy accesses. *)

let[@inline never] violation f length data =
  fail "Vector.%s: length is %d, but data array has length %d (racy access?)"
    f length (A.length data)

let[@inline] validate f length data =
  if defensive && not (length <= A.length data) then
    violation f length data

(* -------------------------------------------------------------------------- *)

(* Construction. *)

let (* public *) make capacity =
  let length = 0 in
  let data = [||] in
  { length; capacity; data }

let[@inline] (* public *) create () =
  make 0

let (* public *) init n f =
  let length = n
  and capacity = n in
  let data = A.init capacity f in
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

let[@inline] (* public *) get v i =
  A.get v.data i

let[@inline] (* public *) set v i x =
  A.set v.data i x

(* -------------------------------------------------------------------------- *)

(* Popping, truncating, clearing. *)

exception Empty

let (* public *) pop v =
  let { length; _ } = v in
  if length > 0 then
    let i = length - 1 in
    v.length <- i;
    get v i
  else
    raise Empty

let (* public *) pop_opt v =
  let { length; _ } = v in
  if length > 0 then
    let i = length - 1 in
    v.length <- i;
    Some (get v i)
  else
    None

let (* public *) drop v =
  let { length; _ } = v in
  if length > 0 then
    let i = length - 1 in
    v.length <- i
  else
    raise Empty

let[@inline] (* public *) truncate v n =
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

let set_lower_capacity v new_capacity =
  assert (new_capacity < v.capacity);
  v.capacity <- new_capacity;
  (* If the [data] array is nonempty, then it must be truncated so as to
     match the new capacity. If it is empty, then [v.length] must be zero,
     so neither [v.length] nor [v.data] needs to be updated. *)
  let { length; data; _ } = v in
  if 0 < A.length data then begin
    v.length <- min length new_capacity;
    v.data <- A.sub data 0 new_capacity
  end

(* [really_set_higher_capacity] increases the vector's capacity and
   immediately re-allocates the [data] array so as to match the new
   capacity. The value [dummy] is used to initialize unused slots. *)

let really_set_higher_capacity v new_capacity dummy =
  assert (new_capacity > v.capacity);
  let new_data = A.make new_capacity dummy in
  A.blit v.data 0 new_data 0 v.length ;
  v.capacity <- new_capacity;
  v.data <- new_data

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
    really_set_higher_capacity v new_capacity dummy

let[@inline] (* public *) set_capacity v new_capacity =
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

(* [ensure_higher_capacity] ensures that the vector's capacity is at least
   [request]. It uses [set_higher_capacity], so the [data] array is not
   necessarily grown. *)

let ensure_higher_capacity v request =
  let { capacity; _ } = v in
  assert (request > capacity);
  let new_capacity = max (next_capacity capacity) request in
  assert (new_capacity > capacity);
  set_higher_capacity v new_capacity

let[@inline] (* public *) ensure_capacity v request =
  let { capacity; _ } = v in
  if request > capacity then
    ensure_higher_capacity v request

let[@inline] (* public *) ensure_extra_capacity v delta =
  ensure_capacity v (length v + delta)

let (* public *) fit_capacity v =
  let { length; capacity; data; _ } = v in
  if length < capacity then begin
    v.capacity <- length;
    v.data <- A.sub data 0 length
  end

(* -------------------------------------------------------------------------- *)

(* Pushing. *)

let (* public *) push v x =
  let { length; capacity; data } = v in
  (* On the fast path, one test suffices. *)
  if length < A.length data then begin
    (* A physical array slot exists. *)
    A.unsafe_set data length x; (* safe *)
    v.length <- length + 1
  end
  else if length < capacity then begin
    (* The length of the [data] array is less than [capacity], and
       must in fact be zero. The logical length of the vector must
       be zero, too. *)
    assert (A.length data < capacity);
    assert (A.length data = 0);
    assert (length = 0);
    (* Without changing the vector's capacity, allocate a new [data] array. *)
    let dummy = x in
    let data = A.make capacity dummy in
    v.data <- data;
    (* Try again. *)
    let length = 0 in
    A.unsafe_set data length x; (* safe *)
    v.length <- length + 1
  end
  else begin
    (* The [data] array is full. *)
    assert (length = capacity);
    (* Ensure that the vector's capacity is at least [length + 1]. *)
    let request = length + 1 in
    let new_capacity = max (next_capacity capacity) request in
    assert (new_capacity > capacity);
    let dummy = x in
    really_set_higher_capacity v new_capacity dummy;
    (* Try again. *)
    let { data; _ } = v in
    assert (A.length data = v.capacity);
    assert (length < v.capacity);
    A.set data length x;
    v.length <- length + 1
  end

(* -------------------------------------------------------------------------- *)

(* Iterating, searching, showing. *)

(* Calling [validate] ensures that our use [unsafe_get] is safe. *)

let[@inline] (* public *) iter f v =
  let { length; data; _ } = v in
  validate "iter" length data;
  for i = 0 to length - 1 do
    f (A.unsafe_get data i) (* safe *)
  done

let rec find f length data i =
  if i = length then
    raise Not_found
  else if f (A.unsafe_get data i) (* safe *) then
    i
  else
    find f length data (i + 1)

let (* public *) find f v =
  let { length; data; _ } = v in
  validate "find" length data;
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

let (* public *) make capacity =
  if defensive && capacity < 0 then capacity_failure "make" capacity;
  make capacity

let (* public *) init n f =
  if defensive && n < 0 then length_failure "init" n;
  init n f

let[@inline] (* public *) get v i =
  if defensive && not (0 <= i && i < v.length) then get_failure v i;
  get v i

let[@inline] (* public *) set v i x =
  if defensive && not (0 <= i && i < v.length) then set_failure v i;
  set v i x

let (* public *) truncate v n =
  if defensive && n < 0 then length_failure "truncate" n;
  truncate v n

let (* public *) set_capacity v new_capacity =
  if defensive && new_capacity < 0 then
    capacity_failure "set_capacity" new_capacity;
  set_capacity v new_capacity

let (* public *) ensure_capacity v request =
  if defensive && request < 0 then
    capacity_failure "ensure_capacity" request;
  ensure_capacity v request

let (* public *) ensure_extra_capacity v delta =
  if defensive && delta < 0 then
    capacity_failure "ensure_extra_capacity" delta;
  ensure_extra_capacity v delta
