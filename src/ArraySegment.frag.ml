#undef  VALIDATE
#define VALIDATE(a, i, j) \
  assert (0 <= i && i <= j && i <= A.length a)

let[@inline] iter f a i j =
  VALIDATE(a, i, j);
  LOOP5(i, i, j, f (A.unsafe_get a i))

let[@inline] iter_down f a i j =
  VALIDATE(a, i, j);
  LOOP_DOWN(i, i, j, f (A.unsafe_get a i))

let[@inline] iteri f a i j =
  VALIDATE(a, i, j);
  LOOP5(i, i, j, f i (A.unsafe_get a i))

let[@inline] fold_left f accu a i j =
  VALIDATE(a, i, j);
  let accu = ref accu in
  LOOP5(i, i, j, accu := f !accu (A.unsafe_get a i));
  !accu

let[@inline] fold_right f a i j accu =
  VALIDATE(a, i, j);
  let accu = ref accu in
  LOOP_DOWN(i, i, j, accu := f (A.unsafe_get a i) !accu);
  !accu

let[@inline] to_list a i j =
  VALIDATE(a, i, j);
  let accu = ref [] in
  LOOP_DOWN(i, i, j, accu := A.unsafe_get a i :: !accu);
  !accu

let rec to_seq a i j =
  VALIDATE(a, i, j);
  if i = j then
    Seq.empty
  else
    fun () -> Seq.Cons (A.unsafe_get a i, to_seq a (i + 1) j)

let rec to_seq_rev a i j =
  VALIDATE(a, i, j);
  if i = j then
    Seq.empty
  else
    fun () ->
      let j = j - 1 in
      Seq.Cons (A.unsafe_get a j, to_seq_rev a i j)

let[@inline] exists f a i j =
  VALIDATE(a, i, j);
  let exception Break in
  try
    LOOP5(i, i, j, if f (A.unsafe_get a i) then raise Break);
    false
  with Break ->
    true

let[@inline] for_all f a i j =
  VALIDATE(a, i, j);
  let exception Break in
  try
    LOOP5(i, i, j, if not (f (A.unsafe_get a i)) then raise Break);
    true
  with Break ->
    false

let[@inline] equal equal a1 i1 j1 a2 i2 j2 =
  VALIDATE(a1, i1, j1);
  VALIDATE(a2, i2, j2);
  let length1 = j1 - i1
  and length2 = j2 - i2 in
  length1 = length2 &&
  let exception Break in
  try
    let delta = i2 - i1 in
    LOOP(i, i1, j1,
      if not (equal (A.unsafe_get a1 i) (A.unsafe_get a2 (i + delta))) then
        raise Break
    );
    true
  with Break ->
    false

let[@inline] compare compare a1 i1 j1 a2 i2 j2 =
  VALIDATE(a1, i1, j1);
  VALIDATE(a2, i2, j2);
  let length1 = j1 - i1
  and length2 = j2 - i2 in
  let exception Break of int in
  try
    let delta = i2 - i1 in
    LOOP(i, i1, i1 + min length1 length2,
      let c = compare (A.unsafe_get a1 i) (A.unsafe_get a2 (i + delta)) in
      if c <> 0 then
        raise (Break c)
    );
    Stdlib.Int.compare length1 length2
  with Break c ->
    c

let rec find f a i j =
  VALIDATE(a, i, j);
  if i = j then
    raise Not_found
  else if f (A.unsafe_get a i) then
    i
  else
    find f a (i + 1) j
