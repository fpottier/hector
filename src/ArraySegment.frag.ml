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

(* -------------------------------------------------------------------------- *)

(* Our stable sort algorithm is taken from OCaml's [Stdlib.Array] and adapted
   to use our array module [A] and to sort an array segment. *)

#undef  ALLOC
#define ALLOC A.alloc
#undef  GET
#define GET   A.unsafe_get
#undef  SET
#define SET   A.unsafe_set
#undef  BLIT
#define BLIT  A.blit

(* [merge cmp src1 src1ofs src1len src2 src2ofs src2len dst dstofs] merges
   the sorted array segments described by [src1], [src1ofs], [src1len] and
   [src2], [src2ofs], [src2len]. The resulting data is written into the
   array segment described by [dst], [dstofs], and [src1len + src2len].
   One of the source segments (say, the first one) must be disjoint with
   the destination segment. The other source segment (say, the second one)
   can be either disjoint with the destination segment or a suffix of the
   destination segment. (This works because the data in the second source
   segment is then moved left: it is read before it is overwritten.) *)

let merge cmp src1 src1ofs src1len src2 src2ofs src2len dst dstofs =
  let src1r = src1ofs + src1len
  and src2r = src2ofs + src2len in
  let rec loop i1 s1 i2 s2 d =
    if cmp s1 s2 <= 0 then begin
      SET dst d s1;
      let i1 = i1 + 1 in
      if i1 < src1r then
        loop i1 (GET src1 i1) i2 s2 (d + 1)
      else
        BLIT src2 i2 dst (d + 1) (src2r - i2)
    end else begin
      SET dst d s2;
      let i2 = i2 + 1 in
      if i2 < src2r then
        loop i1 s1 i2 (GET src2 i2) (d + 1)
      else
        BLIT src1 i1 dst (d + 1) (src1r - i1)
    end
  in
  loop src1ofs (GET src1 src1ofs) src2ofs (GET src2 src2ofs) dstofs

(* [isortto cmp src srcofs dst dstofs len] sorts the array segment described
   by [src], [srcofs], [len]. The resulting data is written into the array
   segment described by [dst], [dstofs], [len]. The destination segment must
   be disjoint from the source segment. This is an insertion sort. *)

let isortto cmp src srcofs dst dstofs len =
  for i = 0 to len - 1 do
    let e = GET src (srcofs + i) in
    let j = ref (dstofs + i - 1) in
    while !j >= dstofs && cmp (GET dst !j) e > 0 do
      SET dst (!j + 1) (GET dst !j);
      decr j
    done;
    SET dst (!j + 1) e
  done

(* The cutoff determines where we switch from merge sort to insertion sort.
   OCaml uses 5. I find 8 to be slightly better, but the difference is very
   small: about 2%. The best setting is likely machine-dependent. *)

let cutoff = 8

(* [sortto cmp src srcofs dst dstofs len] sorts the array segment described
   by [src], [srcofs], [len]. The resulting data is written into the array
   segment described by [dst], [dstofs], [len]. The destination segment must
   be disjoint from the source segment. This is a merge sort, with an
   insertion sort at the leaves. *)

let rec sortto cmp src srcofs dst dstofs len =
  if len <= cutoff then
    isortto cmp src srcofs dst dstofs len
  else begin
    let len1 = len / 2 in
    let len2 = len - len1 in
    (* The second half of [src] can be larger by one slot. *)
    assert (len1 <= len2 && len2 <= len1 + 1);
    (* Sort the second half of [src] and move it to the second half of [dst]. *)
    sortto cmp src (srcofs + len1) dst (dstofs + len1) len2;
    (* Sort the first half of [src] and move it to the second half of [src]. *)
    (* This requires [len1 <= len2]. *)
    sortto cmp src srcofs src (srcofs + len2) len1;
    (* Merge the two sorted halves, moving the data to [dst]. *)
    (* This is an in-place merge: the second source segment is contained
       within the destination segment! *)
    merge cmp src (srcofs + len2) len1 dst (dstofs + len1) len2 dst dstofs
  end

(* [unsafe_stable_sort_segment cmp a ofs len] sorts the array segment
   described by [a], [ofs], [len]. This array segment is sorted in place.
   This function is named [unsafe] because it does not validate [ofs] and
   [len]. *)

let unsafe_stable_sort cmp a ofs len =
  let base = ofs in
  if len <= cutoff then isortto cmp a base a base len else begin
    let len1 = len / 2 in
    let len2 = len - len1 in
    let t = ALLOC len2 (GET a base) in
    sortto cmp a (base + len1) t 0 len2;
    sortto cmp a base a (base + len2) len1;
    merge cmp a (base + len2) len1 t 0 len2 a base;
  end
