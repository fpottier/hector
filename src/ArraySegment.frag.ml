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

(* [disjoint a1 ofs1 len1 a2 ofs2 len2] tests whether the array segments
   described by [a1], [ofs1], [len1] and [a2], [ofs2], [len2] are disjoint.
   It is used only in assertions. *)

let disjoint a1 ofs1 len1 a2 ofs2 len2 =
  a1 != a2 ||
  ofs1 + len1 <= ofs2 ||
  ofs2 + len2 <= ofs1

(* [suffix a1 ofs1 len1 a2 ofs2 len2] tests whether the array segment
   described by [a1], [ofs1], [len1] is a suffix of the array segment
   described by [a2], [ofs2], [len2]. It is used only in assertions. *)

let suffix a1 ofs1 len1 a2 ofs2 len2 =
  a1 == a2 &&
  ofs1 + len1 = ofs2 + len2 &&
  len1 <= len2

(* If the input data is already sorted, or close to sorted, it may be the
   case that the calls from [merge] (below) to [blit] copy an array segment
   to itself. We recognize this situation, where there is nothing to do. *)

let[@inline] terminal_blit src srcofs dst dstofs len =
  if src == dst && srcofs = dstofs then
    ()
  else
    A.unsafe_blit src srcofs dst dstofs len

(* [merge cmp src1 src1ofs src1len src2 src2ofs src2len dst dstofs] merges
   the sorted array segments described by [src1], [src1ofs], [src1len] and
   [src2], [src2ofs], [src2len]. The resulting data is written into the
   array segment described by [dst], [dstofs], and [src1len + src2len].
   One of the source segments (say, the first one) must be disjoint with
   the destination segment. The other source segment (say, the second one)
   can be either disjoint with the destination segment or a suffix of the
   destination segment. (This works because the data in the second source
   segment is then moved left: it is read before it is overwritten.) *)

(* [src1len] and [src2len] must be nonzero. *)

(* This is a stable merge: when [s1] and [s2] are equal, [s1] is favored. *)

let merge cmp src1 src1ofs src1len src2 src2ofs src2len dst dstofs =
  assert (0 < src1len && 0 < src2len);
  assert (
    let dstlen = src1len + src2len in
    (* Either both source segments are disjoint with the destination segment, *)
    disjoint src1 src1ofs src1len dst dstofs dstlen &&
    disjoint src2 src2ofs src2len dst dstofs dstlen ||
    (* or the first source segment is a suffix of the destination segment, *)
      suffix src1 src1ofs src1len dst dstofs dstlen &&
    disjoint src2 src2ofs src2len dst dstofs dstlen ||
    (* or the second source segment is a suffix of the destination segment. *)
    disjoint src1 src1ofs src1len dst dstofs dstlen &&
      suffix src2 src2ofs src2len dst dstofs dstlen
  );
  let src1r = src1ofs + src1len
  and src2r = src2ofs + src2len in
  let rec loop i1 s1 i2 s2 d =
    if cmp s1 s2 <= 0 then begin
      SET dst d s1;
      let i1 = i1 + 1 in
      if i1 < src1r then
        loop i1 (GET src1 i1) i2 s2 (d + 1)
      else
        terminal_blit src2 i2 dst (d + 1) (src2r - i2)
    end else begin
      SET dst d s2;
      let i2 = i2 + 1 in
      if i2 < src2r then
        loop i1 s1 i2 (GET src2 i2) (d + 1)
      else
        terminal_blit src1 i1 dst (d + 1) (src1r - i1)
    end
  in
  loop src1ofs (GET src1 src1ofs) src2ofs (GET src2 src2ofs) dstofs

(* Although [merge] (above) works in all situations, we can make it much
   faster in the special case where the data in the first source segment is
   less than or equal to than the data in the second source segment. Indeed,
   in that case, two calls to [blit] suffice. The cost of recognizing this
   special case is two reads, a comparison, and a conditional. *)

let[@inline] optimistic_merge
    cmp src1 src1ofs src1len src2 src2ofs src2len dst dstofs =
  assert (0 < src1len && 0 < src2len);
  let last1  = GET src1 (src1ofs + src1len - 1)
  and first2 = GET src2 src2ofs in
  if cmp last1 first2 <= 0 then begin
    A.unsafe_blit src1 src1ofs dst dstofs src1len;
    A.unsafe_blit src2 src2ofs dst (dstofs + src1len) src2len
  end
  else
    merge cmp src1 src1ofs src1len src2 src2ofs src2len dst dstofs

(* Even better: in the special case where the second source segment is a
   suffix of the destination segment, [optimistic_merge] is simplified: the
   second call to [blit] has no effect, as it copies the second source
   segment to itself. Thus, it can be removed. *)

let[@inline] magic_optimistic_merge
    cmp src1 src1ofs src1len src2 src2ofs src2len dst dstofs =
  (* Check that the second source segment
     is a suffix of the destination segment. *)
  assert (src2 == dst && dstofs + src1len = src2ofs);
  assert (0 < src1len && 0 < src2len);
  let last1  = GET src1 (src1ofs + src1len - 1)
  and first2 = GET src2 src2ofs in
  if cmp last1 first2 <= 0 then
    A.unsafe_blit src1 src1ofs dst dstofs src1len
  else
    merge cmp src1 src1ofs src1len src2 src2ofs src2len dst dstofs

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
   insertion sort at the leaves. It is a stable sort. *)

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
    (* This is a stable sort, because the first half of the original
       data (now moved and sorted) is the first argument to [merge]. *)
    magic_optimistic_merge cmp
      src (srcofs + len2) len1 dst (dstofs + len1) len2 dst dstofs
  end

(* [unsafe_stable_sort_segment cmp a ofs len] sorts the array segment
   described by [a], [ofs], [len]. This array segment is sorted in place.
   This function is named [unsafe] because it does not validate [ofs] and
   [len]. *)

let unsafe_stable_sort cmp a ofs len =
  let base = ofs in
  if len <= cutoff then
    isortto cmp a base a base len
  else begin
    let len1 = len / 2 in
    let len2 = len - len1 in
    (* The second half of [a] can be larger by one slot. *)
    assert (len1 <= len2 && len2 <= len1 + 1);
    (* Allocate a temporary array that fits the second half of [a]. *)
    let t = ALLOC len2 (GET a base) in
    (* Sort the second half of [a] and move it to [t]. *)
    sortto cmp a (base + len1) t 0 len2;
    (* Sort the first half of [a] and move it to the second half of [a]. *)
    (* This requires [len1 <= len2]. *)
    sortto cmp a base a (base + len2) len1;
    (* Merge the two sorted halves, moving the data to [a]. *)
    (* This is an in-place merge: the first source segment is contained
       within the destination segment! *)
    (* This is a stable sort, because the first half of the original
       data (now moved and sorted) is the first argument to [merge]. *)
    optimistic_merge cmp a (base + len2) len1 t 0 len2 a base
  end
