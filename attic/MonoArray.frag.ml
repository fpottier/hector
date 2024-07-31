(* -------------------------------------------------------------------------- *)

(* [blit]. *)

(* Let us begin with an implementation of [blit] as a loop. *)

(* When [n] is small, a loop is faster than a foreign function call.
   Both [unsafe_blit] (below) and [Array.blit] involve a call to a
   C function. *)

let cutoff = 8

let[@inline] manual_unsafe_blit src sofs dst dofs n =
  if src != dst || dofs <= sofs then
    LOOP(i, 0, n, unsafe_set dst (dofs + i) (unsafe_get src (sofs + i)))
  else
    LOOP_DOWN(i, 0, n, unsafe_set dst (dofs + i) (unsafe_get src (sofs + i)))

#ifdef IMMEDIATE

(* If the type [element] is immediate (i.e., not a pointer type) then
   [memmove] can be used. *)

(* In the case of integer arrays, [memmove] can be 4 times faster than
   hand-written loop, and 12 times faster than [Array.blit]. *)

external foreign_unsafe_blit :
  int array -> int ->
  int array -> int ->
  int ->
  unit
= "hector_array_blit"

let[@inline] unsafe_blit (src : t) sofs dst dofs n =
  if n < cutoff then
    manual_unsafe_blit src sofs dst dofs n
  else
    foreign_unsafe_blit src sofs dst dofs n

let blit (src : t) sofs dst dofs n =
  validate_segment (length src) sofs n;
  validate_segment (length dst) dofs n;
  unsafe_blit src sofs dst dofs n

#else

(* We always use [Array.blit], as opposed to a hand-written loop, because this
   is easier, and because [Array.blit] can be more efficient. In particular,
   in [Array.blit], some tests (e.g., is this array young?) are hoisted out of
   the loop. *)

let[@inline] unsafe_blit (src : t) sofs dst dofs n =
  if n < cutoff then
    manual_unsafe_blit src sofs dst dofs n
  else
    Array.blit src sofs dst dofs n

let blit src sofs dst dofs n =
  if n < cutoff then begin
    validate_segment (length src) sofs n;
    validate_segment (length dst) dofs n;
    manual_unsafe_blit src sofs dst dofs n
  end
  else
    (* In this branch, there is no need to validate. *)
    Array.blit src sofs dst dofs n

#endif
