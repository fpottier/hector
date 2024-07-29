(* -------------------------------------------------------------------------- *)

(**[validate_segment n ofs len] checks that the offset [ofs] and the
   length [len] determine a valid interval inside an array or vector of
   length [n]. *)

let[@inline never] invalid_segment n ofs len =
  Printf.ksprintf invalid_arg
    "invalid segment (ofs = %d, len = %d) in a sequence of length %d"
    ofs len n

let[@inline] validate_segment n ofs len =
  if not (0 <= len && 0 <= ofs && ofs + len <= n) then
    invalid_segment n ofs len
