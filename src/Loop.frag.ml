(* An ordinary loop, from [start] (included) to [finish] (excluded).
   The loop index is named [i], and can be used in the loop [body]. *)

#define LOOP(i, start, finish, body) (\
  for i = start to finish-1 do\
    body\
  done\
)

(* An unrolled loop, with the same semantics. The loop is unrolled
   five times. *)

(* Unrolling a loop five times lets us avoid a bizarre slowness that
   we have observed on arm64 processors, including Apple M1 and M2;
   see https://github.com/ocaml/ocaml/issues/13262 *)

#define LOOP5(i, start, finish, body) (\
  let __finish = (finish) in\
  let __index = ref (start) in\
  let __limit = __finish - 5 in\
  while !__index <= __limit do\
    let __this = !__index in\
    (let i = __this + 0 in body);\
    (let i = __this + 1 in body);\
    (let i = __this + 2 in body);\
    (let i = __this + 3 in body);\
    (let i = __this + 4 in body);\
    __index := __this + 5\
  done;\
  let __finish = __limit + 5 in\
  while !__index < __finish do\
    (let i = !__index + 0 in body);\
    __index := !__index + 1\
  done\
)
