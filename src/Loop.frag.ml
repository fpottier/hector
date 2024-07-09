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

(* An ordinary loop, from [start] (included) to [finish] (excluded).
   The loop index is named [i], and can be used in the loop body. The
   loop body is of the form [let data = read in write]. We assume that
   the iterations are independent (this is a parallel loop), so a read
   in one iteration commutes with the write in a previous iteration. *)

#define LOOPRW(i, start, finish, data, read, write) (\
  for i = start to finish-1 do\
    let data = read in write\
  done\
)

(* An unrolled loop, with the same semantics. The loop is unrolled five times.
   We schedule the five reads before the five writes, in the hope of reducing
   the latency caused by load instructions and (perhaps) allowing the compiler
   to merge several memory barriers into one. *)

#define LOOPRW5(i, start, finish, data, read, write) (\
  let __finish = (finish) in\
  let __index = ref (start) in\
  let __limit = __finish - 5 in\
  while !__index <= __limit do\
    let __this = !__index in\
    let __data0 = (let i = __this + 0 in read) in\
    let __data1 = (let i = __this + 1 in read) in\
    let __data2 = (let i = __this + 2 in read) in\
    let __data3 = (let i = __this + 3 in read) in\
    let __data4 = (let i = __this + 4 in read) in\
    (let i = __this + 0 in let data = __data0 in write);\
    (let i = __this + 1 in let data = __data1 in write);\
    (let i = __this + 2 in let data = __data2 in write);\
    (let i = __this + 3 in let data = __data3 in write);\
    (let i = __this + 4 in let data = __data4 in write);\
    __index := __this + 5\
  done;\
  let __finish = __limit + 5 in\
  while !__index < __finish do\
    (let i = !__index + 0 in let data = read in write);\
    __index := !__index + 1\
  done\
)
