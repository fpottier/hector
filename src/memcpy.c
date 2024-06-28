#include <string.h>
#include <stdatomic.h>
#include "caml/mlvalues.h"

CAMLprim value hector_array_blit (
  value src, value sofs,
  value dst, value dofs,
  value n
)
{
  atomic_thread_fence(memory_order_acquire);
  memcpy (
    ((value*) dst) + Long_val(sofs),
    ((value*) src) + Long_val(dofs),
    Long_val(n) * sizeof(value)
  );
  return Val_unit;
}
