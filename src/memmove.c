/******************************************************************************/
/*                                                                            */
/*                                   Hector                                   */
/*                                                                            */
/*                       François Pottier, Inria Paris                        */
/*                                                                            */
/*       Copyright 2024--2024 Inria. All rights reserved. This file is        */
/*       distributed under the terms of the GNU Library General Public        */
/*       License, with an exception, as described in the file LICENSE.        */
/*                                                                            */
/******************************************************************************/

#include <string.h>
#include <stdatomic.h>
#include "caml/mlvalues.h"

/* Using [memmove] can in general be incompatible with the OCaml
   memory model. However, our vectors are supposed to be accessed by
   at most one thread at a time, so this should be safe. We place a
   single memory barrier before the loop. */

CAMLprim value hector_array_blit (
  value src, value sofs,
  value dst, value dofs,
  value n
)
{
  atomic_thread_fence(memory_order_acquire);
  memmove(
    ((value*) dst) + Long_val(dofs),
    ((value*) src) + Long_val(sofs),
    Long_val(n) * sizeof(value)
  );
  return Val_unit;
}
