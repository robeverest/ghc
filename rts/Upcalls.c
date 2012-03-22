/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2009
 *
 * Upcall support
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "Schedule.h"
#include "RtsUtils.h"
#include "Trace.h"
#include "Prelude.h"
#include "Upcalls.h"

/* GC for the upcall queue, called inside Capability.c for all capabilities in
 * turn. */
void
traverseUpcallQueue (evac_fn eval, void* user, Capability *cap)
{
  //XXX KC -- Copy paste from traverseSparkPool. Merge these if possible.
  StgClosure **upcallp;
  upcallPool *pool;
  StgWord top,bottom, modMask;

  pool = cap->upcalls;

  ASSERT_WSDEQUE_INVARIANTS(pool);

  top = pool->top;
  bottom = pool->bottom;
  upcallp = (StgClosurePtr*)pool->elements;
  modMask = pool->moduloSize;

  while (top < bottom) {
    /* call evac for all closures in range (wrap-around via modulo)
     * In GHC-6.10, evac takes an additional 1st argument to hold a
     * GC-specific register, see rts/sm/GC.c::mark_root()
     */
    evac( user , upcallp + (top & modMask) );
    top++;
  }

  debugTrace(DEBUG_upcalls,
             "traversed upcall queue, len=%ld; (hd=%ld; tl=%ld)",
             upcallPoolSize(pool), pool->bottom, pool->top);
}
