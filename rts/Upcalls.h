/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2009
 *
 * Upcall support -- Upcalls are typically pending IO actions (UPCALL_CLOSURE)
 * that are ALWAYS evaluated by capability's upcall_thread. Upcalls are created
 * when threads block on black holes, finalizers, etc. An upcall can itself
 * become blocked, in which case, its continuation is captured and stored as a
 * new upcall (UPCALL_STACK).
 *
 * ---------------------------------------------------------------------------*/

#ifndef UPCALLS_H
#define UPCALLS_H

#include "BeginPrivate.h"

typedef WSDeque UpcallQueue;

// Initialization
UpcallQueue *allocUpcallQueue (void);

INLINE_HEADER long upcallQueueSize (UpcallQueue *q);


void traverseUpcallQueue (evac_fn eval, void* user, Capability *cap);

/* -----------------------------------------------------------------------------
 * PRIVATE below here
 * -------------------------------------------------------------------------- */

INLINE_HEADER long upcallQueueSize (UpcallQueue* q)
{
    return dequeElements(q);
}

#include "EndPrivate.h"

#endif /* SPARKS_H */
