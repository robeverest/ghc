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
#include "Threads.h"
#include "Upcalls.h"

// Initialization
UpcallQueue*
allocUpcallQueue (void)
{
  return newWSDeque (4096); //TODO KC -- Add this to RtsFlags.UpcallFlags
}

// Add a new upcall
void
addUpcall (Capability* cap, StgClosure* p)
{
  if (!pushWSDeque (cap->upcall_queue, p))
    barf ("addUpcall overflow!!");
}

// returns true if the given upcall is a suspended upcall, i.e) it is a
// reference to a StgStack.
rtsBool
isSuspendedUpcall (StgClosure* p)
{
  return (get_itbl(p)->type == STACK);
}


// current_thread can be END_TSO_QUEUE if there is no current thread.

StgTSO*
prepareUpcallThread (Capability* cap, StgTSO* current_thread)
{
  StgTSO *upcall_thread = current_thread;

  //If cap->upcall_thread is the upcall thread, then swap it with the current
  //thread.
  if (current_thread == (StgTSO*)END_TSO_QUEUE ||
      !isUpcallThread(current_thread)) {
    ASSERT (isUpcallThread(cap->upcall_thread));
    debugTrace (DEBUG_sched, "Switching to upcall_thread %d. Saving current "
                "thread %p", cap->upcall_thread->id, current_thread);
    upcall_thread = cap->upcall_thread;
    cap->upcall_thread = current_thread;
  }

  ASSERT (upcall_thread); //This can happen if upcall_thread executed a
                          //blockAction that never cleanly returned to schedule
                          //loop. TODO: Handle this case.
  ASSERT (isUpcallThread (upcall_thread));

  StgClosure* upcall = popUpcallQueue (cap->upcall_queue);
  if (isSuspendedUpcall (upcall)) {
    barf ("prepareUpcallThread: impossible!");
    upcall_thread->stackobj = (StgStack*)upcall;
  }
  else {

    ASSERT (upcall_thread->what_next != ThreadKilled);
    upcall_thread->what_next = ThreadRunGHC;
    upcall_thread->why_blocked = NotBlocked;

    StgStack* stack = upcall_thread->stackobj;
    stack->dirty = 1;
    //Pop everything
    stack->sp = stack->stack + stack->stack_size;
    //Push stop frame
    stack->sp -= sizeofW(StgStopFrame);
    SET_HDR((StgClosure*)stack->sp,
            (StgInfoTable *)&stg_stop_thread_info,CCS_SYSTEM);
    pushCallToClosure (cap, upcall_thread, upcall);
  }

  return upcall_thread;
}

// restoreCurrentThreadIfNecessary can return END_TSO_QUEUE if there was no
// current thread when we first switched to the upcall_thread. Care must be
// taken by the caller to handle the case when returned thread is END_TSO_QUEUE.

StgTSO*
restoreCurrentThreadIfNecessary (Capability* cap, StgTSO* current_thread) {

  StgTSO* return_thread = current_thread;

  //Given Thread is the upcall thread
  if (isUpcallThread (current_thread)) {
    return_thread = cap->upcall_thread;
    //Save the upcall thread
    cap->upcall_thread = current_thread;
  }

  return return_thread;
}

/* GC for the upcall queue, called inside Capability.c for all capabilities in
 * turn. */
void
traverseUpcallQueue (evac_fn evac, void* user, Capability *cap)
{
  //XXX KC -- Copy paste from traverseSparkPool. Merge these if possible.
  StgClosure **upcallp;
  UpcallQueue *queue;
  StgWord top,bottom, modMask;

  queue = cap->upcall_queue;

  ASSERT_WSDEQUE_INVARIANTS(queue);

  top = queue->top;
  bottom = queue->bottom;
  upcallp = (StgClosurePtr*)queue->elements;
  modMask = queue->moduloSize;

  while (top < bottom) {
    /* call evac for all closures in range (wrap-around via modulo)
     * In GHC-6.10, evac takes an additional 1st argument to hold a
     * GC-specific register, see rts/sm/GC.c::mark_root()
     */
    evac( user , upcallp + (top & modMask) );
    top++;
  }

  debugTrace(DEBUG_gc,
             "traversed upcall queue, len=%ld; (hd=%ld; tl=%ld)",
             upcallQueueSize(queue), queue->bottom, queue->top);
}
