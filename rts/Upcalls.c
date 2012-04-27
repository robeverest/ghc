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
pushUpcall (Capability* cap, Upcall uc)
{
  if (!pushWSDeque (cap->upcall_queue, uc))
    barf ("pushUpcall overflow!!");
  debugTrace (DEBUG_sched, "Adding new upcall %p (queue size = %d)",
              (void*)uc, upcallQueueSize (cap->upcall_queue));
}

//See libraries/base/LwConc/Substrate.hs:unblockThreadRts
Upcall
getResumeThreadUpcall (Capability* cap, StgTSO* t)
{
  Upcall p;

  ASSERT (!t->is_upcall_thread);
  ASSERT (t->resume_thread != (StgClosure*)defaultUpcall_closure);

  p = rts_apply (cap, (StgClosure*)unblockThreadRts_closure,
                 rts_mkSCont (cap, t));

  debugTrace (DEBUG_sched, "cap %d: getResumeThreadUpcall(%p) for thread %d",
              cap->no, (void*)p, t->id);
  return p;
}

//See libraries/base/LwConc/Substrate.hs:switchToNextThread
Upcall
getSwitchToNextThreadUpcall (Capability* cap, StgTSO* t)
{
  Upcall p;

  ASSERT (!t->is_upcall_thread);
  ASSERT (t->switch_to_next != (StgClosure*)defaultUpcall_closure);

  p = rts_apply (cap, (StgClosure*)switchToNextThreadRts_closure,
                 rts_mkSCont (cap, t));

  debugTrace (DEBUG_sched, "cap %d: getSwitchToNextThreadupcall(%p) for thread %d",
              cap->no, (void*)p, t->id);
  return p;
}

Upcall
getFinalizerUpcall (Capability* cap STG_UNUSED, StgTSO* t)
{
  ASSERT (!t->is_upcall_thread);
  StgClosure* p = t->finalizer;
  return p;
}


StgTSO*
prepareUpcallThread (Capability* cap, StgTSO* current_thread)
{
  StgTSO *upcall_thread;


  //If current thread is not an upcall thread, get the upcall thread.
  if (current_thread == (StgTSO*)END_TSO_QUEUE ||
      !isUpcallThread(current_thread)) {

    //Upcall thread is running, create a new upcall thread
    if (cap->upcall_thread->what_next != ThreadComplete)
      initUpcallThreadOnCapability (cap);

    upcall_thread = cap->upcall_thread;
    debugTrace (DEBUG_sched, "Switching to upcall_thread %d. Saving current "
                "thread %d.", cap->upcall_thread->id,
                (current_thread == (StgTSO*)END_TSO_QUEUE)?-1:(int)current_thread->id);
    //Save current thread
    ASSERT (saved_thread == (StgTSO*)END_TSO_QUEUE);
    cap->saved_thread = current_thread;
  }
  else {
    upcall_thread = current_thread;
  }

  ASSERT (isUpcallThread (upcall_thread));

  //Upcall thread is currently running
  if (upcall_thread->what_next != ThreadComplete)
    return upcall_thread;

  StgClosure* upcall = popUpcallQueue (cap->upcall_queue);

  ASSERT (upcall_thread->what_next != ThreadKilled);
  upcall_thread->_link = (StgTSO*)END_TSO_QUEUE;
  upcall_thread->what_next = ThreadRunGHC;
  upcall_thread->why_blocked = NotBlocked;
  //Save the upcall in the finalzer slot of the upcall thread so that it can be
  //retrieved quickly if the upcall happens to block on a black hole -- KC.
  upcall_thread->finalizer = upcall;


  StgStack* stack = upcall_thread->stackobj;
  stack->dirty = 1;
  //Pop everything
  stack->sp = stack->stack + stack->stack_size;
  //Push stop frame
  stack->sp -= sizeofW(StgStopFrame);
  SET_HDR((StgClosure*)stack->sp,
          (StgInfoTable *)&stg_stop_thread_info,CCS_SYSTEM);
  pushCallToClosure (cap, upcall_thread, upcall);

  return upcall_thread;
}

// restoreCurrentThreadIfNecessary can return END_TSO_QUEUE if there was no
// current thread when we first switched to the upcall_thread. Care must be
// taken by the caller to handle the case when returned thread is END_TSO_QUEUE.

StgTSO*
restoreCurrentThreadIfNecessary (Capability* cap, StgTSO* current_thread) {

  StgTSO* return_thread;

  //Given Thread is the upcall thread, which has finished
  if (isUpcallThread (current_thread) &&
      current_thread->what_next == ThreadComplete) {

    return_thread = cap->saved_thread;
    cap->saved_thread = (StgTSO*)END_TSO_QUEUE;

    debugTrace (DEBUG_sched, "Saving upcall thread %d and restoring original"
                " thread %d", current_thread->id,
                (return_thread == (StgTSO*)END_TSO_QUEUE)?-1:(int)return_thread->id);

    //Save the upcall thread
    cap->upcall_thread = current_thread;
  }
  else {
    return_thread = current_thread;
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

//upcallQueueSize == 0 && cap->saved_thread == END_TSO_QUEUE. Second condition
//necessary since restoring the saved thread can be necessary for the program to
//run to completion.
rtsBool emptyUpcallQueue (Capability* cap)
{
  UpcallQueue* q = cap->upcall_queue;
  return (upcallQueueSize (q) == 0 &&
          cap->saved_thread == END_TSO_QUEUE);
}
