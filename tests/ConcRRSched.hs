-----------------------------------------------------------------------------
-- |
-- Module      :  concurrentRRScheduler
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- A concurrent round-robin scheduler
--
-----------------------------------------------------------------------------


module ConcRRSched
(ConcRRSched
, newConcRRSched      -- IO () -> IO (ConcRRSched)
, forkIO              -- ConcRRSched -> IO () -> IO ()
, yield               -- ConcRRSched -> IO ()
, getSchedActionPair  -- ConcRRSched -> IO (PTM (), PTM ())
) where

import LwConc.Substrate
import GHC.IORef

type CurrentHolePtr = IORef (IORef SCont)
newtype ConcRRSched = ConcRRSched (PVar [(SCont, IORef SCont)], CurrentHolePtr)

newConcRRSched :: IO () -> IO (ConcRRSched)
newConcRRSched = do
  ref <- newPVarIO []
  currentHolePtr <- newIORef $ newIORef undefined
  return $ ConcRRSched (ref, currentHolePtr)

forkIO :: ConcRRSched -> IO () -> IO ()
forkIO sched task = do
  let (ConcRRSched (ref, _)) = sched
  let yieldingTask = do {
    task;
    yield sched;
  }
  thread <- newSCont yieldingTask
  hole <- newIORef undefined
  atomically $ do
    contents <- readPVar ref
    writePVar ref $ contents++[(thread, hole)]

yield :: ConcRRSched -> IO ()
yield (ConcRRSched (ref, currentHolePtr)) = do
  oldCurrentHole <- readIORef currentHolePtr
  switch (\s -> do
    contents <- readPVar ref
    case contents of
         [] -> return s
         ((x, newCurrentHole):tail) -> do
           writePVar ref $ tail++[(s, oldCurrentHole)]
           unsafeIOToPTM $ writeIORef currentHolePtr newCurrentHole
           return x)



-- blockAction must be called by the same thread (say t) which invoked
-- getSchedActionPair. unblockAction must be called by a thread other than t,
-- which adds t back to its scheduler.

getSchedActionPair :: ConcRRSched -> (PTM (), PTM ())
getSchedActionPair (ConcRRSched (ref, currentHolePtr)) = do
  currentHole <- readIORef currentHolePtr
  let blockAction = do {
    s <- getSCont;
    contents <- readPVar ref;
    case contents of
         [] -> undefined -- what happens if I am blocked but I have no other
                         -- thread on my scheduler? (1) Should I abort the
                         -- transaction? (2) Should I throw an exception that will
                         -- be caught by my parent scheduler?
         ((x, newCurrentHole):tail) -> do
           unsafeIOToPTM $ writeIORef currentHole s
           writePVar ref tail
           unsafeIOToPTM $ writeIORef currentHolePtr newCurrentHole
           switchSContPTM x
  }
  let unblockAction = do {
    s <- unsafeIOToPTM $ readIORef currentHole;
    contents <- readPVar ref;
    writePVar ref $ contents++[(s, currentHole)]
  }
