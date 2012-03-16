{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  LwConc.Schedulers.ConcRRSched
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- A concurrent round-robin scheduler.
--
-----------------------------------------------------------------------------


module ConcRRSched
(ConcRRSched
, SwitchStatus
, newConcRRSched      -- IO (ConcRRSched)
, forkIO              -- ConcRRSched -> IO () -> IO ()
, yield               -- ConcRRSched -> IO ()
, getSchedActionPair  -- ConcRRSched -> IO (PTM (), PTM ())

, createThread        -- ConcRRSched -> IO () -> IO SCont | Creates a thread but
                      -- does not add it to the scheduler
) where

import LwConc.Substrate
import qualified System.Exit as E
import qualified Data.Sequence as Seq
import qualified Control.OldException as Exn

newtype ConcRRSched = ConcRRSched (PVar (Seq.Seq SCont))

newConcRRSched :: IO (ConcRRSched)
newConcRRSched = do
  ref <- newPVarIO Seq.empty
  s <- getSContIO
  (b,u) <- getSchedActionPairPrim (ConcRRSched ref);
  setResumeThreadClosure s $ \s2 -> atomically $ u s2;
  setSwitchToNextThreadClosure s $ \s -> atomically $ b s;
   -- Exn.catch (atomically (b s)) (\e -> putStrLn $ show (e::Exn.Exception));
  return $ ConcRRSched (ref)

switchToNextAndFinish :: ConcRRSched -> IO ()
switchToNextAndFinish (ConcRRSched ref) = atomically $ do
  contents <- readPVar ref
  case contents of
       (Seq.viewl -> Seq.EmptyL) -> undefined
       (Seq.viewl -> x Seq.:< tail) -> do {
          writePVar ref $ tail;
          switchTo x Completed
       }

createThread :: ConcRRSched -> IO () -> IO SCont
createThread (ConcRRSched ref) task =
  let yieldingTask = do {
    {-Exn.try-} task;
    switchToNextAndFinish (ConcRRSched ref);
    print "ConcRRSched.forkIO: Should not see this!"
  }
  in do {
    s <- newSCont yieldingTask;
    (b,u) <- getSchedActionPairPrim (ConcRRSched ref);
    setResumeThreadClosure s $ \s2 -> atomically $ u s2;
    setSwitchToNextThreadClosure s $ \s -> atomically $ b s;
      -- Exn.catch (atomically (b s)) (\e -> putStrLn $ show (e::Exn.Exception));
    return s
    }

forkIO :: ConcRRSched -> IO () -> IO ()
forkIO (ConcRRSched ref) task = do
  thread <- createThread (ConcRRSched ref) task
  atomically $ do
    contents <- readPVar ref
    writePVar ref $ contents Seq.|> thread

switchToNextWith :: ConcRRSched -> (Seq.Seq SCont -> Seq.Seq SCont) -> SwitchStatus -> PTM ()
switchToNextWith (ConcRRSched ref) f status = do
  contents <- readPVar ref
  case f contents of
       (Seq.viewl -> Seq.EmptyL) -> undefined
       (Seq.viewl -> x Seq.:< tail) -> do {
          writePVar ref $ tail;
          switchTo x status
       }

push :: ConcRRSched -> SCont -> PTM ()
push (ConcRRSched ref) s = do
  contents <- readPVar ref
  let newSeq = s Seq.<| contents
  writePVar ref $ newSeq

enque :: ConcRRSched -> SCont -> PTM ()
enque (ConcRRSched ref) s = do
  contents <- readPVar ref
  let newSeq = contents Seq.|> s
  writePVar ref $ newSeq


yield :: ConcRRSched -> IO ()
yield sched = atomically $ do
  s <- getSCont
  switchToNextWith sched (\tail -> tail Seq.|> s) BlockedOnSched

-- blockAction must be called by the thread t whose SCont corresponds to the
-- Scont passed to the getSchedActionPair function.  unblockAction must be
-- called by a thread other than t, which adds t back to its scheduler.

getSchedActionPairPrim :: ConcRRSched -> IO (SwitchStatus -> PTM (), SCont -> PTM ())
getSchedActionPairPrim sched = do
  let blockAction status = switchToNextWith sched (\tail -> tail) status
  let unblockAction s = do
      enque sched s
  return (blockAction, unblockAction)

getSchedActionPair :: ConcRRSched -> IO (PTM (), PTM ())
getSchedActionPair sched = do
  s <- getSContIO
  (b, u) <- getSchedActionPairPrim sched
  return (b BlockedOnConcDS, u s)
