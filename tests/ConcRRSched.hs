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
, ThreadStatus
, newConcRRSched      -- IO (ConcRRSched)
, forkIO              -- ConcRRSched -> IO () -> IO ()
, forkOS              -- ConcRRSched -> IO () -> IO ()
, yield               -- ConcRRSched -> IO ()
, newVProc            -- ConcRRSched -> IO ()
) where

import LwConc.Substrate
import qualified System.Exit as E
import qualified Data.Sequence as Seq
import qualified Control.Concurrent as Conc
-- import qualified Control.OldException as Exn

newtype ConcRRSched = ConcRRSched (PVar (Seq.Seq SCont))

newConcRRSched :: IO (ConcRRSched)
newConcRRSched = do
  ref <- newPVarIO Seq.empty
  s <- getSContIO
  (b,u) <- getSchedActionPairPrim (ConcRRSched ref)
  setResumeThread s $ u s
  setSwitchToNextThread s b
   -- Exn.catch (atomically (b s)) (\e -> putStrLn $ show (e::Exn.Exception));
  return $ ConcRRSched (ref)

newVProc :: ConcRRSched -> IO ()
newVProc sched = do
  let init = do {
    s <- getSContIO;
    (b,u) <- getSchedActionPairPrim sched;
    setResumeThread s $ u s;
    setSwitchToNextThread s b;
  }
  let loop = do {
    yield sched;
    loop
  }
  Conc.forkOn 1 $ init >> loop
  return ()


switchToNextAndFinish :: ConcRRSched -> IO ()
switchToNextAndFinish (ConcRRSched ref) = atomically $ do
  contents <- readPVar ref
  case contents of
       (Seq.viewl -> Seq.EmptyL) -> undefined
       (Seq.viewl -> x Seq.:< tail) -> do {
          writePVar ref $ tail;
          currentSCont <- getSCont;
          setThreadStatus currentSCont Completed;
          switchTo x
       }

data SContKind = Bound | Unbound

fork :: ConcRRSched -> IO () -> SContKind -> IO ()
fork (ConcRRSched ref) task kind = do
  let yieldingTask = do {
    {-Exn.try-} task;
    switchToNextAndFinish (ConcRRSched ref);
    print "ConcRRSched.forkIO: Should not see this!"
  }
  let makeSCont = case kind of
                    Bound -> newBoundSCont
                    Unbound -> newSCont
  s <- makeSCont yieldingTask;
  (b,u) <- getSchedActionPairPrim (ConcRRSched ref);
  setResumeThread s $ u s;
  setSwitchToNextThread s b;
  -- Exn.catch (atomically (b s)) (\e -> putStrLn $ show (e::Exn.Exception));
  atomically $ do
    contents <- readPVar ref
    writePVar ref $ contents Seq.|> s

forkIO :: ConcRRSched -> IO () -> IO ()
forkIO sched task = fork sched task Unbound


forkOS :: ConcRRSched -> IO () -> IO ()
forkOS sched task = fork sched task Bound

switchToNextWith :: ConcRRSched -> (Seq.Seq SCont -> Seq.Seq SCont) -> PTM ()
switchToNextWith (ConcRRSched ref) f = do
  contents <- readPVar ref
  case f contents of
       (Seq.viewl -> Seq.EmptyL) -> undefined
       (Seq.viewl -> x Seq.:< tail) -> do {
          writePVar ref $ tail;
          switchTo x
       }

enque :: ConcRRSched -> SCont -> PTM ()
enque (ConcRRSched ref) s = do
  contents <- readPVar ref
  let newSeq = contents Seq.|> s
  writePVar ref $ newSeq


yield :: ConcRRSched -> IO ()
yield sched = atomically $ do
  s <- getSCont
  setThreadStatus s Yielded
  switchToNextWith sched (\tail -> tail Seq.|> s)

-- blockAction must be called by the thread t whose SCont corresponds to the
-- Scont passed to the getSchedActionPair function.  unblockAction must be
-- called by a thread other than t, which adds t back to its scheduler.

getSchedActionPairPrim :: ConcRRSched -> IO (PTM (), SCont -> PTM ())
getSchedActionPairPrim sched = do
  let blockAction = switchToNextWith sched (\tail -> tail)
  let unblockAction s = do
      enque sched s
  return (blockAction, unblockAction)

getSchedActionPair :: ConcRRSched -> SCont -> IO (PTM (), PTM ())
getSchedActionPair sched s = do
  (b, u) <- getSchedActionPairPrim sched
  return (setThreadStatus s BlockedOnConcDS >> b, u s)
