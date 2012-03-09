-----------------------------------------------------------------------------
-- |
-- Module      :  LwConc.Schedulers.ParRRSched
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


module ParRRSched
(ParRRSched
, newParRRSched       -- IO (ParRRSched)
, forkIO              -- ParRRSched -> IO () -> IO ()
, yield               -- ParRRSched -> IO ()
, getSchedActionPair  -- ParRRSched -> IO (PTM (), PTM ())

, SCont
, switchToNext        -- ParRRSched -> (SCont -> PTM ()) -> PTM ()
) where

import LwConc.Substrate
import qualified Control.Concurrent as Conc
import qualified Data.Sequence as Seq

newtype ParRRSched = ParRRSched (PVar (Seq.Seq SCont))

newParRRSched :: IO (ParRRSched)
newParRRSched = do
  ref <- newPVarIO Seq.empty
  return $ ParRRSched (ref)

forkIO :: ParRRSched -> IO () -> IO ()
forkIO (ParRRSched ref) task = do
  let yieldingTask = do {
    task;
    atomically $ switchToNextWith (ParRRSched ref) (\tail -> tail);
    print "ParRRSched.forkIO: Should not see this!"
  }
  thread <- newSCont yieldingTask
  atomically $ do
    contents <- readPVar ref
    writePVar ref $ contents Seq.|> thread

switchToNextWith :: ParRRSched -> (Seq.Seq SCont -> Seq.Seq SCont) -> PTM ()
switchToNextWith (ParRRSched ref) f = do
  oldContents <- readPVar ref
  let contents = f oldContents
  if Seq.length contents == 0
     then unsafeIOToPTM $ do
       putStrLn "***** NO THREADS TO SWITCH TO *****"
       tid <- Conc.myThreadId
       Conc.killThread tid
     else do
       let x = Seq.index contents 0
       let tail = Seq.drop 1 contents
       writePVar ref $ tail
       switchSContPTM x

getNext :: ParRRSched -> PTM SCont
getNext (ParRRSched ref) = do
  contents <- readPVar ref
  if Seq.length contents == 0
     then getNext $ ParRRSched ref
     else do
       let x = Seq.index contents 0
       let tail = Seq.drop 1 contents
       writePVar ref $ tail
       return x


enque :: ParRRSched -> SCont -> PTM ()
enque (ParRRSched ref) s = do
  contents <- readPVar ref
  let newSeq = contents Seq.|> s
  writePVar ref $ newSeq

yield :: ParRRSched -> IO ()
yield sched = atomically $ do
  s <- getSCont
  switchToNextWith sched (\tail -> tail Seq.|> s)

-- blockAction must be called by the same thread (say t) which invoked
-- getSchedActionPair. unblockAction must be called by a thread other than t,
-- which adds t back to its scheduler.

getSchedActionPair :: ParRRSched -> IO (PTM (), PTM ())
getSchedActionPair sched = do
  s <- atomically $ getSCont
  let blockAction   = switchToNextWith sched (\tail -> tail)
  let unblockAction = enque sched s
  return (blockAction, unblockAction)

switchToNext :: ParRRSched -> (SCont -> PTM ()) -> PTM ()
switchToNext sched f = do
  s <- getSCont
  f s
  next <- getNext sched
  switchSContPTM next

