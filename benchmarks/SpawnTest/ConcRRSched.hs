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
, newConcRRSched      -- IO (ConcRRSched)
, forkIO              -- ConcRRSched -> IO () -> IO ()
, yield               -- ConcRRSched -> IO ()
, getSchedActionPair  -- ConcRRSched -> IO (PTM (), PTM ())
) where

import LwConc.Substrate
import qualified Data.Sequence as Seq

newtype ConcRRSched = ConcRRSched (PVar (Seq.Seq SCont))

newConcRRSched :: IO (ConcRRSched)
newConcRRSched = do
  ref <- newPVarIO Seq.empty
  return $ ConcRRSched (ref)

forkIO :: ConcRRSched -> IO () -> IO ()
forkIO (ConcRRSched ref) task = do
  let yieldingTask = do {
    task;
    yield $ ConcRRSched ref
  }
  thread <- newSCont yieldingTask
  atomically $ do
    contents <- readPVar ref
    writePVar ref $ contents Seq.|> thread


enqueAndSwitchToNext :: ConcRRSched -> SCont -> PTM ()
enqueAndSwitchToNext (ConcRRSched ref) s = do
  contents <- readPVar ref
  if Seq.length contents == 0
     then switchSContPTM s
     else do
       let x = Seq.index contents 0
       let tail = Seq.drop 1 contents
       let newSeq = tail Seq.|> s
       writePVar ref $ newSeq
       switchSContPTM x

enque :: ConcRRSched -> SCont -> PTM ()
enque (ConcRRSched ref) s = do
  contents <- readPVar ref
  let newSeq = contents Seq.|> s
  writePVar ref $ newSeq

yield :: ConcRRSched -> IO ()
yield sched = atomically $ do
  s <- getSCont
  enqueAndSwitchToNext sched s

-- blockAction must be called by the same thread (say t) which invoked
-- getSchedActionPair. unblockAction must be called by a thread other than t,
-- which adds t back to its scheduler.

getSchedActionPair :: ConcRRSched -> IO (PTM (), PTM ())
getSchedActionPair sched = do
  s <- atomically $ getSCont
  let blockAction   = enqueAndSwitchToNext sched s
  let unblockAction = enque sched s
  return (blockAction, unblockAction)
