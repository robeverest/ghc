{-# LANGUAGE ViewPatterns #-}
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
, ThreadStatus
, newParRRSched      -- IO (ParRRSched)
, forkIO              -- ParRRSched -> IO () -> IO ()
, forkOS              -- ParRRSched -> IO () -> IO ()
, yield               -- ParRRSched -> IO ()
, newVProc            -- ParRRSched -> IO ()
) where

import LwConc.Substrate
import qualified System.Exit as E
import qualified Data.Sequence as Seq
import Data.Array.IArray
import qualified Control.Concurrent as Conc
-- import qualified Control.OldException as Exn

data ParRRSched = ParRRSched (Array Int (PVar (Seq.Seq SCont))) (PVar Int)

newParRRSched :: IO (ParRRSched)
newParRRSched = do
  token <- newPVarIO 0
  s <- getSContIO
  nc <- Conc.getNumCapabilities
  rl <- createPVarList nc []
  let sched = ParRRSched (listArray (0, nc-1) rl) token
  (b,u) <- getSchedActionPairPrim sched
  setResumeThread s $ u s
  setSwitchToNextThread s b
   -- Exn.catch (atomically (b s)) (\e -> putStrLn $ show (e::Exn.Exception));
  return sched

createPVarList 0 l = return l
createPVarList n l = do {
  ref <- newPVarIO Seq.empty;
  createPVarList (n-1) $ ref:l
}

newVProc :: ParRRSched -> IO ()
newVProc sched = do
  let loop = do {
    yield sched;
    loop
  }
  s <- newBoundSCont $ print "Running VProc" >> loop
  (b,u) <- getSchedActionPairPrim sched;
  setResumeThread s $ u s;
  setSwitchToNextThread s b;
  scheduleSContOnFreeCap s

switchToNextAndFinish :: ParRRSched -> IO ()
switchToNextAndFinish sched = do
  let ParRRSched pa _ = sched
  cc <- getCurrentCapability
  let ref = pa ! cc
  let body = do {
  contents <- readPVar ref;
  case contents of
       (Seq.viewl -> Seq.EmptyL) -> do
          unsafeIOToPTM $ print "Spinning...."
          body
       (Seq.viewl -> x Seq.:< tail) -> do
          canRun <- iCanRunSCont x
          if canRun
            then do {
              writePVar ref $ tail;
              setCurrentSContStatus Completed;
              switchTo x
            }
            else do {
              enque sched x;
              body
            }
  }
  atomically $ body

data SContKind = Bound | Unbound

fork :: ParRRSched -> IO () -> SContKind -> IO ()
fork sched task kind = do
  let ParRRSched pa token = sched
  let yieldingTask = do {
    {-Exn.try-} task;
    switchToNextAndFinish sched;
    print "ParRRSched.forkIO: Should not see this!"
  }
  let makeSCont = case kind of
                    Bound -> newBoundSCont
                    Unbound -> newSCont
  s <- makeSCont yieldingTask;
  (b,u) <- getSchedActionPairPrim sched;
  setResumeThread s $ u s;
  setSwitchToNextThread s b;
  t <- atomically $ readPVar token
  nc <- Conc.getNumCapabilities
  cc <- getCurrentCapability
  let ref = pa ! t
  setOC s t
  -- Exn.catch (atomically (b s)) (\e -> putStrLn $ show (e::Exn.Exception));
  atomically $ do
    contents <- readPVar ref
    writePVar ref $ contents Seq.|> s
    writePVar token $ (t + 1) `mod` nc

forkIO :: ParRRSched -> IO () -> IO ()
forkIO sched task = fork sched task Unbound


forkOS :: ParRRSched -> IO () -> IO ()
forkOS sched task = fork sched task Bound

switchToNextWith :: ParRRSched -> (Seq.Seq SCont -> Seq.Seq SCont) -> PTM ()
switchToNextWith sched f = do
  let ParRRSched pa _ = sched
  cc <- getCurrentCapabilityPTM
  let ref = pa ! cc
  contents <- readPVar ref;
  case f contents of
      (Seq.viewl -> Seq.EmptyL) -> switchToNextWith sched (\x -> x)
      (Seq.viewl -> x Seq.:< tail) -> do
        canRun <-iCanRunSCont x
        if canRun
          then do {
            writePVar ref $ tail;
            switchTo x
          }
          else do {
            enque sched x;
            switchToNextWith sched (\x -> x)
          }

enque :: ParRRSched -> SCont -> PTM ()
enque (ParRRSched pa _) s = do
  sc <- getSContCapability s
  let ref = pa ! sc
  contents <- readPVar ref
  let newSeq = contents Seq.|> s
  writePVar ref $ newSeq


yield :: ParRRSched -> IO ()
yield sched = atomically $ do
  s <- getSCont
  setCurrentSContStatus Yielded
  switchToNextWith sched (\tail -> tail Seq.|> s)

getSchedActionPairPrim :: ParRRSched -> IO (PTM (), SCont -> PTM ())
getSchedActionPairPrim sched = do
  let blockAction = switchToNextWith sched (\tail -> tail)
  let unblockAction s = do
      id <- getSContId s
      enque sched s
  return (blockAction, unblockAction)
