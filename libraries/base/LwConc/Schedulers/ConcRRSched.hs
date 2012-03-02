{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , ForeignFunctionInterface
           , MagicHash
           , UnboxedTuples
           , ScopedTypeVariables
  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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


module LwConc.Schedulers.ConcRRSched
(ConcRRSched
, newConcRRSched      -- IO (ConcRRSched)
, forkIO              -- ConcRRSched -> IO () -> IO ()
, yield               -- ConcRRSched -> IO ()
, getSchedActionPair  -- ConcRRSched -> IO (PTM (), PTM ())
) where

import LwConc.Substrate

newtype ConcRRSched = ConcRRSched (PVar [SCont])

newConcRRSched :: IO (ConcRRSched)
newConcRRSched = undefined

forkIO :: ConcRRSched -> IO () -> IO ()
forkIO (ConcRRSched ref) task = undefined

yield :: ConcRRSched -> IO ()
yield sched = undefined

-- blockAction must be called by the same thread (say t) which invoked
-- getSchedActionPair. unblockAction must be called by a thread other than t,
-- which adds t back to its scheduler.

getSchedActionPair :: ConcRRSched -> IO (PTM (), PTM ())
getSchedActionPair sched = undefined
