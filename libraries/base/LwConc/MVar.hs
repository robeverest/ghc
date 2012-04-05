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
-- Module      :  LWConc.MVar
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- An implementation of MVar with pluggable scheduler.
--
-----------------------------------------------------------------------------

module LwConc.MVar
( MVar
, newMVar       -- a -> IO (MVar a)
, newEmptyMVar  -- IO (MVar a)
, putMVar       -- MVar a -> a -> IO ()
, takeMVar      -- MVar a -> IO a
) where

import Prelude
import LwConc.Substrate
import GHC.Prim
import GHC.IORef


newtype MVar a = MVar (PVar (MVPState a)) deriving (Eq)
data MVPState a = Full a [(a, PTM())]
                | Empty [(IORef a, PTM())]


newMVar :: a -> IO (MVar a)
newMVar x = do
  ref <- newPVarIO (Full x [])
  return $ MVar ref

newEmptyMVar :: IO (MVar a)
newEmptyMVar = do
  ref <- newPVarIO (Empty [])
  return $ MVar ref

{-# INLINE putMVar #-}
putMVar :: MVar a -> a -> IO ()
putMVar (MVar ref) x = atomically $ do
  st <- readPVar ref
  case st of
       Empty [] -> do
         writePVar ref $ Full x []
       Empty ((hole, wakeup):ts) -> do
         unsafeIOToPTM $ writeIORef hole x
         writePVar ref $ Empty ts
         wakeup
       Full x' ts -> do
         unblockAct <- getResumeThread
         blockAct <- getSwitchToNextThread
         writePVar ref $ Full x' $ ts++[(x, unblockAct)]
         sc <- getSCont
         setThreadStatus sc BlockedOnConcDS
         blockAct

{-# INLINE takeMVar #-}
takeMVar :: MVar a -> IO a
takeMVar (MVar ref) = do
  hole <- newIORef undefined
  atomically $ do
    st <- readPVar ref
    case st of
         Empty ts -> do
           blockAct <- getSwitchToNextThread
           unblockAct <- getResumeThread
           writePVar ref $ Empty $ ts++[(hole, unblockAct)]
           sc <- getSCont
           setThreadStatus sc BlockedOnConcDS
           blockAct
         Full x [] -> do
           writePVar ref $ Empty []
           unsafeIOToPTM $ writeIORef hole x
         Full x ((x', wakeup):ts) -> do
           writePVar ref $ Full x' ts
           unsafeIOToPTM $ writeIORef hole x
           wakeup
  readIORef hole


