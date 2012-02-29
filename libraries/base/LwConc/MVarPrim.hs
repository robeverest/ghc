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
-- Module      :  LWConc.MVarPrim
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

module LwConc.MVarPrim
( MVarPrim
, newMVarPrim       -- a -> IO (MVarPrim a)
, newEmptyMVarPrim  -- IO (MVarPrim a)
, putMVarPrim       -- PTM () -> PTM () -> MVarPrim a -> a -> IO ()
, takeMVarPrim      -- PTM () -> PTM () -> MVarPrim a -> IO a
) where

import Prelude
import LwConc.Substrate
import GHC.Prim
import GHC.IORef


newtype MVarPrim a = MVarPrim (PVar (MVPState a)) deriving (Eq)
data MVPState a = Full a [(a, PTM())]
                | Empty [(IORef a, PTM())]


newMVarPrim :: a -> IO (MVarPrim a)
newMVarPrim x = do
  ref <- newPVarIO (Full x [])
  return $ MVarPrim ref

newEmptyMVarPrim :: IO (MVarPrim a)
newEmptyMVarPrim = do
  ref <- newPVarIO (Empty [])
  return $ MVarPrim ref

{-# INLINE putMVarPrim #-}
putMVarPrim :: PTM () -> PTM () -> MVarPrim a -> a -> IO ()
putMVarPrim blockAct unblockAct (MVarPrim ref) x = atomically $ do
  st <- readPVar ref
  case st of
       Empty [] -> do
         writePVar ref $ Full x []
       Empty ((hole, wakeup):ts) -> do
         unsafeIOToPTM $ writeIORef hole x
         writePVar ref $ Empty ts
         wakeup
       Full x' ts -> do
         writePVar ref $ Full x' $ ts++[(x, unblockAct)]
         blockAct

{-# INLINE takeMVarPrim #-}
takeMVarPrim :: PTM () -> PTM () -> MVarPrim a -> IO a
takeMVarPrim blockAct unblockAct (MVarPrim ref) = do
  hole <- newIORef undefined
  atomically $ do
    st <- readPVar ref
    case st of
         Empty ts -> do
           writePVar ref $ Empty $ ts++[(hole, unblockAct)]
           blockAct
         Full x [] -> do
           writePVar ref $ Empty []
           unsafeIOToPTM $ writeIORef hole x
         Full x ((x', wakeup):ts) -> do
           writePVar ref $ Full x' ts
           unsafeIOToPTM $ writeIORef hole x
           wakeup
  readIORef hole


