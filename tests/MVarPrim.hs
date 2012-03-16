{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP
           , ForeignFunctionInterface
           , MagicHash
           , UnboxedTuples
           , ScopedTypeVariables
  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  MVarPrim
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

module MVarPrim
( MVarPrim
, newMVarPrim       -- a -> IO (MVarPrim a)
, newEmptyMVarPrim  -- IO (MVarPrim a)
, putMVarPrim       -- PTM () -> PTM () -> MVarPrim a -> a -> IO ()
, takeMVarPrim      -- PTM () -> PTM () -> MVarPrim a -> IO a
) where

import Prelude
import LwConc.Substrate
import qualified Data.Sequence as Seq
import GHC.Prim
import GHC.IORef


newtype MVarPrim a = MVarPrim (PVar (MVPState a)) deriving (Eq)
data MVPState a = Full a (Seq.Seq (a, PTM()))
                | Empty (Seq.Seq (IORef a, PTM()))


newMVarPrim :: a -> IO (MVarPrim a)
newMVarPrim x = do
  ref <- newPVarIO $ Full x Seq.empty
  return $ MVarPrim ref

newEmptyMVarPrim :: IO (MVarPrim a)
newEmptyMVarPrim = do
  ref <- newPVarIO $ Empty Seq.empty
  return $ MVarPrim ref

{-# INLINE putMVarPrim #-}
putMVarPrim :: PTM () -> PTM () -> MVarPrim a -> a -> IO ()
putMVarPrim blockAct unblockAct (MVarPrim ref) x = atomically $ do
  st <- readPVar ref
  case st of
       Empty (Seq.viewl -> Seq.EmptyL) -> do
         writePVar ref $ Full x Seq.empty
       Empty (Seq.viewl -> (hole, wakeup) Seq.:< ts) -> do
         unsafeIOToPTM $ writeIORef hole x
         writePVar ref $ Empty ts
         wakeup
       Full x' ts -> do
         writePVar ref $ Full x' $ ts Seq.|> (x, unblockAct)
         blockAct

{-# INLINE takeMVarPrim #-}
takeMVarPrim :: PTM () -> PTM () -> MVarPrim a -> IO a
takeMVarPrim blockAct unblockAct (MVarPrim ref) = do
  hole <- newIORef undefined
  atomically $ do
    st <- readPVar ref
    case st of
         Empty ts -> do
           writePVar ref $ Empty $ ts Seq.|> (hole, unblockAct)
           blockAct
         Full x (Seq.viewl -> Seq.EmptyL) -> do
           writePVar ref $ Empty Seq.empty
           unsafeIOToPTM $ writeIORef hole x
         Full x (Seq.viewl -> (x', wakeup) Seq.:< ts) -> do
           writePVar ref $ Full x' ts
           unsafeIOToPTM $ writeIORef hole x
           wakeup
  readIORef hole
