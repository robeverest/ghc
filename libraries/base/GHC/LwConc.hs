{-# LANGUAGE Unsafe #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , BangPatterns
           , MagicHash
           , UnboxedTuples
           , DeriveDataTypeable
           , StandaloneDeriving
  #-}
{-# OPTIONS_HADDOCK hide #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Weak
-- Copyright   :  (c) The University of Glasgow, 1998-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Lightweigh concurrency.
--
-----------------------------------------------------------------------------


-- #hide
module GHC.LwConc
( SCont (..)
, runSchedulerActionsBatch
) where

import GHC.Base
import Data.Maybe
import Data.Typeable

-----------------------------------------------------------------------------
-- SCont datatyoe

data SCont = SCont SCont#

-- run a batch of unblock actions from the garbage collector.  We're given an
-- array of unblock actions and the length of the array, and we just call each
-- one in turn.
--
-- the IO primitives are inlined by hand here to get the optimal
-- code (sigh) --SDM.

runSchedulerActionsBatch :: Int -> Array# (IO ()) -> IO ()
runSchedulerActionsBatch (I# n) arr =
   let  go m  = IO $ \s ->
                  case m of
                  0# -> (# s, () #)
                  _  -> let !m' = m -# 1# in
                        case indexArray# arr m' of { (# io #) ->
                        case unIO io s of          { (# s', _ #) ->
                        unIO (go m') s'
                        }}
   in
        go n

