{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , ForeignFunctionInterface
           , MagicHash
           , UnboxedTuples
           , ScopedTypeVariables
  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -w #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LWConc.Substrate
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- A common interface for lightweight concurrency abstractions.
--
-----------------------------------------------------------------------------


module LwConc.Substrate
( PTM
, unsafeIOToPTM     -- IO a -> PTM a
, atomically        -- PTM a -> IO a

, PVar
, newPVar           -- a -> PTM (PVar a)
, newPVarIO         -- a -> IO (PVar a)
, readPVar          -- PVar a -> PTM a
, writePVar         -- PVar a -> a -> PTM ()

, SCont
, ThreadStatus (Blocked, Completed)
, newSCont          -- IO () -> IO SCont
, switch            -- (SCont -> PTM (SCont, ThreadStatus)) -> IO ()
, getSCont          -- PTM SCont
, switchTo          -- SCont -> ThreadStatus -> PTM ()
) where


import Prelude
import GHC.Base
import GHC.Prim
import GHC.IO
import Data.Typeable

newtype PTM a = PTM (State# RealWorld -> (# State# RealWorld, a #))


unPTM :: PTM a -> (State# RealWorld -> (# State# RealWorld, a #))
unPTM (PTM a) = a

ptmTc = mkTyCon3 "ghc-prim" "LwConc.Substrate" "PTM"
instance Typeable1 PTM where { typeOf1 _ = mkTyConApp ptmTc [] }

instance  Functor PTM where
   fmap f x = x >>= (return . f)

instance  Monad PTM  where
    {-# INLINE return #-}
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    m >> k      = thenPTM m k
    return x	= returnPTM x
    m >>= k     = bindPTM m k

{-# INLINE bindPTM  #-}
bindPTM :: PTM a -> (a -> PTM b) -> PTM b
bindPTM (PTM m) k = PTM ( \s ->
  case m s of
    (# new_s, a #) -> unPTM (k a) new_s
  )

{-# INLINE thenPTM  #-}
thenPTM :: PTM a -> PTM b -> PTM b
thenPTM (PTM m) k = PTM ( \s ->
  case m s of
    (# new_s, a #) -> unPTM k new_s
  )

{-# INLINE returnPTM  #-}
returnPTM :: a -> PTM a
returnPTM x = PTM (\s -> (# s, x #))

{-# INLINE unsafeIOToPTM  #-}
-- | Unsafely performs IO in the PTM monad.
unsafeIOToPTM :: IO a -> PTM a
unsafeIOToPTM (IO m) = PTM m

-- |Perform a series of PTM actions atomically.
--
-- You cannot use 'atomically' inside an 'unsafePerformIO' or 'unsafeInterleaveIO'.
-- Any attempt to do so will result in a runtime error.  (Reason: allowing
-- this would effectively allow a transaction inside a transaction, depending
-- on exactly when the thunk is evaluated.)
--
-- However, see 'newPVarIO', which can be called inside 'unsafePerformIO',
-- and which allows top-level PVars to be allocated.

{-# INLINE atomically  #-}
atomically :: PTM a -> IO a
-- atomically = undefined
atomically (PTM c) = IO $ \s10 ->
  atomically# c s10

---------------------------------------------------------------------------------

data PVar a = PVar (TVar# RealWorld a)

pvarTc = mkTyCon3 "ghc-prim" "LwConc.Substrate" "PVar"
instance Typeable1 PVar where { typeOf1 _ = mkTyConApp pvarTc [] }

instance Eq (PVar a) where
	(PVar tvar1#) == (PVar tvar2#) = sameTVar# tvar1# tvar2#

{-# INLINE newPVar  #-}
-- |Create a new PVar holding a value supplied
newPVar :: a -> PTM (PVar a)
newPVar val = PTM $ \s1# ->
    case newTVar# val s1# of
	 (# s2#, tvar# #) -> (# s2#, PVar tvar# #)

-- |@IO@ version of 'newPVar'.  This is useful for creating top-level
-- 'PVar's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
{-# INLINE newPVarIO  #-}
newPVarIO :: a -> IO (PVar a)
newPVarIO val = IO $ \s1# ->
    case newTVar# val s1# of
	 (# s2#, tvar# #) -> (# s2#, PVar tvar# #)

-- |Return the current value stored in a PVar
{-# INLINE readPVar #-}
readPVar :: PVar a -> PTM a
readPVar (PVar tvar#) = PTM $ \s# -> readTVar# tvar# s#

-- |Write the supplied value into a PVar
{-# INLINE writePVar #-}
writePVar :: PVar a -> a -> PTM ()
writePVar (PVar tvar#) val = PTM $ \s1# ->
    case writeTVar# tvar# val s1# of
    	 s2# -> (# s2#, () #)

---------------------------------------------------------------------------------

data SCont = SCont SCont#

data ThreadStatus = Blocked | Completed

getIntStatus status =
  let (I# intStatus) = case status of
                               Blocked -> 0
                               Completed -> 1
  in intStatus


{-# INLINE newSCont #-}
newSCont :: IO () -> IO SCont
newSCont x = IO $ \s ->
   case (newSCont# x s) of (# s1, scont #) -> (# s1, SCont scont #)

{-# INLINE switchTo #-}
switchTo :: SCont -> ThreadStatus -> PTM ()
switchTo (SCont sc) status = do
  let intStatus = getIntStatus status
  PTM $ \s ->
    case (atomicSwitch# sc intStatus s) of s1 -> (# s1, () #)

{-# INLINE getSCont #-}
getSCont :: PTM SCont
getSCont = PTM $ \s10 ->
  case getSCont# s10 of
   (# s20, scont #) -> (# s20, SCont scont #)

{-# INLINE switch  #-}
switch :: (SCont -> PTM (SCont, ThreadStatus)) -> IO ()
switch arg = atomically $ do
  s1 <- getSCont
  (s2, status) <- arg s1
  switchTo s2 status
