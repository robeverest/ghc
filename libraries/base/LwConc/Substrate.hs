{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , ForeignFunctionInterface
           , MagicHash
           , UnboxedTuples
           , ScopedTypeVariables
  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -w -XBangPatterns #-}

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
, unsafeIOToPTM           -- IO a -> PTM a
, atomically              -- PTM a -> IO a

, PVar
, newPVar                 -- a -> PTM (PVar a)
, newPVarIO               -- a -> IO (PVar a)
, readPVar                -- PVar a -> PTM a
, writePVar               -- PVar a -> a -> PTM ()

, SCont
, ThreadStatus (..)
, setThreadStatus         -- SCont -> ThreadStatus -> PTM ()
, getThreadStatus         -- SCont -> PTM ThreadStatus

, initSubstrate           -- IO ()
, newSCont                -- IO () -> IO SCont
, switch                  -- (SCont -> PTM SCont) -> IO ()
, getSCont                -- PTM SCont
, getSContIO              -- IO SCont
, switchTo                -- SCont -> PTM ()

#ifdef __GLASGOW_HASKELL__
, newBoundSCont           -- IO () -> IO SCont
, isCurrentThreadBound    -- IO Bool
, rtsSupportsBoundThreads -- Bool
#endif

, setResumeThread         -- SCont -> PTM () -> IO ()
, setSwitchToNextThread   -- SCont -> PTM () -> IO ()
, setFinalizer            -- SCont -> IO () -> IO ()
) where


import Prelude
import Control.Exception.Base as Exception

#ifdef __GLASGOW_HASKELL__
import GHC.Exception
import GHC.Base
import GHC.Prim
import GHC.IO
import Control.Monad    ( when )
#endif

import GHC.Conc (yield, childHandler)
import Data.Typeable
import Foreign.StablePtr
import Foreign.C.Types

----------------------------------------------------------------------------
-- PTM
----------------------------------------------------------------------------

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
-- PVar
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
-- One-shot continuations (SCont)
---------------------------------------------------------------------------------

data ThreadStatus = Running |
                    Yielded |
                    BlockedOnConcDS |
                    BlockedOnBlackHole |
                    Completed

data SCont = SCont SCont# (PVar ThreadStatus)

getStatusFromInt x | x == 0 = Running
                   | x == 1 = Yielded
                   | x == 2 = BlockedOnConcDS
                   | x == 3 = BlockedOnBlackHole
                   | x == 4 = Completed

getIntFromStatus Running = 0#
getIntFromStatus Yielded = 1#
getIntFromStatus BlockedOnConcDS = 2#
getIntFromStatus BlockedOnBlackHole = 3#
getIntFromStatus Completed = 4#

{-# INLINE getThreadStatus #-}
getThreadStatus :: SCont -> PTM ThreadStatus
getThreadStatus (SCont _ ts) = readPVar ts

{-# INLINE setThreadStatus #-}
setThreadStatus :: SCont -> ThreadStatus -> PTM ()
setThreadStatus (SCont _ ts) status = writePVar ts status

{-# INLINE initSubstrate #-}
initSubstrate :: IO ()
initSubstrate = do
  status <- newPVarIO Running
  let PVar tvarStatus# = status
  IO $ \s ->
    case (setSContStatus# tvarStatus# s) of s -> (# s, () #)

{-# INLINE newSCont #-}
newSCont :: IO () -> IO SCont
newSCont x = do
  status <- newPVarIO Yielded
  let PVar tvarStatus# = status
  IO $ \s ->
   case (newSCont# x tvarStatus# s) of (# s1, scont #) -> (# s1, SCont scont status #)

{-# INLINE switchToWithStatus #-}
switchToWithStatus :: SCont -> Int# -> PTM ()
switchToWithStatus (SCont targetSContPrim targetStatusPVar) intStatus = do
  writePVar targetStatusPVar Running
  PTM $ \s ->
    case (atomicSwitch# targetSContPrim intStatus s) of s1 -> (# s1, () #)

{-# INLINE switchTo #-}
switchTo :: SCont -> PTM ()
switchTo targetSCont = do
  SCont _ currentStatusPVar <- getSCont
  currentStatus <- readPVar currentStatusPVar
  let intStatus = getIntFromStatus currentStatus
  switchToWithStatus targetSCont intStatus

{-# INLINE getSCont #-}
getSCont :: PTM SCont
getSCont = PTM $ \s10 ->
  case getSCont# s10 of
   (# s20, scont, status #) -> (# s20, SCont scont (PVar status) #)

{-# INLINE getSContIO #-}
getSContIO :: IO SCont
getSContIO = IO $ \s10 ->
  case getSCont# s10 of
   (# s20, scont, status #) -> (# s20, SCont scont (PVar status) #)

{-# INLINE switch  #-}
switch :: (SCont -> PTM SCont) -> IO ()
switch arg = atomically $ do
  currentSCont <- getSCont
  targetSCont <- arg currentSCont
  -- Get int# of current thread's status
  let SCont _ currentStatusPVar = currentSCont
  currentStatus <- readPVar currentStatusPVar
  let intStatus = getIntFromStatus currentStatus
  switchToWithStatus targetSCont intStatus

{-# INLINE setResumeThread #-}
setResumeThread :: SCont -> PTM () -> IO ()
setResumeThread (SCont sc _) r = IO $ \s ->
  case (setResumeThread# sc rp s) of s -> (# s, () #)
       where rp = atomically $ r

{-# INLINE setSwitchToNextThread #-}
setSwitchToNextThread :: SCont -> PTM () -> IO ()
setSwitchToNextThread (SCont sc ts) b = IO $ \s ->
  case (setSwitchToNextThread# sc bp s) of s -> (# s, () #)
       where bp = \intStatus -> atomically $ do {
               writePVar ts $ getStatusFromInt intStatus; b }


{-# INLINE setFinalizer #-}
setFinalizer :: SCont -> IO () -> IO ()
setFinalizer (SCont sc _) b = IO $ \s ->
  case (setFinalizer# sc b s) of s -> (# s, () #)

----------------------------------------------------------------------------
-- Bound threads
----------------------------------------------------------------------------

-- | 'True' if bound threads are supported.
-- If @rtsSupportsBoundThreads@ is 'False', 'isCurrentThreadBound'
-- will always return 'False' and both 'forkOS' and 'runInBoundThread' will
-- fail.
foreign import ccall rtsSupportsBoundThreads :: Bool

-- | Returns 'True' if the calling thread is /bound/, that is, if it is
-- safe to use foreign libraries that rely on thread-local state from the
-- calling thread.
isCurrentThreadBound :: IO Bool
isCurrentThreadBound = IO $ \ s# ->
    case isCurrentThreadBound# s# of
        (# s2#, flg #) -> (# s2#, not (flg ==# 0#) #)

isThreadBound :: SCont -> IO Bool
isThreadBound (SCont sc ts) = IO $ \ s# ->
    case isThreadBound# sc s# of
        (# s2#, flg #) -> (# s2#, not (flg ==# 0#) #)

failNonThreaded :: IO a
failNonThreaded = fail $ "RTS doesn't support multiple OS threads "
                       ++"(use ghc -threaded when linking)"

foreign import ccall forkOS_createThreadForSCont
    :: StablePtr (SCont) -> IO CInt

newBoundSCont :: IO () -> IO SCont
newBoundSCont action0
  | rtsSupportsBoundThreads = do
        b <- Exception.getMaskingState
        let
            -- async exceptions are masked in the child if they are masked
            -- in the parent, as for forkIO (see #1048). forkOS_createThread
            -- creates a thread with exceptions masked by default.
            action1 = case b of
                        Unmasked -> unsafeUnmask action0
                        MaskedInterruptible -> action0
                        MaskedUninterruptible -> uninterruptibleMask_ action0
            action_plus = Exception.catch action1 childHandler
        s <- newSCont action_plus
        entry <- newStablePtr s
        err <- forkOS_createThreadForSCont entry
        when (err /= 0) $ fail "Cannot create OS thread."
        -- Wait for initialization
        let wait = do {
          r <- isThreadBound s;
          if r
             then
               return ()
             else do {
               yield;
               wait
               }
        }
        wait
        freeStablePtr entry
        return s
  | otherwise = failNonThreaded
