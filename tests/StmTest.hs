module Main where

import ConcRRSched
import PChan
import MVar
import LwConc.Substrate
import qualified GHC.Conc as C

data State = State {
  vt :: PVar Int,
  vm :: MVar Int,
  chan :: PChan (),
  count :: PVar Int
  }

loopmax = 100
numthreads = 50

main
  = do t <- atomically (newPVar 0)
       m <- newEmptyMVar
       putMVar m 0
       c <- atomically (newPChan)
       cnt <- atomically (newPVar 0)
       sched <- newConcRRSched
       nc <- C.getNumCapabilities
       spawnScheds sched $ nc-1
       let st = State t m c cnt
       forkIter sched numthreads (proc st domv loopmax)
       atomically (readPChan c)
       return ()

spawnScheds _ 0 = return ()
spawnScheds s n = do
  newVProc s
  spawnScheds s $ n-1

proc :: State -> (State -> IO ()) -> Int -> IO ()
proc st w 0 = do c <- atomically (do cnt <- readPVar (count st)
                                     writePVar (count st) (cnt+1)
                                     if cnt+1 >= numthreads
                                        then writePChan (chan st) ()
                                        else return ()
                                     return cnt)
                 return ()
proc st w i
  = do w st
       proc st w (i-1)

dotv :: State -> IO ()
dotv st
  = do n <- atomically (do n <- readPVar (vt st)
                           writePVar (vt st) (n+1)
                           return n)
       return ()

domv :: State -> IO ()
domv st
  = do n <- takeMVar (vm st)
       putMVar (vm st) (n+1)
       return ()

forkIter :: ConcRRSched -> Int -> IO () -> IO ()
forkIter s n p
  = iter n (do forkIO s p
               return ())

iter :: Int -> IO () -> IO ()
iter 0 _ = return ()
iter n f
  = do f
       iter (n-1) f
