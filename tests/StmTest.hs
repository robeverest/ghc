module Main where

import ParRRSched
import PChan
import MVar
import LwConc.Substrate
import qualified GHC.Conc as C

data State = State {
  vt :: PVar Int,
  vm :: MVar Int,
  chan :: MVar (),
  count :: PVar Int
  }

loopmax = 1
numthreads = 2

main
  = do t <- atomically (newPVar 0)
       m <- newEmptyMVar
       putMVar m 0
       c <- newEmptyMVar
       cnt <- atomically (newPVar 0)
       sched <- newParRRSched
       nc <- C.getNumCapabilities
       spawnScheds sched $ nc-1
       let st = State t m c cnt
       forkIter sched numthreads (proc st domv loopmax)
       takeMVar c
       yield sched
       return ()

spawnScheds _ 0 = return ()
spawnScheds s n = do
  newVProc s
  spawnScheds s $ n-1

proc :: State -> (State -> IO ()) -> Int -> IO ()
proc st w 0 = do c <- atomically (do cnt <- readPVar (count st)
                                     writePVar (count st) (cnt+1)
                                     if cnt+1 >= numthreads
                                        then do {
                                          unsafeIOToPTM $ print "Finishing...";
                                          asyncPutMVar (chan st) ()
                                        }
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
       print "doMV: before putMVar"
       putMVar (vm st) (n+1)
       print "doMV: after putMVar"
       return ()

forkIter :: ParRRSched -> Int -> IO () -> IO ()
forkIter s n p
  = iter n (do forkIO s p
               return ())

iter :: Int -> IO () -> IO ()
iter 0 _ = return ()
iter n f
  = do f
       iter (n-1) f
