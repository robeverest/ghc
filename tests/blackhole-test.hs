{-# LANGUAGE BangPatterns #-}

import ParRRSched
import Data.IORef (atomicModifyIORef, newIORef)
import qualified GHC.Conc as C
import System.Environment

task _ 0 = return ()
task p n = do
  atomicModifyIORef p $ \a -> let !b = a+1 in (b,b)
  task p $ n - 1

loop _ p 0 = return ()
loop sched p n = do
  forkIO sched $ task p 100000
  loop sched p $ n-1

spawnScheds _ 0 = return ()
spawnScheds s n = do
  newVProc s
  spawnScheds s $ n-1

parse (a:_) = rInt a
parse otherwise = undefined

rInt :: String -> Int
rInt = read

main = do
  args <- getArgs
  sched <- newParRRSched
  n <- C.getNumCapabilities
  spawnScheds sched $ n-1
  p <- newIORef 0
  loop sched p $ parse args
  yield sched
  print "Main done"
