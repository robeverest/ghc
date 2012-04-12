import ConcRRSched
import LwConc.Substrate
import qualified GHC.Conc as C
import System.Environment

task _ 0 = return ()
task p n = do
  let d = if n `mod` 2 == 0 then 1 else -1
  atomically $ do {
      c <- readPVar p;
      writePVar p $ c + d
  }
  task p $ n - 1

loop _ p 0 = return ()
loop sched p n = do
  forkIO sched $ task p 10000
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
  sched <- newConcRRSched
  n <- C.getNumCapabilities
  spawnScheds sched $ n-1
  p <- newPVarIO 0
  loop sched p $ parse args
  yield sched
  print "Main done"
