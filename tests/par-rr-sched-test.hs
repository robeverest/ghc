import ParRRSched
import System.Environment
import qualified GHC.Conc as C

task n = do
  print $ "Running" ++ show n
  return ()

loop _ tick (0, maxTick) = return ()
loop sched tick (n, maxTick) = do
  forkIO sched $ task n
  tick <- if tick == maxTick
             then do {
              print "Main yield";
              yield sched;
              return 0
             }
             else return (tick+1)
  loop sched tick (n-1, maxTick)


parse (a:b:_) = (rInt a, rInt b)
parse otherwise = undefined

rInt :: String -> Int
rInt = read

main = do
  args <- getArgs
  sched <- newParRRSched
  n <- C.getNumCapabilities
  spawnScheds sched $ n-1
  loop sched 0 $ parse args
  -- yield sched

spawnScheds _ 0 = return ()
spawnScheds s n = do
  newVProc s
  spawnScheds s $ n-1
