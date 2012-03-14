import ConcRRSched
import qualified LwConc.Substrate as Substrate
import LwConc.MVarPrim
import System.Environment

task n = do
  print $ "Running" ++ show n
  return ()

parse (a:b:_) = (rInt a, rInt b)
parse otherwise = undefined

rInt :: String -> Int
rInt = read

main = do
  -- Initialize
  args <- getArgs
  sched <- newConcRRSched
  let (n, maxTick) = parse args
  -- Define zombie task
  let zombie = do {
    mv <- newEmptyMVarPrim;
    s <- Substrate.atomically $ Substrate.getSCont;
    (b, u) <- getSchedActionPair sched s;
    takeMVarPrim b u mv;
    print "Zombie: should not see this"
  }
  -- Define loop
  let loop tick 0 = return ()
      loop tick n = do {
        -- create Zombie
        z <- forkIO sched zombie;
        nextTick <- (if tick == maxTick
                        then do {
                              print "Main yield";
                              yield sched;
                              return 0
                            }
                       else return (tick+1));
        loop nextTick $ n-1
      }
  -- invoke loop
  loop 0 n
  yield sched
