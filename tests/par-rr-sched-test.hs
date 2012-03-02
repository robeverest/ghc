import ConcRRSched
import LwConc.MVarPrim
import System.Environment

parse (a:b:_) = return (rInt a, rInt b)
parse otherwise = undefined

rInt :: String -> Int
rInt = read

main = do
  -- Initialization
  args <- getArgs
  sched <- newConcRRSched
  mv <- newEmptyMVarPrim
  (numThreads, maxTick) <- parse args
  (blockAct, unblockAct) <- getSchedActionPair sched
  -- task
  let task n = do {
    print $ "Running" ++ show n;
    (b, u) <- getSchedActionPair sched;
    putMVarPrim b u mv ()
  }
  -- Create the threads
  let loop _ 0 = return ()
      loop tick n = do
        forkIO sched $ task n
        tick <- if tick == maxTick
                  then do {
                    yield sched;
                    return 0
                  }
                  else return (tick+1)
        loop tick $ n-1
  loop 0 numThreads
  -- Wait for thread finish
  let wait finishedThreads = if finishedThreads==numThreads
                                then return ()
                                else do
                                  takeMVarPrim blockAct unblockAct mv
                                  wait $ finishedThreads + 1
  wait 0
