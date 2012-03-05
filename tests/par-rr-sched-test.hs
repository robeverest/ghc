import ParRRSched
import LwConc.Substrate
import LwConc.MVarPrim
import System.Environment
import Control.Concurrent (myThreadId, threadCapability, forkOn, getNumCapabilities)

parse (a:b:_) = return (rInt a, rInt b)
parse otherwise = undefined

rInt :: String -> Int
rInt = read

main = do
  -- Initialization
  args <- getArgs
  sched <- newParRRSched
  mv <- newEmptyMVarPrim
  (numThreads, maxTick) <- parse args
  numCaps <- getNumCapabilities
  mainSCont <- atomically $ getSCont
  let blockAct = switchToNext sched (\_ -> return ())
  let unblockAct = scheduleThread mainSCont
  -- Worker
  let worker = atomically $ switchToNext sched $ \s -> return ()
  let spawnWorkers n | n == numCaps = return ()
                     | otherwise    = do {
                          forkOn n worker;
                          spawnWorkers $ n+1
                          }
  spawnWorkers 1
  -- task
  let task n = do {
    tid <- myThreadId;
    (t, _) <- threadCapability tid;
    print $ "Running task " ++ show n ++ " On " ++ show t;
    (b, u) <- getSchedActionPair sched;
    putMVarPrim b u mv ();
    -- print $ "Finishing task " ++ show n
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
  finalExit <- newEmptyMVarPrim
  let loop finishedThreads (b,u) = if finishedThreads==numThreads
                                then putMVarPrim b u finalExit ()
                                else do {
                                  takeMVarPrim b u mv;
                                  loop (finishedThreads + 1) (b,u);
                                  }
  let wait = do {
    (b,u) <- getSchedActionPair sched;
    loop 0 (b,u)
    }
  forkIO sched wait
  takeMVarPrim blockAct unblockAct finalExit
