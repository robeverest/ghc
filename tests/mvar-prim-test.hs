import ConcRRSched
import LwConc.MVarPrim
import System.Environment

putter sched m n = do
  (blockAct, unblockAct) <- getSchedActionPair sched
  print $ "Before Putting " ++ show n
  putMVarPrim blockAct unblockAct m n
  print $ "After Putting " ++ show n

taker sched m n = do
  (blockAct, unblockAct) <- getSchedActionPair sched
  print $ "Before Taking in " ++ show n
  v <- takeMVarPrim blockAct unblockAct m
  print $ "After Taking " ++ show n ++ " Value: " ++ show v

parse (a:_) = rInt a
parse otherwise = undefined

rInt :: String -> Int
rInt = read

main = do
  args <- getArgs
  let c = parse args
  m <- newEmptyMVarPrim
  sched <- newConcRRSched
  let fork t 0 = return ()
      fork t n = do
        forkIO sched $ t sched m n
        fork t $ n-1
  fork putter c
  fork taker c
  yield sched
