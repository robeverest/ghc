import ConcRRSched
import System.Environment

task n = do
  --print $ "Running" ++ show n
  return ()

loop _ 0 = return ()
loop s n = do
  forkIO s $ task n
  yield s
  loop s $ n-1


parse [] = "1"
parse (a:_) = a

rInt :: String -> Int
rInt = read

main = do
  args <- getArgs
  sched <- newConcRRSched
  loop sched $ rInt $ parse args
  yield sched
