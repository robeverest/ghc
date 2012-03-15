import Control.Concurrent
import Control.Concurrent.MVar
import System.Environment
import System.Exit

task n v = if n==1 then do
                        putMVar v ()
                 else return () -- print "RUNNING"


loop _ 0 = return ()
loop v n = do
  forkIO $ task n v
  loop v $ n-1


parse [] = "1"
parse (a:_) = a

rInt :: String -> Int
rInt = read

main = do
  args <- getArgs
  v <- newEmptyMVar
  loop v $ rInt $ parse args
  readMVar v
