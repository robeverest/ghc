import Control.Concurrent
import System.Environment
import System.Exit

task n = print "RUNNING"


loop 0 = return ()
loop n = do
  forkIO $ task n
  loop $ n-1


parse [] = "1"
parse (a:_) = a

rInt :: String -> Int
rInt = read

main = do
  args <- getArgs
  loop $ rInt $ parse args
  yield
