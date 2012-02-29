import Data.Char
import LwConc.Substrate
import Control.Concurrent.Chan
import Control.Concurrent
import System.Environment
import System.Exit

producer c v | v < 0      = return ()
             | otherwise  = do
                 writeChan c v
                 producer c (v-1)

consumer c v | v == 0 = exitWith ExitSuccess
             | otherwise = do
                 v <- readChan c
                 consumer c v

parse :: [String] -> String
parse [] = "1"
parse (a:_) = a

rInt :: String -> Int
rInt = read

dummyLoop = do
  yield
  dummyLoop


main = do
  args <- getArgs
  c <- newChan
  forkHEC $ producer c $ rInt $ parse args
  forkIO $ dummyLoop
  consumer c 1 -- the initial value does not matter if the input arg is > 0
