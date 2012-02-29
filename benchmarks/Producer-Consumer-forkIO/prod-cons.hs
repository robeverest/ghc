import Data.Char
import Control.Concurrent
import Control.Concurrent.Chan
import System.Environment
import System.Exit

producer c v | v < 0      = return ()
             | otherwise  = do
                 writeChan c v
                 producer c (v-1)

consumer c v | v == 0 = return ()
             | otherwise = do
                 v <- readChan c
                 -- print v
                 consumer c v

parse :: [String] -> String
parse [] = "1"
parse (a:_) = a

rInt :: String -> Int
rInt = read

main = do
  args <- getArgs
  c <- newChan
  forkIO $ producer c $ rInt $ parse args
  consumer c 1 -- the initial value does not matter if the input arg is > 0
