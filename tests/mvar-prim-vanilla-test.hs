import Control.Concurrent
import System.Environment

putter m n = do
  -- print $ "Before Putting " ++ show n
  putMVar m n
  -- print $ "After Putting " ++ show n

taker m n = do
  -- print $ "Before Taking in " ++ show n
  v <- takeMVar m
  -- print $ "After Taking " ++ show n ++ " Value: " ++ show v
  return ()

parse (a:_) = rInt a
parse otherwise = undefined

rInt :: String -> Int
rInt = read

main = do
  args <- getArgs
  let c = parse args
  m <- newEmptyMVar
  let fork t 0 = return ()
      fork t n = do
        forkIO $ t m n
        fork t $ n-1
  fork putter c
  fork taker c
  yield
