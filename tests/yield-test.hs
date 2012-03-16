import GHC.Conc

main = do
  let task = return ()
  forkIO $ task
  yield
