import ConcRRSched

task n = do
  print $ "In thread"++show n

main = do
  sched <- newConcRRSched
  let fork 0 = return ()
      fork n = forkIO sched $ task n
  yield sched
