import LwConc.Schedulers.ConcRRSched

task n = do
  print $ "In thread"++show n

main = do
  sched <- newConcRRSched
  let fork 0 = return ()
      fork n = do
        forkIO sched $ task n
        fork $ n-1
  fork 100
  yield sched
