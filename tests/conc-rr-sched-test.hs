import ConcRRSched

task n = do
  print ("In thread"++n)

main = do
  m <- newEmptyMVarPrim
  sched <- newConcRRSched
  let fork 0 = return ()
      fork n = forkIO sched $ task n
  yield sched
