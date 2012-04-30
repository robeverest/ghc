import ParRRSched

main = do
  sched <- newParRRSched
  forkIO sched (return () >> print "Done")
  yield sched
  print "Done"
