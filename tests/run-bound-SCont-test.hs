import LwConc.Substrate

main = do
  mainS <- atomically $ getSCont
  let task = do {
    print "Task: done";
    switch (\_ -> return (mainS, Completed))
  }
  s <- newBoundSCont task
  putStrLn "Main: switching to task"
  switch (\_ -> return (s, BlockedOnSched))
  putStrLn "Main: done"
