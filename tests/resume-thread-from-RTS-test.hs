import LwConc.Substrate

main = do
  let resumeThreadClosure = do {
    print "***** In RTC *****"
  }
  let task = do {
    print "***** Task Done *****"
  }
  let spawnTask = do {
    s <- newSCont task;
    setResumeThreadClosure s resumeThreadClosure
  }
  spawnTask
  s <- atomically $ getSCont
  print "GotSCont"
  -- forceRTCEval s
  print "***** Main Done *****"
