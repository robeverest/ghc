import LwConc.Substrate

{- XXX KC does not work as it is intended to -}
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
