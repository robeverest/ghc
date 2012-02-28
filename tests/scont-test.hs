import LwConc.Substrate

thread1 mainT = do
  print "Thread1: Hello, World!"
  switch $ \_ -> unsafeIOToPTM $ return mainT

main = do
  switch $ \mt -> unsafeIOToPTM $ newSCont $ thread1 mt
  print "Main: Bye!"
