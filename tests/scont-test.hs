import LwConc.Substrate
import GHC.Conc


thread1 mainT = do
  print "Thread1: Hello, World!"
  switch $ \_ -> unsafeIOToSTM $ return mainT

main = do
  switch $ \mt -> unsafeIOToSTM $ newSCont $ thread1 mt
  print "Main: Bye!"
