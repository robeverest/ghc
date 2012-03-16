import LwConc.Substrate

main = do
  s <- atomically $ getSCont
  atomically $ switchTo s BlockedOnConcDS
  print "Main done"
