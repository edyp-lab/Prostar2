toto <- function(connexion, logFile) {
  
  if (connexion == T) {sink(logFile, split=T)}
  Sys.sleep(1)
  cat("step0\n")
  
  Sys.sleep(2)
  cat("step1\n")
  
  Sys.sleep(3)
  cat("step2\n")
  
  Sys.sleep(2)
  cat("step3\n")
  
  Sys.sleep(1)
  cat("end\n")
  
  Sys.sleep(1)
  
  if (connexion == T) {sink()}
  
}

logFile="./sink-steps.txt"
toto(T, logFile)
