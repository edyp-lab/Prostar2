toto <- function(connexion, logFile) {
  
  if (connexion == T) {sink(logFile, split=T)}
  Sys.sleep(2)
  cat("step0\n")
  
  if (connexion == T) {sink(logFile,append=T, split=T)}
  Sys.sleep(5)
  cat("step1\n")
  
  if (connexion == T) {sink(logFile,append=T, split=T)}
  Sys.sleep(10)
  cat("step2\n")
  
  if (connexion == T) {sink(logFile,append=T, split=T)}
  Sys.sleep(4)
  cat("step3\n")
  
  if (connexion == T) {sink()}
  
}

logFile="./sink-steps.txt"
toto(T, logFile)
