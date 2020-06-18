toto <- function(connexion, logFile) {
  
  if (connexion == T) {sink(logFile, split=T)}
  Sys.sleep(2)
  cat("step0\n")
  if (connexion == T) {sink(logFile,append=T, split=T)}
  
  for (i in 1:2) {
    Sys.sleep(5)
    cat(paste0("step",i,"\n"))
  }
  
  if (connexion == T) {sink()}
  
}

logFile="./sink-steps.txt"
toto(T, logFile)