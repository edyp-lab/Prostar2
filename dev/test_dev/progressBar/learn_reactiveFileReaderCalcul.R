toto <- function(logFile) {
  
  sink(logFile, split=T)
  Sys.sleep(5)
  cat("step0\n")
  sink(logFile,append=T, split=T)
  
  for (i in 1:2) {
    Sys.sleep(5)
    cat(paste0("step",i,"\n"))
  }
  
  sink()
  
}
