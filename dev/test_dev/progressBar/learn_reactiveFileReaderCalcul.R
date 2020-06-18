
toto <- function(logFile) {
  
  #sink(logFile, split=T)
  
  writeLines("step0",logFile)
  Sys.sleep(5)
  #sink(logFile,append=T, split=T)
  for (i in 1:2) {
    Sys.sleep(5)
    writeLines(paste0("step",i), logFile)
  }
  #sink()
  
}

