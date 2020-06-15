
withProgress_appCalcul <- function(connexion=F){
  
  # ################################################################################
  # logfile <- tempfile(fileext=".log")
  # con <- file(logfile,open="wt")
  # sink(con, append=TRUE)
  # sink(con, append=TRUE, type="message")
  # 
  # fileReaderData <- reactiveFileReader(500, session,logfile, readLines)
  # 
  # output$fileReaderText <- renderText({
  #   text <- fileReaderData()
  #   paste(text, collapse = '\n')
  # })
  # 
  # ################################################################################

  
  if (connexion==T) { sink("sink-steps.txt") }
  print("1. Step 1")
  sink()
  
  
  if (connexion==T) { sink("sink-steps.txt",append=T) }
  print("2. Step 2")
  sink()
  
  
  if (connexion==T) { sink("sink-steps.txt",append=T) }
  print("3. Step 3")
  sink()
  
  
  if (connexion==T) { sink("sink-steps.txt",append=T) }
  print("4. Step 4")
  sink()
  
  
  hc <- plot(table(rpois(100, 5)), type = "h", col = "red", lwd = 10,
             main = "rpois(100, lambda = 5)")
  if (connexion==T) { sink("sink-steps.txt",append=T) }
  print("5. Making Plot")
  sink()
  
  return(hc)
}
