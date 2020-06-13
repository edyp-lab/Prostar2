# library(highcharter)
# library(DAPAR2)
# 
# utils::data(Exp1_R25_pept, package='DAPARdata2')
# qData <- SummarizedExperiment::assay(Exp1_R25_pept[[2]])
# conds <- SummarizedExperiment::colData(Exp1_R25_pept)[['Condition']]
# hc_mvTypePlot2(qData, conds)

hc_mvTypePlot2 <- function(qData, conds, palette=NULL, title=NULL){
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
  # sink("sink-examp.txt")
  # i <- 1:10
  # outer(i, i, "*")
  # sink()
  # 
  # 
  # zz <- file("all.Rout", open = "wt")
  # sink(zz)
  # sink(zz, type = "message")
  # try(log("a"))
  # ## revert output back to the console -- only then access the file!
  # sink(type = "message")
  # sink()
  # file.show("all.Rout")
  # ################################################################################
  sink("sink-steps.txt")
  print("1. Parameters checks")
  sink()
  if (is.null(conds)){return(NULL)}
  
  sink("sink-steps.txt",append=T)
  print("2. Variables initialization")
  sink()
  if (is.null(palette)) {
    palette <- RColorBrewer::brewer.pal(length(unique(conds)),"Dark2")[1:length(unique(conds))]
  }else{
    if (length(palette) != length(unique(conds))){
      warning("The color palette has not the same dimension as the number of conditions")
      return(NULL)
    }
  }
  
  conditions <- conds
  mTemp <- nbNA <- nbValues <- matrix(rep(0,nrow(qData)*length(unique(conditions))), nrow=nrow(qData),
                                      dimnames=list(NULL,unique(conditions)))
  ymax <- 0
  series <- list()
  myColors <- NULL
  j <- 1
  
  sink("sink-steps.txt",append=T)
  print("3. plop")
  sink()
  for (iCond in unique(conditions)){
    if (length(which(conditions==iCond)) == 1) {
      
      mTemp[,iCond] <- qData[,which(conditions==iCond)]
      nbNA[,iCond] <- as.integer(is.OfType(qData[,which(conditions==iCond)]))
      nbValues[,iCond] <- length(which(conditions==iCond)) - nbNA[,iCond]
    } else {
      mTemp[,iCond] <- apply(qData[,which(conditions==iCond)], 1, mean, na.rm=TRUE)
      nbNA[,iCond] <- apply(qData[,which(conditions==iCond)],1,function(x) length(which(is.na(x) == TRUE)))
      nbValues[,iCond] <- length(which(conditions==iCond)) - nbNA[,iCond]
    }
    
    
    for (i in 1:length(which(conditions==iCond))){
      data <- mTemp[which(nbValues[, iCond] == i), iCond]
      tmp <- NULL    
      if (length(data) >= 2) {
        tmp <- density(mTemp[which(nbValues[,iCond]==i),iCond])
        tmp$y <- tmp$y + i
        if (max(tmp$y) > ymax) { ymax <- max(tmp$y)}
      }
      series[[j]] <- tmp
      myColors <- c(myColors, palette[which(unique(conditions)==iCond)])
      j <- j+1
    }
    
  }
  
  sink("sink-steps.txt",append=T)
  print("4. Making Plot")
  sink()
  hc <-  highchart() %>%
    hc_title(text = title) %>%
    dapar_hc_chart(chartType = "spline", zoomType="xy") %>%
    
    hc_legend(align = "left", verticalAlign = "top",
              layout = "vertical") %>%
    hc_xAxis(title = list(text = "Mean of intensities")) %>%
    hc_yAxis(title = list(text = "Number of quantity values per condition"),
             tickInterval= 0.5
    ) %>%
    hc_tooltip(headerFormat= '',
               pointFormat = "<b> {series.name} </b>: {point.y} ",
               valueDecimals = 2) %>%
    dapar_hc_ExportMenu(filename = "POV_distribution") %>%
    hc_plotOptions(
      series=list(
        showInLegend = TRUE,
        animation=list(
          duration = 100
        ),
        connectNulls= TRUE,
        marker=list(
          enabled = FALSE)
        
      )
    )
  
  sink("sink-steps.txt",append=T)
  print("5. Making series")
  sink()
  for (i in 1:length(series)){
    hc <- hc_add_series(hc,
                        data = list_parse(data.frame(cbind(x = series[[i]]$x, 
                                                           y = series[[i]]$y))), 
                        showInLegend=FALSE,
                        color = myColors[i],
                        name=conds[i])
  }
  
  for (c in 1:length(unique(conds))){
    hc <-  hc_add_series(hc,data = data.frame(),
                         name = unique(conds)[c],
                         color = palette[c],
                         marker = list(symbol = "circle"),
                         type = "line")
  }
  
  
  return(hc)
}
