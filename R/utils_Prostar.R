

# function to read DT inputs
#' @export
shinyValue <- function(id,num) {
  unlist(lapply(seq_len(num),function(i) {
    value <- input[[paste0(id,i)]]
    if (is.null(value)) NA else value
  }))
}









#' @export
shinyOutput <- function(FUN,id,num,...) {
  inputs <- character(num)
  for (i in seq_len(num)) {
    inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
  }
  inputs
}


# function for dynamic inputs in DT
#' @export
shinyInput <- function(FUN,id,num,...) {
  inputs <- character(num)
  for (i in seq_len(num)) {
    inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
  }
  inputs
}




# Call this function with all the regular navbarPage() parameters, plus a text parameter,
# if you want to add text to the navbar
#' @export
navbarPageWithText <- function(..., text) {
  navbar <- navbarPage(...)
  textEl <- tags$p(class = "navbar-text", text)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], textEl)
  navbar
}

# Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
# want to add an input to the navbar
#' @export
navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}



#' @export
Group2Color <- reactive({
  print(paste0("rv$settings()$whichGroup2Color = ", rv.prostar$settings()$whichGroup2Color))
  rv.prostar$settings()$whichGroup2Color
})






############################""
#' @export
getPackagesVersions <- reactive({
  type <- "all"
  outOfDate <- "(Out of date)"
  dev <- "(Devel)"
  
  biocRelease <- NULL
  DAPARdata.version <- NULL
  tryCatch({
    biocRelease <- available.packages(contrib.url("http://bioconductor.org/packages/release/bioc/"))
    require(XML)
    html <- readHTMLTable("http://bioconductor.org/packages/release/data/experiment/html/DAPARdata.html")
    DAPARdata.version <- as.character(html[[3]][2][1,])
    
  }, warning = function(w) {
    return()
  }, error = function(e) {
    return()
  }, finally = {
    #cleanup-code 
  })
  
  pkgs <- c("Prostar", "DAPAR", "DAPARdata")
  loc.pkgs <-c("Prostar.loc", "DAPAR.loc", "DAPARdata.loc")
  instPkgs <- list(Prostar = installed.packages(lib.loc=Prostar.loc)["Prostar2","Version"],
                   DAPAR = installed.packages(lib.loc=DAPAR.loc)["DAPAR","Version"],
                   DAPARdata = installed.packages(lib.loc=DAPARdata.loc)["DAPARdata","Version"])
  
  
  names <- c(as.character(tags$a(href="http://www.bioconductor.org/packages/release/bioc/html/Prostar.html", "Prostar")), 
             as.character(tags$a(href="http://www.bioconductor.org/packages/release/bioc/html/DAPAR.html", "DAPAR")), 
             as.character(tags$a(href="http://www.bioconductor.org/packages/release/data/experiment/html/DAPARdata.html", "DAPARdata")))
  
  
  df <- data.frame("Name" = names,
                   "Installed.packages"= rep(NA, 3), 
                   "Bioc.release" =  rep(NA, 3),
                   "NeedsUpdate"= rep(FALSE,3),
                   stringsAsFactors = FALSE)
  
  
  df[, "Installed.packages"] <- unlist(instPkgs)
  
  if (!is.null(biocRelease)) {
    biocPkgs <- list(Prostar = as.character(biocRelease["Prostar","Version"]),
                     DAPAR = as.character(biocRelease["DAPAR","Version"]),
                     DAPARdata = as.character(DAPARdata.version))
    
    if (compareVersion(instPkgs$Prostar,biocPkgs$Prostar) == 0){df[1,"Name"] <-  names[1]}
    else if (compareVersion(instPkgs$Prostar,biocPkgs$Prostar) == 1){df[1,"Name"] <-   paste(names[1],  "<strong>",dev, "</strong>", sep=" ")}
    else if (compareVersion(instPkgs$Prostar,biocPkgs$Prostar)==-1){
      df[1,"Name"] <-   paste(names[1], "<strong>", outOfDate, "</strong>", sep=" ")
      inst <- unlist(strsplit(instPkgs$Prostar, split=".", fixed=TRUE))
      bioc <- unlist(strsplit(biocPkgs$Prostar, split=".", fixed=TRUE))
      df[1,"NeedsUpdate"] <- ((inst[2]==bioc[2] && (as.numeric(inst[3]) < as.numeric(bioc[3]))))
    }
    
    if (compareVersion(instPkgs$DAPAR,biocPkgs$DAPAR) == 0){df[2,"Name"] <-  names[2]}
    else if (compareVersion(instPkgs$DAPAR , biocPkgs$DAPAR) == 1){df[2,"Name"] <-   paste(names[2],  "<strong>",dev, "</strong>", sep=" ")}
    else if (compareVersion(instPkgs$DAPAR , biocPkgs$DAPAR)==-1){
      df[2,"Name"] <-   paste(names[2],  "<strong>",outOfDate, "</strong>", sep=" ")
      inst <- unlist(strsplit(instPkgs$DAPAR, split=".", fixed=TRUE))
      bioc <- unlist(strsplit(biocPkgs$DAPAR, split=".", fixed=TRUE))
      df[2,"NeedsUpdate"] <- ((inst[2]==bioc[2] && (as.numeric(inst[3]) < as.numeric(bioc[3]))))
    }
    
    if (compareVersion(instPkgs$DAPARdata,biocPkgs$DAPARdata) == 0){df[3,"Name"] <-  names[3]}
    else if (compareVersion(instPkgs$DAPARdata , biocPkgs$DAPARdata) == 1){df[3,"Name"] <-   paste(names[3],  "<strong>",dev, "</strong>", sep=" ")}
    else if (compareVersion(instPkgs$DAPARdata , biocPkgs$DAPARdata)==-1){
      df[3,"Name"] <-   paste(names[3],  "<strong>",outOfDate, "</strong>", sep=" ")
      inst <- unlist(strsplit(instPkgs$DAPARdata, split=".", fixed=TRUE))
      bioc <- unlist(strsplit(biocPkgs$DAPARdata, split=".", fixed=TRUE))
      df[3,"NeedsUpdate"] <- ((inst[2]==bioc[2] && (as.numeric(inst[3]) < as.numeric(bioc[3]))))
    }
    df[, "Bioc.release"] <- unlist(biocPkgs)
  }
  
  
  colnames(df) <- c("Names", "Installed packages", "Bioc release","NeedsUpdate")
  
  switch(type,
         all=df <- df,
         installed = {
           df <- df[,1:2]
           df[,1] <- c('Prostar', 'DAPAR', 'DAPARdata')
         }
  )
  df
  #}
  
})





###-------------------------------------
#' @export
Compute_PCA_nbDimensions <- reactive({
  # ncp should not be greater than...
  nmax <- 12  
  # pour info, ncp = nombre de composantes ou de dimensions dans les r?sultats de l'ACP
  
  y <- Biobase::exprs(rv$current.obj)
  nprot <- dim(y)[1]
  # If too big, take the number of conditions.
  n <- dim(y)[2] 
  
  if (n > nmax){
    n <- length(unique(Biobase::pData(rv$current.obj)$Condition))
  }
  
  
  ncp <- min(n, nmax)
  ncp
})




#' @export
GetOnlineZipVersion <- function(){
  
  thepage <- readLines('http://prabig-prostar.univ-lyon1.fr/ProstarZeroInstall/')
  substr(thepage[12], regexpr("Prostar_",thepage[12])[1], 2+regexpr("zip",thepage[12])[1])
  
  
  thetable <- readHTMLTable('http://prabig-prostar.univ-lyon1.fr/ProstarZeroInstall/', stringsAsFactors=FALSE)
  onlineZipVersion <- thetable[[1]]$Name[3]
  
  return(onlineZipVersion)
}



#' @export
launchGA <- function(){
  if (system('hostname')=="prabig-prostar"){
    tags$head(includeScript("www/google-analytics.js"))
  } else {
    #tags$head(includeScript("www/google-analytics-ProstarZeroInstall.js"))
  }
  
}


# Dans mod_msnset_explorer.R
#' @export
initComplete <- function(){
  return (JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': 'darkgrey', 'color': 'black'});",
    "}"))
} #comonFunc.R de prostar 2.0