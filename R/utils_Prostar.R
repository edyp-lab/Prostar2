

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
shinyInput <- function(FUN, id ,num,...) {
  inputs <- character(num)
  for (i in seq_len(num)) {
    inputs[i] <- as.character(FUN(paste0(id, i),label=NULL,...))
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




###-------------------------------------
#' @export
#' @importFrom shiny reactive
Compute_PCA_nbDimensions <- shiny::reactive({
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
  
  
  thetable <- XML::readHTMLTable('http://prabig-prostar.univ-lyon1.fr/ProstarZeroInstall/', stringsAsFactors=FALSE)
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