#' @title Save text UI
#'
#' @description The UI for the text and its appearance in each save
#'
#' @return The corresponding HTML
#'
#' @examples
#' save_txt_ui()
#'
#' @export
#'
save_txt_ui <- function() {
  div(
    style = "margin: 25px;",
    p(
      HTML(
        "Click <b>'Run'</b> to validate this step.<br>
         If you need to make changes, click <b>'Reset'</b>."
      ),
      style = "
        font-size: 17px;
        line-height: 1.6;
        margin: 0;
        padding: 12px 16px;
        background-color: #EAEAEA;
        border-radius: 4px;
      "
    )
  )
}


#' @title End process save dataset
#'
#' @description Do the necessary steps at the end of a process
#'
#' @param data A `QFeatures`
#' @param i A `numeric(1)`, SE to work on
#' @param history A `data.frame`, contains the history to add to 
#' the designated SE
#' @param namePipeline A `character(1)`, the pipeline type
#' @param SEname A `character(1)`, name to give to the designated SE
#'
#' @return A `QFeatures` 
#'
#' @examples
#' NULL
#'
#' @export
#'
prepareQFsave <- function(data, 
                          i = NULL,
                          history,
                          namePipeline = 'PipelineProtein', 
                          SEname = NULL) {
  if (missing(data)) {
    stop("'data' is required.")
  }
  if (!inherits(data, "QFeatures")) {
    stop("'data' must be an object of class QFeatures.")
  }
  if (!is.null(i) && (!is.numeric(i) || (length(i) != 1))){
    stop("'i' must be a numeric of length 1.")
  }
  if (missing(history)) {
    stop("'history' is required.")
  }
  if (!is.data.frame(history)) {
    stop("'history' must be an object of class data.frame.")
  }
  if (!is.character(namePipeline) || is.na(namePipeline) || (length(namePipeline) != 1)) {
    stop("'namePipeline' must be a character of length 1.")
  }
  if (!is.null(SEname) && (!is.character(SEname)  || is.na(SEname) || (length(SEname) != 1))) {
    stop("'SEname' must be a character of length 1.")
  }
  
  if (is.null(i)){
    i <- length(data)
  }
  if (!is.null(SEname)){
    names(data)[i] <- SEname
  }
  
  S4Vectors::metadata(data)$name.pipeline <- namePipeline
  DaparToolshed::paramshistory(data[[i]]) <- rbind(DaparToolshed::paramshistory(data[[i]]),
                                                   history)
  return(data)
}


#' @title Read file
#'
#' @description Read a file uploaded
#'
#' @param file The object from a fileInput widget
#' @param sheet The name of the sheet if the dataset is an excel
#'
#' @return The dataset
#'
#' @examples
#' NULL
#'
#' @export
#'
readFileConvert <- function(file,
                            sheet = NULL){
  tryCatch({
    ext <- MagellanNTK::GetExtension(file$name)
    f.path <- file$datapath
    data <- switch(ext,
                   txt = read.csv(f.path, header = TRUE, sep = "\t", as.is = T),
                   csv = read.csv(f.path, header = TRUE, sep = ";", as.is = T),
                   tsv = read.csv(f.path, header = TRUE, sep = "\t", as.is = T),
                   xls = DaparToolshed::readExcel(f.path, sheet = sheet),
                   xlsx = DaparToolshed::readExcel(f.path, sheet = sheet)
                   )
    
    colnames(data) <- gsub(".", "_", colnames(data), fixed = TRUE)
    colnames(data) <- gsub(" ", "_", colnames(data), fixed = TRUE)
  },
  warning = function(w) {
    shinyjs::info(conditionMessage(w))
    return(NULL)
  },
  error = function(e) {
    shinyjs::info(conditionMessage(e))
    return(NULL)
  },
  finally = {
    # cleanup-code
  })
  
  return(data)
}
