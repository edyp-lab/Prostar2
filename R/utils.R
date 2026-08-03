#' @title Get the last validated step before current position.
#'
#' @description This function returns the indice of the last validated step
#' before the current step.
#'
#' @param history A `data.frame()`
#' @param step A `character()`
#' @param substep A `character()`
#' @param param.name A `character()`
#' @param value The value corresponding to the param.name
#'
#' @return A `data.frame()`
#'
#' @examples
#' history <- InitializeHistory()
#' Add2History(history, "Example step", "First sub-step", "my param", "THE value")
#'
#' @export
#'
Add2History <- function(history, step, substep, param.name, value){
  if (inherits(value, "list")) {
    value <- paste(names(value), unlist(value), collapse = ", ", sep = "=")
  }
  
  if (is.null(value)) {
    value <- NA
  }
  
  history[nrow(history) + 1, ] <- c(step, substep, param.name, value)
  
  return(history)
}



#' @title Get the history of an assay
#' 
#' @param dataIn An instance of `MultiAssayExperiment` class
#' @param x The name of a slot in the object
#'
#' @return A `data.frame()`
#'
#' @examples
#' NULL
#' 
#' @export
#' 
GetHistory <- function(dataIn, x){
    history <- NULL
    
    if (x == 'Description'){
      if ('Convert' %in% names(dataIn))
        history <- DaparToolshed::paramshistory(dataIn[['Convert']])
    } else if (x == 'Save'){
      history <- NULL
    } else if (x %in% names(dataIn)){
      history <- DaparToolshed::paramshistory(dataIn[[x]])
    }

    return(history)
  }



#' @title Get the last validated step before current position.
#'
#' @description This function returns the indice of the last validated step
#' before the current step.
#'
#' @return A `data.frame()` with four columns: 'Process', 'Step', 'Parameter'
#' and 'Value'
#'
#' @examples
#' InitializeHistory()
#'
#' @export
#'
InitializeHistory <- function() {
  history <- NULL
  history <- setNames(
    data.frame(matrix(ncol = 4, nrow = 0)),
    c("Step", "Substep", "Parameter", "Value")
  )
  
  return(history)
}



#' @title Loads packages
#' 
#' @description Checks if a package is available to load it
#' 
#' @param ll.deps A `character()` vector which contains packages names
#' 
#' @return NA
#' 
#' @examples 
#' NULL
#' 
#' @export
#' 
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' @importFrom MagellanNTK Get_Code_Declare_widgets Get_Code_for_ObserveEvent_widgets source_shinyApp_files nav_process_ui nav_process_server source_wf_files Get_Code_for_rv_reactiveValues Get_Code_Declare_rv_custom Get_Code_for_dataOut format_DT_ui format_DT_server Timestamp toggleWidget mod_popover_for_help_server mod_popover_for_help_ui
#' 
#' @author Samuel Wieczorek
#' 
pkgs_require <- function(ll.deps){
  
  if (!requireNamespace('BiocManager', quietly = TRUE)) {
    txt <- paste0("Please run install.packages('BiocManager')")
    stop(txt)
  }
  
  lapply(ll.deps, function(x) {
    if (!requireNamespace(x, quietly = TRUE)) {
      txt <- paste0("Please install ", x, ": BiocManager::install('", x, "')")
      stop(txt)
    }
  })
}


#' @title Add resource paths
#' 
#' @return NA
#' 
#' @examples
#' add_resourcePath()
#' 
#' @export
#' 
#' @importFrom shiny addResourcePath
#' @author Samuel Wieczorek
#' 
add_resourcePath <- function(){
  addResourcePath("www", system.file("app/www", package = "Prostar2"))
  addResourcePath("images", system.file("app/images", package = "Prostar2"))
}





#' @title
#' xxxx
#'
#' @description
#' xxxx
#'
#' @param typeDataset xx
#' 
#' @return NA
#' 
#' @examples
#' NULL
#'
#' @export
BuildColorStyles <- function(typeDataset) {
  mc <- DaparToolshed::metacellDef(typeDataset)
  styles <- setNames(mc$color, nm = mc$node)

  styles
}




#' @title
#' xxxx
#'
#' @description
#' xxxx
#'
#' @param obj.se xx
#' @param digits xxx
#' 
#' @return NA
#' 
#' @examples
#' NULL
#' 
#' @export
#'
Build_enriched_qdata <- function(obj.se, digits = NULL) {
  if (is.null(digits)) {
    digits <- 2
  }
  
  test.table <- as.data.frame(round(SummarizedExperiment::assay(obj.se)))
  
  if (!is.null(names(DaparToolshed::qMetacell(obj.se)))) { 
   
    colnames.data <- colnames(SummarizedExperiment::assay(obj.se))
    colnames.metadata <- colnames(DaparToolshed::qMetacell(obj.se))
    colnames.metadata <- gsub('metacell_', '', colnames.metadata)
    .ind2keep <- which(colnames.metadata %in% colnames.data)
    
    test.table <- cbind(
      round(SummarizedExperiment::assay(obj.se), digits = digits),
      DaparToolshed::qMetacell(obj.se)[ ,.ind2keep]
    )
  } else {
    test.table <- cbind(
      test.table,
      as.data.frame(
        matrix(rep(NA, ncol(test.table) * nrow(test.table)),
          nrow = nrow(test.table)
        )
      )
    )
  }
  return(test.table)
}
