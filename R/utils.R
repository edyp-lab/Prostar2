

#' @title Get the last validated step before current position.
#'
#' @description This function returns the indice of the last validated step before
#' the current step.
#'
#' @param history xxx
#' @param process xxx
#' @param step.name xxx
#' @param param.name xxx
#' @param value xxx
#' @return A `integer(1)`
#'
#' @export
#' @examples
#' NULL
Add2History <- function(history, process, step.name, param.name, value){
  if (inherits(value, 'list'))
    value <- unlist(value)
  
  if (is.null(value))
    value <- NA
  
  history[nrow(history)+1, ] <- c(process, step.name, param.name, value)
  
  return(history)
}




#' @title Get the last validated step before current position.
#'
#' @description This function returns the indice of the last validated step before
#' the current step.
#'
#' @param dataIn xxx
#' @param x xxxx
#' @return A `integer(1)`
#'
#' @export
#' @examples
#' NULL
GetHistory <- function(dataIn, x){
  
  history <- NULL
  
  if (x %in% c('Description', 'Save')){
    history <- NULL
  } else if (x %in% names(dataIn)){
    history <- DaparToolshed::paramshistory(dataIn[[x]])
  }
  
  return(history)
}



#' @title Get the last validated step before current position.
#'
#' @description This function returns the indice of the last validated step before
#' the current step.
#'
#' @param widgets.names xxx
#' @return A `integer(1)`
#'
#' @export
#' @examples
#' .names <- c('A_A', 'A_Z', 'B_Q', 'B_F')
#' InitializeHistory(.names)
#' 
InitializeHistory <- function(){
  
  history <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), 
                      c('Process', 'Step', 'Parameter', 'Value'))
  
  return(history)
}






#' @title Loads packages
#' 
#' @description Checks if a package is available to load it
#' 
#' @param ll.deps A `character()` vector which contains packages names
#' 
#' @examples 
#' NULL
#' 
#' @export
#' 
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' @importFrom MagellanNTK Get_Code_Declare_widgets Get_Code_for_ObserveEvent_widgets 
#' source_shinyApp_files nav_process_ui nav_process_server source_wf_files 
#' Get_Code_for_rv_reactiveValues Get_Code_Declare_rv_custom Get_Code_for_dataOut 
#' format_DT_ui format_DT_server Timestamp toggleWidget 
#' mod_popover_for_help_server mod_popover_for_help_ui
#' 
#' @author Samuel Wieczorek
#' 
pkgs.require <- function(ll.deps){
  
  if (!requireNamespace('BiocManager', quietly = TRUE)) {
    stop(paste0("Please run install.packages('BiocManager')"))
  }
  
  lapply(ll.deps, function(x) {
    if (!requireNamespace(x, quietly = TRUE)) {
      stop(paste0("Please install ", x, ": BiocManager::install('", x, "')"))
    }
  })
}


#' @title Add resource paths
#' 
#' @examples 
#' add.resourcePath()
#' 
#' @export
#' 
#' @importFrom shiny addResourcePath
#' @author Samuel Wieczorek
#' 
add.resourcePath <- function(){
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
#' @export
BuildColorStyles <- function(typeDataset) {
  mc <- DaparToolshed::metacell.def(typeDataset)
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