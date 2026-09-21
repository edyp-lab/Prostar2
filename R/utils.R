#' @title Add row to history
#'
#' @description This function adds a row to the history.
#'
#' @param history A `data.frame` corresponding to the current history.
#' @param step A `character(1)` corresponding to the process name.
#' @param substep A `character(1)` corresponding to the substep name.
#' @param param.name A `character(1)` corresponding to the parameter name.
#' @param value The value of the corresponding parameter.
#' 
#' @return A `data.frame` with one added row
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



#' @title Initialize the history
#'
#' @description This function initializes the history.
#'
#' @return An empty `data.frame` with 4 columns ('Step', 'Substep', 'Parameter' and 'Value')
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


#' @title
#' xxxx
#'
#' @description
#' xxxx
#'
#' @param value xx
#' @param expected_type xxx
#' 
#' @return NA
#' 
#' @examples
#' NULL
#' 
#' @export
#'

Extract_Value <- function(value, expected_type = c("numeric", "character", "logical", "factor", "integer")) {
  expected_type <- match.arg(expected_type)
  
  tryCatch({
    switch(
      expected_type,
      numeric = as.numeric(value),
      character = as.character(value),
      logical = as.logical(value),
      factor = as.factor(value),
      integer = as.integer(value)
    )
  },
  warning = function(w) NA,
  error = function(e) NA
  )
}


#' @title Get filters scope
#'
#' @description Get the list of possible scopes for filters
#'
#' @return A `list`
#'
#' @examples
#' GetFiltersScope()
#'
#' @export
#'
GetFiltersScope <- function(){
  c("Whole Line" = "WholeLine",
    "Whole matrix" = "WholeMatrix",
    "For every condition" = "AllCond",
    "At least one condition" = "AtLeastOneCond"
  )
}


#' @title Numeric test
#'
#' @description Test whether an object is numeric or not
#'
#' @param input The object to test
#'
#' @return `NULL` if numeric, a `character` if not
#'
#' @examples
#' not_a_numeric(1)
#' not_a_numeric("A")
#'
#' @export
#'
not_a_numeric <- function(input) {
  if (is.na(as.numeric(input))) {
    "Please input a number"
  } else {
    NULL
  }
}


#' @title Object contained in another one
#'
#' @description Check whether an object contained in another one
#'
#' @param strA Object to find
#' @param strB Object to check
#' 
#' @return A `logical(1)`
#'
#' @examples
#' isContainedIn("A", c("A", "B"))
#' isContainedIn("A", c("ABCDE", "FGHIJ"))
#'
#' @export
#'
isContainedIn <- function(strA, strB) {
  return(all(strA %in% strB))
}
