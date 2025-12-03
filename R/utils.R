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
#' add.ResourcePath()
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
#' @param obj.se xx
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
#' @param obj xx
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
    test.table <- cbind(
      round(SummarizedExperiment::assay(obj.se), digits = digits),
      DaparToolshed::qMetacell(obj.se)
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