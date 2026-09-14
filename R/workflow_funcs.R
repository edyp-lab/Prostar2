#' @title Create filter
#'
#' @description Create filter for variable filtering
#'
#' @param value A `character(1)` or `numeric(1)` the value to apply
#' @param operator A `character(1)` of the operator used in the filter
#' @param cname A `character(1)` of the column name
#' @param keep_vs_remove A `character(1)` whether the filter is used to keep 
#'                       or to delete 
#' @param data A `QFeatures` corresponding to the dataset to which filters 
#'             are applied
#' @param i A `numeric(1)` corresponding to the SummarizedExperiment to use
#'
#' @return A `NumericVariableFilter` filter to apply on the dataset
#'
#' @examples
#' data(subR25prot, package = "DaparToolshed")
#' Variablefiltering_BuildVariableFilter(value = 2,
#'                              operator = "<",
#'                              cname = "Unique_peptides",
#'                              keep_vs_remove = "delete",
#'                              data = subR25prot,
#'                              i = 1)
#'
#' @export
#'
Variablefiltering_BuildVariableFilter <- function(
    value = NULL,
    operator = NULL,
    cname = NULL,
    keep_vs_remove = NULL,
    data = NULL,
    i = NULL){
  req(value != "Enter value..." && !is.null(value))
  req(operator != "None" && !is.null(operator))
  req(cname != "None" && !is.null(cname))
  req(!is.null(keep_vs_remove))
  req(!is.null(data))
  
  if (is.null(i)){ 
    i <- length(data)
  }
  
  rowdata <- SummarizedExperiment::rowData(data[[i]])
  col_data <- rowdata[, cname, drop = TRUE]
  expected_type <- if (is.numeric(col_data)) "numeric" else "character"
  
  val <- tryCatch(
    Extract_Value(value, expected_type),
    warning = function(w) NULL,
    error = function(e) NULL
  )
  req(val)
  
  QFeatures::VariableFilter(
    field = cname,
    value = val,
    condition = operator,
    not = keep_vs_remove == "delete"
  )
}


#' @title Write query filtering
#'
#' @description Write query for variable filtering
#'
#' @param value A `character(1)` or `numeric(1)` the value to apply
#' @param operator A `character(1)` of the operator used in the filter
#' @param cname A `character(1)` of the column name
#' @param keep_vs_remove A `character(1)` whether the filter is used to keep 
#'                       or to delete 
#' @param data A `QFeatures` corresponding to the dataset to which filters 
#'             are applied
#' @param i A `numeric(1)` corresponding to the SummarizedExperiment to use
#'
#' @return A `character`
#'
#' @examples
#' data(subR25prot, package = "DaparToolshed")
#' Variablefiltering_WriteQuery(value = 2,
#'                              operator = "<",
#'                              cname = "Unique_peptides",
#'                              keep_vs_remove = "delete",
#'                              data = subR25prot,
#'                              i = 1)
#'
#' @export
#'
Variablefiltering_WriteQuery <- function(
    value = NULL,
    operator = NULL,
    cname = NULL,
    keep_vs_remove = NULL,
    data = NULL,
    i = NULL){
  req(value != "Enter value..." && !is.null(value))
  req(operator != "None" && !is.null(operator))
  req(cname != "None" && !is.null(cname))
  req(!is.null(keep_vs_remove))
  req(!is.null(data))
  
  if (is.null(i)){ 
    i <- length(data)
  }
  
  rowdata <- SummarizedExperiment::rowData(data[[i]])
  col_data <- rowdata[, cname, drop = TRUE]
  expected_type <- if (is.numeric(col_data)) "numeric" else "character"
  
  val <- tryCatch(
    Extract_Value(value, expected_type),
    warning = function(w) NULL,
    error = function(e) NULL
  )
  req(val)
  
  query <- paste0(
    keep_vs_remove, " values for which ",
    cname, " ", operator, " ", value)
  query
}