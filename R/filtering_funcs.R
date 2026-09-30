#' @title Cell metadata filtering
#'
#' @description Perform cell metadata filtering
#'
#' @param data A `QFeatures`
#' @param filters The filter
#' @param query A `character(1)`, the query for the filtering
#' @param history A `data.frame`, the history
#' @param original_length A `numeric(1)`, the length of the dataset prefiltering
#'
#' @return A `list` containing the filtered dataset, important values 
#' and the history
#'
#' @examples
#' NULL
#'
#' @export
#'
cellmetadataFiltering <- function(data,
                                  filters,
                                  query,
                                  history,
                                  original_length) {
  # Apply filter
  tmp <- DaparToolshed::filterFeaturesOneSE(object = data,
                                            i = length(data),
                                            name = paste0("qMetacellFiltered", MagellanNTK::Timestamp()),
                                            filters = filters)
  
  # Information about filtering
  nBefore <- nrow(tmp[[length(tmp) - 1]])
  nAfter  <- nrow(tmp[[length(tmp)]])
  
  nbDeleted  <- nBefore - nAfter
  nbRemaining <- nrow(SummarizedExperiment::assay(tmp[[length(tmp)]]))
  
  # Keep only the newly filtered assay
  len_diff <- length(tmp) - original_length
  
  req(len_diff > 0)
  
  if (len_diff == 2) {
    dataOut <- QFeatures::removeAssay(tmp, length(tmp) - 1)
  } else {
    dataOut <- tmp
  }
  
  # Rename filtered dataset
  names(dataOut)[length(dataOut)] <- "Cellmetadatafiltering"
  
  # Update history
  history <- Prostar2::Add2History(history, "Filtering", "Cellmetadatafiltering", "query", query)
  
  return(list(data = dataOut,
              summary = c(query, nbDeleted, nbRemaining),
              history = history))
}


#' @title Variable filtering
#'
#' @description Perform variable filtering
#'
#' @param data A `QFeatures`
#' @param widgets A `list()`, all widget values
#' @param history A `data.frame`, the history
#' @param original_length A `numeric(1)`, the length of the dataset prefiltering
#'
#' @return A `list` containing the filtered dataset, important values
#' and the history
#'
#' @examples
#' NULL
#'
#' @export
#'
variableFiltering <- function(data,
                              widgets,
                              history,
                              original_length){
  # Build filter and query
  ll.var <- list(
    Variablefiltering_BuildVariableFilter(
      value = widgets$Variablefiltering_value,
      operator = widgets$Variablefiltering_operator,
      cname = widgets$Variablefiltering_cname,
      keep_vs_remove = widgets$Variablefiltering_keep_vs_remove,
      data = data
    )
  )

  ll.query <- list(
    Variablefiltering_WriteQuery(
      value = widgets$Variablefiltering_value,
      operator = widgets$Variablefiltering_operator,
      cname = widgets$Variablefiltering_cname,
      keep_vs_remove = widgets$Variablefiltering_keep_vs_remove,
      data = data
    )
  )

  req(length(ll.var) > 0)

  # Apply filter
  tmp <- DaparToolshed::filterFeaturesOneSE(object = data,
                                            i = length(data),
                                            name = paste0("variableFiltered",
                                                          MagellanNTK::Timestamp()),
                                            filters = ll.var)
  
  # Filtering information
  nBefore <- nrow(tmp[[length(tmp) - 1]])
  nAfter  <- nrow(tmp[[length(tmp)]])
  
  nbDeleted <- nBefore - nAfter
  nbRemaining <- nrow(SummarizedExperiment::assay(tmp[[length(tmp)]]))

  # Keep only the last filtered SE
  len_diff <- length(tmp) - original_length

  req(len_diff > 0)
  
  if (len_diff == 2) {
    dataOut <- QFeatures::removeAssay(tmp, length(tmp) - 1)
  } else {
    dataOut <- tmp
  }
  
  # Rename filtered dataset
  names(dataOut)[length(dataOut)] <- "Variablefiltering"

  # Update history
  history <- Prostar2::Add2History(history, "Filtering", "Variablefiltering", "query", ll.query)
  
  return(list(data = dataOut,
              summary = c(ll.query, nbDeleted, nbRemaining),
              history = history))
}


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
