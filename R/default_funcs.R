#' @title Datasets processing
#'
#' @description
#' This manual page describes methods for manipulating datasets stored in
#' [QFeatures] objects. A `QFeatures` object contains a collection of assays,
#' each represented as a `SummarizedExperiment` object.
#'
#' The following functions are currently available:
#' 
#' \describe{
#'   \item{`addDatasets(object, dataset, name)`}{Adds a new dataset (assay) to a 
#'   `QFeatures` object and assigns it a user-defined name.}
#'   \item{`keepDatasets(object, range)`}{Keeps only selected datasets (assays) 
#'   from a `QFeatures` object and removes all others.}
#' }
#' 
#' @param object An object of class `QFeatures`.
#' @param dataset A `SummarizedExperiment` object containing the dataset to be 
#'                added.
#' @param name A `character(1)` specifying the name of the new assay.
#' @param range A `numeric` vector containing the indices of assays to retain.
#' 
#' @return A processed `QFeatures` object.
#'
#' @examples
#' NULL
#'
#' @aliases keepDatasets keepDatasets,list-method
#' @aliases addDatasets addDatasets,list-method
#'
#' @name dataset-processing
#'
#' @importFrom QFeatures addAssay removeAssay
#' @importFrom S4Vectors setdiff
#'
NULL


#' @rdname dataset-processing
#'
#' @export
#' 
addDatasets <- function(object, dataset, name) {
  req(inherits(object, "QFeatures"))
  req(inherits(dataset, "SummarizedExperiment"))

  object <- QFeatures::addAssay(object, dataset, name)

  object
}


#' @rdname dataset-processing
#'
#' @export
#'
keepDatasets <- function(object, range = seq(length(object))) {
  if (missing(object)) {
    stop("Provide object to be processed")
  }
  if (is.null(object)) {
    warning("object is NULL")
    return(NULL)
  }
  if(!inherits(object, "QFeatures")){
    stop("Provide object of class QFeatures")
  }
  if (missing(range)) {
    stop("Provide range of array to be processed")
  }
  if (!is.numeric(range)) {
    stop("Provide numeric range of array to be processed")
  }
  if (min(range) < 1 || max(range) > length(object)) {
    stop("Provide numeric range with values consistent with the number of assays")
  }

  toRemove <- S4Vectors::setdiff(seq(length(object)), range)
  if (length(toRemove) > 0) {
    object <- QFeatures::removeAssay(object, toRemove)
  }

  return(object)
}
