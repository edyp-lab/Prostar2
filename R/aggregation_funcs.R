#' @title Peptide aggregation
#'
#' @description Perform aggregation for peptides
#'
#' @param data A `QFeatures`
#' @param history The history
#' @param sharePept How shared peptides are handled. One of `Yes_As_Specific`, 
#' `Yes_Iterative_Redistribution`, `Yes_Simple_Redistribution` or `No`
#' @param operator Aggregation function: `Sum`, `Mean`, `Median`, 
#' `medianPolish` or `robustSummary`
#' @param considerPept A `character(1)` defining what peptide to consider. 
#' Available values are `allPeptides` and `topN`
#' @param ponderation A `character(1)` defining what to consider to create the 
#' coefficient for redistribution of shared peptides. Available values are 
#' `Global`, `Condition` or `Sample`
#' @param n Number of peptides to consider per protein. 
#' Required if `considerPept = "topN"`
#' @param aggCol A `character()` of rowData column names to aggregate
#' @param maxIter A `numeric(1)`, maximum number of iterations
#' @param rmEmptyLines A `logical(1)`, remove empty lines after aggregation
#'
#' @return A `list` with the aggregated dataset (`data`) and the `history`
#'
#' @examples
#' NULL
#'
#' @export
#'
aggregationPept <- function(data,
                            history = NULL,
                            sharePept = c("Yes_As_Specific",
                                         "Yes_Iterative_Redistribution",
                                         "Yes_Simple_Redistribution",
                                         "No"),
                            operator = c("Sum", "Mean", "Median",
                                         "medianPolish", "robustSummary"),
                            considerPept = c("allPeptides", "topN"),
                            ponderation = c("Global", "Condition", "Sample"),
                            n = NULL,
                            aggCol,
                            maxIter,
                            rmEmptyLines = TRUE){
  # Input validation
  if (!inherits(data, "QFeatures")) {
    stop("'data' must be a QFeatures")
  }
  sharePept    <- match.arg(sharePept)
  operator     <- match.arg(operator)
  considerPept <- match.arg(considerPept)
  ponderation  <- match.arg(ponderation)
  stopifnot(is.logical(rmEmptyLines), length(rmEmptyLines) == 1,
            !is.na(rmEmptyLines))
  
  if (considerPept == "topN") {
    if (is.null(n) || length(n) != 1 || !is.numeric(n) || n < 1) {
      stop("'n' must be a positive number when considerPept = 'topN'")
    }
  } else {
    n <- NULL
  }

  agg <- DaparToolshed::RunAggregation(
    qf = data,
    includeSharedPeptides = sharePept,
    operator = operator,
    considerPeptides = considerPept,
    adjMatrix = 'adjacencyMatrix',
    ponderation = ponderation,
    n = n,
    aggregated_col = aggCol,
    max_iter = maxIter
  )
  
  req(agg)
  
  if (rmEmptyLines) {
    last <- length(agg)
    # Make it so aggregated protein with 0 as value are transformed in NA
    agg <- QFeatures::zeroIsNA(agg, last)
    # Removes empty lines
    agg <- QFeatures::filterNA(agg, pNA = 0.99, last)
    # Put the 0 back
    SummarizedExperiment::assay(agg[[last]])[which(is.na(SummarizedExperiment::assay(agg[[last]])))] <- 0
    # This avoid issues with NA and empty lines in the next step
  }
  
  history <- Prostar2::Add2History(history, 'Aggregation', 'Aggregation', 'includeSharedPeptides', sharePept)
  history <- Prostar2::Add2History(history, 'Aggregation', 'Aggregation', 'operator', operator)
  history <- Prostar2::Add2History(history, 'Aggregation', 'Aggregation', 'considerPeptides', considerPept)
  history <- Prostar2::Add2History(history, 'Aggregation', 'Aggregation', 'ponderation', ponderation)
  history <- Prostar2::Add2History(history, 'Aggregation', 'Aggregation', 'topN', n)
  
  return(list(data = agg, 
              history = history))
}


#' @title Aggregation stats tables
#'
#' @description Make table for aggregation stats
#'
#' @param data A `QFeatures` whose last assay has an `adjacencyMatrix` 
#' in its rowData
#'
#' @return A `list` with two `data.frame`s: `pept` and `prot`
#'
#' @examples
#' data(subR25pept, package = "DaparToolshed")
#' aggregStatTables(subR25pept)
#'
#' @export
#'
aggregStatTables <- function(data){
  if (!inherits(data, "QFeatures")) {
    stop("data must be a QFeatures")
  }
  if (length(data) == 0) {
    stop("data contains no assay")
  }
  
  adj <- SummarizedExperiment::rowData(data[[length(data)]])[["adjacencyMatrix"]]
  if (is.null(adj)) {
    stop("No 'adjacencyMatrix' found in the rowData of the last assay")
  }
  
  res <- DaparToolshed::getProteinsStats(adj)
  
  # Make peptide table
  tablePept_desc <- c(
    "Total number of peptides",
    "Number of specific peptides",
    "Number of shared peptides"
  )
  tablePept_nb <- c(
    res$nbPeptides,
    res$nbSpecificPeptides,
    res$nbSharedPeptides
  )
  tablePept_percent <- round(tablePept_nb*100/tablePept_nb[1], 2)
  
  tablePept <- data.frame("Description" = tablePept_desc, 
                          "Count" = tablePept_nb,
                          "Percentage" = tablePept_percent)
  
  # Make protein table
  tableProt_desc <- c(
    "Total number of proteins",
    "Number of proteins with only specific peptides",
    "Number of proteins with only shared peptides",
    "Number of proteins with both specific and shared peptides"
  )
  tableProt_nb <- c(
    res$nbProt,
    length(res$protOnlyUniquePep),
    length(res$protOnlySharedPep),
    length(res$protMixPep)
  )
  tableProt_percent <- round(tableProt_nb*100/tableProt_nb[1], 2)
  
  tableProt <- data.frame("Description" = tableProt_desc, 
                          "Count" = tableProt_nb,
                          "Percentage" = tableProt_percent)
  
  return(list(pept = tablePept, prot = tableProt))
}
