#' @title Peptide aggregation
#'
#' @description Perform aggregation for peptides
#'
#' @param data A `QFeatures`
#' @param history The history
#' @param sharePept How shared peptides are handled. Either `Yes_As_Specific`, 
#' `Yes_Iterative_Redistribution`, `Yes_Simple_Redistribution` or `No`
#' @param operator A function used for quantitative feature aggregation. 
#' Available functions are `Sum`, `Mean`, `Median`, `medianPolish` or 
#' `robustSummary`
#' @param considerPept A `character(1)` defining what peptide to consider. 
#' Available values are `allPeptides` and `topN`
#' @param ponderation A `character(1)` defining what to consider to create the 
#' coefficient for redistribution of shared peptides. Available values are 
#' `Global`, `Condition` or `Sample`
#' @param n If `topN`, specifies the number of peptides to use for each protein
#' @param aggCol A `character()` of column names from rowdata to be aggregated
#' @param maxIter A `numeric(1)` setting the maximum number of iteration
#' @param rmEmptyLines A `logical(1)`, whether empty lines should be 
#' automatically removed after aggregation
#'
#' @return A `list` containing the aggregated dataset and the history
#'
#' @examples
#' NULL
#'
#' @export
#'
aggregationPept <- function(data,
                            history = NULL,
                            sharePept,
                            operator,
                            considerPept,
                            ponderation,
                            n,
                            aggCol,
                            maxIter,
                            rmEmptyLines){
  # Input validation
  sharePept <- match.arg(sharePept, c("Yes_As_Specific", "Yes_Iterative_Redistribution",
                                      "Yes_Simple_Redistribution", "No"))
  operator <- match.arg(operator, c("Sum", "Mean", "Median", "medianPolish", "robustSummary"))
  considerPept <- match.arg(considerPept, c("allPeptides", "topN"))
  ponderation <- match.arg(ponderation, c("Global", "Condition", "Sample"))
  
  if (considerPept == "topN" && missing(n)) {
    stop("'n' must be provided when considerPept = 'topN'")
  }

  .tmp <- DaparToolshed::RunAggregation(
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
  
  req(.tmp)
  
  if (rmEmptyLines) {
    # Make it so aggregated protein with 0 as value are transformed in NA
    .tmp <- QFeatures::zeroIsNA(.tmp, length(.tmp))
    # Removes empty lines
    .tmp <- QFeatures::filterNA(.tmp, pNA = 0.99, length(.tmp))
    # Put the 0 back
    SummarizedExperiment::assay(.tmp[[length(.tmp)]])[which(is.na(SummarizedExperiment::assay(.tmp[[length(.tmp)]])))] <- 0
    # This avoid issues with NA and empty lines in the next step
  }
  
  history <- Prostar2::Add2History(history, 'Aggregation', 'Aggregation', 'includeSharedPeptides', sharePept)
  history <- Prostar2::Add2History(history, 'Aggregation', 'Aggregation', 'operator', operator)
  history <- Prostar2::Add2History(history, 'Aggregation', 'Aggregation', 'considerPeptides', considerPept)
  history <- Prostar2::Add2History(history, 'Aggregation', 'Aggregation', 'ponderation', ponderation)
  history <- Prostar2::Add2History(history, 'Aggregation', 'Aggregation', 'topN', n)
  
  return(list(data = .tmp, 
              history = history))
}


#' @title Aggregation stats tables
#'
#' @description Make table for aggregation stats
#'
#' @param data A `QFeatures`
#'
#' @return A `list` containing the 2 `data.frame`
#'
#' @examples
#' data(subR25pept, package = "DaparToolshed")
#' aggregStatTables(subR25pept)
#'
#' @export
#'
aggregStatTables <- function(data){
  if (!inherits(data, "QFeatures")){
    stop("data must be a QFeatures")
  }
  
  res <- DaparToolshed::getProteinsStats(SummarizedExperiment::rowData(data[[length(data)]])[['adjacencyMatrix']])
  
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
