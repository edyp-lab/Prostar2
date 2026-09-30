#' @title Get logFC
#'
#' @description Get logFC from a dataset
#'
#' @param data A `QFeatures` 
#' @param type A `character(1)`, type of contrast to get the logFC of. 
#' Either `"OnevsOne"` or `"OnevsAll"`
#'
#' @return A `data.frame` containing the logFC
#'
#' @examples
#' data(subR25prot, package = "DaparToolshed")
#' getlogFC(subR25prot)
#'
#' @export
#'
getlogFC <- function(data, type = "OnevsOne"){
  qData <- SummarizedExperiment::assay(data, length(data))
  sTab  <- SummarizedExperiment::colData(data)
  groups <- sTab$Condition
  conds <- unique(groups)
  
  if (type == "OnevsOne") {
    logFC_onevsone <- sapply(combn(conds, 2, simplify = FALSE), function(x) {
      rowMeans(qData[, groups == x[1], drop = FALSE]) - rowMeans(qData[, groups == x[2], drop = FALSE])
    })
    
    pairs <- combn(conds, 2, simplify = FALSE)
    colnames(logFC_onevsone) <- vapply(pairs,
      \(x) paste0(x[1], "_vs_", x[2], "_logFC"), character(1) )
    return(logFC_onevsone)
  } else if (type == "OnevsAll") {
    logFC_onevsall <- sapply(conds, function(cond) {
      in_group <- groups == cond
      out_group <- groups != cond
      rowMeans(qData[, in_group, drop = FALSE]) - rowMeans(qData[, out_group, drop = FALSE])
    })
    
    colnames(logFC_onevsall) <- paste0(conds, "_vs_(all-", conds, ")_logFC")
    return(logFC_onevsall)
  }
  return(NULL)
}


#' @title Check conditions for Limma
#'
#' @description Check if limmae can be applied to a dataset
#'
#' @param data A `QFeatures` 
#'
#' @return A `logical(1)` if limma can be applied
#'
#' @examples
#' data(subR25prot, package = "DaparToolshed")
#' checkLimma(subR25prot)
#'
#' @export
#'
checkLimma <- function(data){
  nConds <- length(unique(DaparToolshed::design_qf(data)$Condition))
  design <- SummarizedExperiment::colData(data)
  nLevel <- DaparToolshed::getDesignLevel(design)
  enable <- (nConds <= 26 && nLevel == 1) ||
    (nConds < 10 && (nLevel %in% c(2, 3)))
  return(enable)
}


#' @title Swap conditions logFC
#'
#' @description Swap conditions for logFC plot
#'
#' @param logFC A `data.frame`, contains all logFC
#' @param i A `numeric(1)`, which conditions to swap
#'
#' @return A `list` containing the new contrast name and the logFC
#'
#' @examples
#' NULL
#'
#' @export
#'
swapConditions <- function(logFC, i) {
  current_comp <- colnames(logFC)[i]
  
  # Remove "_logFC"
  comparison <- sub("_logFC$", "", current_comp)
  
  # Split on "_vs_"
  conds <- strsplit(comparison, "_vs_", fixed = TRUE)[[1]]
  
  cond1 <- conds[1]
  cond2 <- conds[2]
  
  new_name <- paste0(cond2, "_vs_", cond1, "_logFC")
  
  return(list(name = new_name,
              values = -logFC[, i]))
}


#' @title Hypothesis test
#'
#' @description Perform hypothesis test for a dataset
#'
#' @param data A `QFeatures`
#' @param method A `character(1)`, hypothesis test type to use. Must choose 
#' between : `"Limma"` and `"ttests"` 
#' @param history The history
#' @param logFC_thr A `numeric(1)`, the logFC threshold value
#' @param design A `character(1)`, the type of contrast. Must choose between : 
#' `"OnevsOne"` and `"OnevsAll"`
#' @param ttest_type A `character(1)`, the type of t-test. Must choose between : 
#' `"Student"` and `"Welch"`
#'
#' @return A `list` containing the hypothesis test result, the history 
#' and additionnal informations
#'
#' @examples
#' NULL
#'
#' @export
#'
hypothesisTestProt <- function(data,
                               method,
                               history = NULL,
                               logFC_thr,
                               design,
                               ttest_type) {

  if (is.null(history)){
    history <- MagellanNTK::InitializeHistory()
  }
  
  AllPairwiseComp <- tryCatch({
      switch(method,
             Limma = {
               DaparToolshed::limmaCompleteTest(
                 qData = SummarizedExperiment::assay(data, length(data)),
                 sTab = SummarizedExperiment::colData(data),
                 comp.type = design
               )
             },
             ttests = {
               DaparToolshed::compute_t_tests(
                 obj = data,
                 i = length(data),
                 contrast = design,
                 type = ttest_type
               )
             }
      )
    },
    warning = function(w) {
      msg <- w
      message <- w$message
      return(NULL)
    },
    error = function(e) {
      message <- e$message
      return(NULL)
    },
    finally = {
      # cleanup-code
    }
  )
  
  history <- Prostar2::Add2History(history, "HypothesisTest", "HypothesisTest", "method", method)
  history <- Prostar2::Add2History(history, "HypothesisTest", "HypothesisTest", "design", design)
  history <- Prostar2::Add2History(history, "HypothesisTest", "HypothesisTest", "thlogFC", as.numeric(logFC_thr))
  if (method == "ttests") {
    history <- Prostar2::Add2History(history, "HypothesisTest", "HypothesisTest", "ttestOptions", ttest_type)
  }
  
  return(list(AllPairwiseComp = AllPairwiseComp, 
              history = history,
              message = message))
}
