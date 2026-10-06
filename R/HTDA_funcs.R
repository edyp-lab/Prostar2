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


#' @title Comparison names
#'
#' @description Get name of the comparisons 
#'
#' @param data A `data.frame` of the tests
#'
#' @return A `character()` containing the name of the comparisons
#'
#' @examples
#' NULL
#'
#' @export
#'
Get_Pairwisecomparison_Names <- function(data){
  .names <- colnames(data)
  .names <- gsub('_logFC', '', .names, fixed = TRUE)
  .names <- gsub('_pval', '', .names, fixed = TRUE)
  
  return(unique(.names))
}


#' @title Calibration methods
#' 
#' @description Get calibration methods
#' 
#' @return A vector of character
#' 
#' @examples 
#' GetCalibMethod()
#' 
#' @export
#' 
GetCalibMethod <- function(){
  calibMethod_Choices <- c(
    "Benjamini-Hochberg",
    "st.boot", "st.spline",
    "langaas", "jiang", "histo",
    "pounds", "abh", "slim",
    "numeric value"
  )
  names(calibMethod_Choices) <- calibMethod_Choices
  
  return(calibMethod_Choices)
}


#' @title Clean p-values
#' 
#' @description Get p-values that were not pushed
#' 
#' @param pval A vector of p-values
#' 
#' @return A vector of p-values
#' 
#' @examples 
#' NULL
#' 
#' @export
#' 
getPValueNonPushed <- function(pval){
  toDelete <- which(pval > 1)
  if (length(toDelete) > 0) { 
    pval <- pval[-toDelete]
  }
  
  return(pval)
}


#' @title Get calibration method
#'
#' @description Get the calibration method
#'
#' @param calibration_method A `character(1)`, the calibration method
#' @param numeric_value A `numeric(1)`, the pi0 value if needed
#'
#' @return A `character(1)` or a `numeric(1)` containing the calibration method
#'
#' @examples
#' get_calibration_method("Benjamini-Hochberg")
#'
#' @export
#'
get_calibration_method <- function(calibration_method, 
                                   numeric_value = NULL){
  if (calibration_method == "Benjamini-Hochberg") {
    calibMethod <- 1
  } else if (calibration_method == "numeric value") {
    calibMethod <- as.numeric(numeric_value)
  } else {
    calibMethod <- calibration_method
  }
  return(calibMethod)
}


#' @title PipelineProtein p-value calibration sub-step
#'
#' @description Do what has to be done at the end of the p-value calibration 
#' sub-step of the PipelineProtein
#'
#' @param history The history
#' @param calibmet A `character(1)`, the calibration method
#' @param pi0 A `numeric(1)`, the pi0
#' @param h1concent The h1 concentration
#' @param unifunder The Uniformity underestimation
#' 
#' @return A list containing the history
#'
#' @examples
#' NULL
#'
#' @export
#'
pvalCalibrationProt <- function(history,
                    calibmet,
                    pi0,
                    h1concent,
                    unifunder){
  history <- Prostar2::Add2History(history, 'DA', 'Pvaluecalibration', 'Calibration method', calibmet)
  
  if (!is.null(pi0))
    history <- Prostar2::Add2History(history, 'DA', 'Pvaluecalibration', 'pi0', pi0)
  
  history <- Prostar2::Add2History(history, 'DA', 'Pvaluecalibration', 'h1.concentration', h1concent)
  history <- Prostar2::Add2History(history, 'DA', 'Pvaluecalibration', 'Uniformity underestimation', unifunder)
  
  if (!is.null(pi0))
    history <- Prostar2::Add2History(history, 'DA', 'Pvaluecalibration', 'Non-DA protein proportion', round(100 * pi0, digits = 2))
  
  if (!is.null(h1concent))
    history <- Prostar2::Add2History(history, 'DA', 'Pvaluecalibration', 'DA protein concentration', round(100 * h1concent, digits = 2))
  
  return(list(history = history))
}


#' @title Warning text FDR
#'
#' @description Warning text in case the number of selected proteins is too low 
#' compared to the FDR
#'
#' @param fdr A `numeric(1)`, the FDR
#' @param n_significant A `numeric(1)`, the number of selected proteins
#' 
#' @return Either `NULL`, or a `character(1)` issuing a warning
#'
#' @examples
#' NULL
#'
#' @export
#'
fdr_warning_text <- function(fdr, 
                             n_significant) {
  expected_false <- fdr * n_significant
  
  if (expected_false < 1) {
    paste0(
      "With such a dataset size (", n_significant,
      " selected discoveries), an FDR of ",
      round(100 * fdr, 2),
      "% should be cautiously interpreted as strictly less than one ",
      "discovery (", round(expected_false, 2),
      ") is expected to be false"
    )
  } else {
    NULL
  }
}


#' @title Count selected proteins
#'
#' @description Count selected proteins
#'
#' @param pval_table A `data.frame` containing the p-values, adjusted p-values 
#' and logFC
#' @param thpval A `numeric(1)`, the p-value threshold
#' @param thlogfc A `numeric(1)`, the logFC threshold
#' 
#' @return A `numeric(1)` containing the number of selected proteins
#'
#' @examples
#' NULL
#'
#' @export
#'
count_selected_prot <- function(pval_table, 
                                 thpval, 
                                 thlogfc) {
  selected_pval <- which(-log10(pval_table$P_Value) >= thpval)
  selected_logfc <- which(abs(pval_table$logFC) >= thlogfc)
  
  if (!is.null(thpval) && !is.null(thlogfc)) {
    sel <- length(intersect(selected_pval, selected_logfc))
  } else if (!is.null(thpval)) {
    sel <- length(selected_pval)
  } else if (!is.null(thlogfc)) {
    sel <- length(selected_logfc)
  }
  
  return(sel)
}


#' @title Count significant proteins
#'
#' @description Count the number of significant proteins
#'
#' @param pval_table A `data.frame` containing the p-values, adjusted p-values 
#' and logFC
#' @param comparison A `character(1)`, the name of the comparison
#' 
#' @return A `numeric(1)` containing the number of significant proteins
#'
#' @examples
#' NULL
#'
#' @export
#'
count_significant <- function(pval_table, 
                              comparison) {
  column <- paste0("isDifferential (", comparison, ")")
  return(sum(pval_table[[column]] == 1))
}


#' @title Compute FDR
#'
#' @description Compute the FDR
#'
#' @param pval_table A `data.frame` containing the p-values, adjusted p-values 
#' and logFC
#' @param thlogfc A `numeric(1)`, the logFC threshold
#' @param thpval A `numeric(1)`, the p-value threshold
#'
#' @return A `data.frame` containing the p-values and important informations
#'
#' @examples
#' NULL
#'
#' @export
#'
compute_fdr <- function(pval_table, 
                        thpval, 
                        thlogfc) {
  
  adj_pval <- pval_table$Adjusted_PValue
  logpval  <- pval_table$Log_PValue
  logfc    <- pval_table$logFC
  
  selected <- which(logpval >= thpval)
  excluded <- which(abs(logfc) < thlogfc)
  
  selected <- setdiff(selected, excluded)
  
  if (length(selected) > 0) {
    fdr <- max(adj_pval[selected], na.rm = TRUE)
  } else {
    fdr <- 1
  }
  
  return(as.numeric(fdr))
}


#' @title Build p-value table
#'
#' @description Build the table containing the p-values and important 
#' informations
#'
#' @param data A `SummarizedExperiment`
#' @param comparison A `character(1)`, the name of the comparison
#' @param thlogfc A `numeric(1)`, the logFC threshold
#' @param thpval A `numeric(1)`, the p-value threshold
#' @param calibration_method A `character(1)`, the calibration method
#' @param tooltip_info A `charcter()`, all selected tooltips
#'
#' @return A `data.frame` containing the p-values and important informations
#'
#' @examples
#' NULL
#'
#' @export
#'
build_pval_table <- function(data, 
                             comparison, 
                             thlogfc, 
                             thpval,
                             calibration_method, 
                             tooltip_info) {
  
  ht <- DaparToolshed::HypothesisTest(data)
  
  logfc <- ht[, paste0(comparison, "_logFC")]
  pval  <- ht[, paste0(comparison, "_pval")]
  
  pval_table <- data.frame(
    id = rownames(SummarizedExperiment::assay(data)),
    logFC = round(logfc, digits = 3),
    P_Value = pval,
    Log_PValue = -log10(pval),
    Adjusted_PValue = NA,
    isDifferential = 0
  )
  
  # Determine significant proteins
  signifItems <- intersect(which(pval_table$Log_PValue >= thpval),
                           which(abs(pval_table$logFC) >= thlogfc)
  )
  pval_table[signifItems,'isDifferential'] <- 1
  
  #push to 1 proteins with logFC under threshold
  pval_pushfc <- pval
  upItems_logfcinf <- which(abs(logfc) < thlogfc)
  upItems_pushedpval <- which(pval > 1)
  upItems_logfcinf <- setdiff(upItems_logfcinf, upItems_pushedpval)
  if (length(upItems_logfcinf) != 0){
    pval_pushfc[upItems_logfcinf] <- 1
  }  
  if (length(upItems_pushedpval) != 0){
    pval_pushfc <- pval_pushfc[-upItems_pushedpval]
  }
  
  # get adjusted p-values
  adjusted_pvalues <- DaparToolshed::diffAnaComputeAdjustedPValues(
    pval_pushfc,
    calibration_method)
  if (length(upItems_pushedpval) != 0){
    pval_table[-upItems_pushedpval, 'Adjusted_PValue'] <- adjusted_pvalues
  } else {
    pval_table[, 'Adjusted_PValue'] <- adjusted_pvalues
  }
  
  pval_table$logFC <- signif(pval_table$logFC, 4)
  pval_table$P_Value <- signif(pval_table$P_Value, 4)
  pval_table$Adjusted_PValue <- signif(pval_table$Adjusted_PValue, 4)
  pval_table$Log_PValue <- signif(pval_table$Log_PValue, 4)
  
  tmp <- as.data.frame(
    SummarizedExperiment::rowData(data)[, tooltip_info]
  )
  
  names(tmp) <- tooltip_info
  
  pval_table <- cbind(pval_table, tmp)
  
  colnames(pval_table)[2:6] <- paste0(
    colnames(pval_table)[2:6],
    " (", comparison, ")"
  )
  
  return(pval_table)
}


#' @title Create downloadable table
#'
#' @description Create downloadable table as workbook
#'
#' @param pval_table A `data.frame` containing the p-values, adjusted p-values 
#' and logFC
#' @param comparison A `character(1)`, the name of the comparison
#' 
#' @return A workbook to download the table
#'
#' @examples
#' NULL
#'
#' @export
#'
build_pairwise_comparison_workbook <- function(pval_table,
                                 comparison){
  DA_Style <- openxlsx::createStyle(fgFill = "#E97D5E")
  hs1 <- openxlsx::createStyle(fgFill = "#DCE6F1",
                               halign = "CENTER",
                               textDecoration = "italic",
                               border = "Bottom")
  
  wb <- openxlsx::createWorkbook() # Create wb in R
  openxlsx::addWorksheet(wb, sheetName = "DA result") # create sheet
  openxlsx::writeData(wb,
                      sheet = 1,
                      as.character(comparison),
                      colNames = TRUE,
                      headerStyle = hs1
  )
  openxlsx::writeData(wb,
                      sheet = 1,
                      startRow = 3,
                      pval_table,
  )
  
  .txt <- paste0("isDifferential (",
                 as.character(comparison),
                 ")")
  
  ll.DA.row <- which(pval_table[, .txt] == 1)
  ll.DA.col <- rep(which(colnames(pval_table) == .txt),
                   length(ll.DA.row) )
  
  openxlsx::addStyle(wb,
                     sheet = 1, 
                     cols = ll.DA.col,
                     rows = 3 + ll.DA.row, 
                     style = DA_Style
  )
  
  return(wb)
}


#' @title PipelineProtein FDR sub-step
#'
#' @description Do what has to be done at the end of the FDR sub-step of the 
#' PipelineProtein
#'
#' @param history The history
#' @param thpval A `numeric(1)`, the pval threshold
#' @param FDR A `numeric(1)`, the FDR
#' @param nbSignif A `numeric(1)`, the number of significant proteins
#' 
#' @return A list containing the history
#'
#' @examples
#' NULL
#'
#' @export
#'
fdrProt <- function(history,
                       thpval,
                       FDR,
                       nbSignif){
  history <- Prostar2::Add2History(history, 'DA', 'FDR', 'th pval', thpval)
  history <- Prostar2::Add2History(history, 'DA', 'FDR', '% FDR', round(100 * FDR, digits = 2))
  history <- Prostar2::Add2History(history, 'DA', 'FDR', 'Nb significant', nbSignif)
  
  return(list(history = history))
}
