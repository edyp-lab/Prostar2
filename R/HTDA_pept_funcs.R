#' @title Scenario info
#'
#' @description Provides UI for information about the different scenarios
#'
#' @return The UI for the scenario information
#'
#' @examples
#' scenarioInfos()
#'
#' @export
#'
scenarioInfos <- function() {
  tagList(
    h2("Scenario choices"),
    fluidRow(
      column(4, div(class = "option-card", div(
        class = "option-panel",
        h4("Contrast", class = "option-title"),
        p("Test conditions to compute the contrasts. If 'Stacked' is selected in the Fold-change step, p-values from all contrasts are stacked together so as to make only a single FDR control."),
        p(strong("Steps:")),
        tags$ul(
          tags$li("Choose the fold-change threshold and contrast (either a single contrast or all contrasts stacked)"),
          tags$li("Push p-value"),
          tags$li("Perform p-value calibration"),
          tags$li("Apply FDR control")
        ),
        p(strong("When to use:")),
        tags$ul(
          tags$li(
            "When comparing multiple conditions",
            tags$ul(
              tags$li("If interested in a single contrast = Unique"),
              tags$li("If interested in all contrasts = Stacked")
            )
          )
        )
      ))),
      column(4, div(class = "option-card", div(
        class = "option-panel",
        h4("Cluster", class = "option-title"),
        p("Make a single omnibus test. In parallel, create of profile for each protein and cluster these profiles. This scenario focus more on the global trend of each protein."),
        p(strong("Steps:")),
        tags$ul(
          tags$li("Choose the method to create the protein profile and the clustering method"),
          tags$li("Perform p-value calibration"),
          tags$li("Apply FDR control")
        ),
        p(strong("When to use:")),
        tags$ul(
          tags$li("Conditions corresponds to a time-course"),
          tags$li("...")
        )
      ))),
      column(4, div(class = "option-card", div(
        class = "option-panel",
        h4("Aggregation", class = "option-title"),
        p("Test conditions to compute the contrasts. The resulting p-values are then aggregated into a single set."),
        p(strong("Steps:")),
        tags$ul(
          tags$li("Choose the p-value aggregation method"),
          tags$li("Perform p-value calibration"),
          tags$li("Apply FDR control")
        ),
        p(strong("When to use:")),
        tags$ul(
          tags$li("If there is more than 2 conditions"),
          tags$li("...")
        )
      )))
    ),
    tags$style(HTML(".option-card {
                            padding-right: 20px;
                            margin-bottom: 10px;
                            }

                            .option-panel {
                            background: #ffffff;
                            border: 1px solid #dddddd;
                            border-radius: 6px;
                            padding: 18px;
                            box-shadow: 0 2px 6px rgba(0,0,0,0.08);
                            }

                            .option-title {
                            margin-bottom: 12px;
                            font-weight: 600;
                            text-align: center;
                            }"))
  )
}


#' @title PipelinePeptide scenario sub-step
#'
#' @description Do what has to be done at the end of the scenario sub-step
#' of the PipelinePeptide
#'
#' @param data A `QFeatures`
#' @param scenario A `character(1)`, the chosen scenario. Either `Contrast`,
#' `Cluster` or `Aggregation`
#' @param method A `character(1)`, the chosen method. Either `ttests`, `ANOVA` 
#' or `Limma`
#' @param history The history
#'
#' @return A list containing the history and necessary values
#'
#' @examples
#' NULL
#'
#' @export
#'
scenarioPept <- function(data,
                         scenario,
                         method,
                         history) {
  if (!inherits(data, "QFeatures")) {
    stop("'data' must be of class QFeatures")
  }

  scenario <- match.arg(scenario, c("Contrast", "Cluster", "Aggregation"))
  method <- match.arg(method, c("ANOVA", "Limma"))

  if (is.null(history)) {
    history <- MagellanNTK::InitializeHistory()
  }

  res_pval_FC_stacked <- NULL
  constratnames <- NULL
  clusters <- NULL
  res_pval_FC_complete <- NULL

  # For Contrast
  if (scenario == "Contrast") {
    if (method == "ANOVA") {
      anova.models <- DaparToolshed::applyAnovasOnProteins(data, length(data))
      res_pval_FC <- DaparToolshed::testAnovaModels(anova.models, test = "TukeyNoMTC")
    } else if (method == "Limma") {
      res_pval_FC <- DaparToolshed::limmaCompleteTest(SummarizedExperiment::assay(data[[length(data)]]), DaparToolshed::design_qf(data), comp.type = "OnevsOne")
    }

    res_pval_FC_stacked$logFC <- data.frame(cond1_vs_cond2_logFC = unlist(res_pval_FC$logFC, use.names = FALSE))
    res_pval_FC_stacked$P_Value <- data.frame(cond1_vs_cond2_pval = unlist(res_pval_FC$P_Value, use.names = FALSE))
    constratnames <- sub("_logFC$", "", colnames(res_pval_FC$logFC))

    # For Cluster
  } else if (scenario == "Cluster") {
    if (method == "ANOVA") {
      anova.models <- DaparToolshed::applyAnovasOnProteins(data, length(data))
      res_pval_FC <- DaparToolshed::testAnovaModels(anova.models, test = "Omnibus")
    } else if (method == "Limma") {
      res_pval_FC <- DaparToolshed::limmaCompleteTest(SummarizedExperiment::assay(data[[length(data)]]), DaparToolshed::design_qf(data), comp.type = "anova1way")
    }

    clusters <- rep("-", nrow(data[[length(data)]]))

    # For Aggregation
  } else if (scenario == "Aggregation") {
    if (method == "ANOVA") {
      anova.models <- DaparToolshed::applyAnovasOnProteins(data, length(data))
      res_pval_FC <- DaparToolshed::testAnovaModels(anova.models, test = "TukeyNoMTC")
    } else if (method == "Limma") {
      res_pval_FC <- DaparToolshed::limmaCompleteTest(SummarizedExperiment::assay(data[[length(data)]]), DaparToolshed::design_qf(data), comp.type = "OnevsOne")
    }
  }

  history <- Prostar2::Add2History(history, "DifferentialAnalysis", "Scenario", "Scenario", scenario)
  history <- Prostar2::Add2History(history, "DifferentialAnalysis", "Scenario", "Method", method)

  return(list(
    history = history,
    res_pval_FC = res_pval_FC,
    res_pval_FC_stacked = res_pval_FC_stacked,
    constratnames = constratnames,
    clusters = clusters,
    res_pval_FC_complete = res_pval_FC
  ))
}


#' @title Select columns from rowData
#'
#' @description Select columns from rowData that are not a data.frame or matrix
#'
#' @param se A `SummarizedExperiment`
#'
#' @return A `character()` with the columns names
#'
#' @examples
#' data(subR25pept, package = "DaparToolshed")
#' plainRowDataCols(subR25pept[[length(subR25pept)]])
#'
#' @export
#'
plainRowDataCols <- function(se) {
  rd <- SummarizedExperiment::rowData(se)
  is_complex <- vapply(rd, function(x) {
    is.matrix(x) || is.data.frame(x) || inherits(x, "DataFrame")
  }, logical(1))
  return(names(rd)[!is_complex])
}

#' @title xxx
#'
#' @description xxx
#'
#' @param se A `SummarizedExperiment`
#' @param conditions A `character()`, vector of conditions
#' @param contrastnames A `character()`, names of contrasts
#'
#' @return A list
#'
#' @examples
#' NULL
#'
#' @export
#'
stackContrasts <- function(se, conditions, contrastnames) {
  n_max <- max(table(conditions))
  assay_mat <- SummarizedExperiment::assay(se)
  qmeta <- SummarizedExperiment::rowData(se)$qMetacell

  pad <- function(x, idx) {
    x <- x[, idx, drop = FALSE]
    n_missing <- n_max - ncol(x)
    if (n_missing > 0) x <- cbind(x, matrix(NA, nrow(x), n_missing))
    x
  }

  per_contrast <- lapply(stats::setNames(contrastnames, contrastnames), function(cn) {
    conds <- strsplit(cn, "_vs_", fixed = TRUE)[[1]]
    a <- do.call(cbind, lapply(conds, function(k) pad(assay_mat, which(conditions == k))))
    m <- do.call(cbind, lapply(conds, function(k) pad(qmeta, which(conditions == k))))
    colnames(a) <- paste0("c", rep(1:2, each = n_max), "_", rep(seq_len(n_max), times = 2))
    colnames(m) <- paste0("metacell_", colnames(a))
    list(assay = a, meta = m)
  })

  return(list(
    assay = do.call(rbind, lapply(per_contrast, `[[`, "assay")),
    meta = do.call(rbind, lapply(per_contrast, `[[`, "meta")),
    n_max = n_max
  ))
}


#' @title Make test
#'
#' @description Make statistical test
#'
#' @param data A `QFeatures`
#' @param method A `character(1)`, the chosen method. Either `ttests`, `ANOVA` 
#' or `Limma`
#' @param design A `character(1)`, the type of contrast. Must choose between : 
#' `"OnevsOne"` and `"OnevsAll"`
#' @param ttest_type A `character(1)`, the type of t-test. Must choose between : 
#' `"Student"` and `"Welch"`
#' @param testANOVA A `character(1)`, the type of ANOVA test. Must choose 
#' between : `"TukeyNoMTC"` and `"Omnibus"`
#'
#' @return Result of test
#'
#' @examples
#' NULL
#'
#' @export
#'
makeTest <- function(data,
                     method = c("Limma", "ttests", "ANOVA"), 
                     design = c("OnevsOne", "OnevsAll", "anova1way"),
                     ttest_type = c("Student", "Welch"),
                     testANOVA = c("TukeyNoMTC", "Omnibus")){
  if (!inherits(data, "QFeatures")) {
    stop("'data' must be of class QFeatures")
  }
  method <- match.arg(method)
  design <- match.arg(design)
  ttest_type <- match.arg(ttest_type)
  testANOVA <- match.arg(testANOVA)
  
  AllPairwiseComp <- tryCatch({
    switch(method,
           Limma = {
             DaparToolshed::limmaCompleteTest(
               qData = SummarizedExperiment::assay(data[[length(data)]]),
               sTab = DaparToolshed::design_qf(data),
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
           },
           ANOVA = {
             anova.models <- DaparToolshed::applyAnovasOnProteins(data, length(data))
             DaparToolshed::testAnovaModels(anova.models, test = testANOVA)
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
  
  return(AllPairwiseComp)
}


#' @title PipelinePeptide hypothesis test sub-step for Contrast
#'
#' @description Do what has to be done at the end of the hypothesis test 
#' sub-step of the PipelinePeptide for the Contrast scenario
#'
#' @param data A `QFeatures`
#' @param history The history
#' @param method A `character(1)`, the chosen method. Either `ttests`, `ANOVA` 
#' or `Limma`
#' @param ttest_type A `character(1)`, the type of t-test. Must choose between : 
#' `"Student"` and `"Welch"`
#' @param contrastchoice A `character(1)`, either `"Stacked"`, `"Unique_OvO"` 
#' or `"Unique_OvA"`
#' @param thlogFC A `numeric(1)`, threshold for log fold change
#' @param uniquechoice A `character(1)`, contrast name
#' @param swap A `logical()` with the same length as the number of contrasts
#'
#' @return A list containing the history and necessary values
#'
#' @examples
#' NULL
#'
#' @export
#'
hypothesisTestPeptContrast <- function(data,
                                       history,
                                       method = c("Limma", "ttests", "ANOVA"),
                                       ttest_type = c("Student", "Welch"),
                                       contrastchoice = c("Stacked", "Unique_OvO", "Unique_OvA"),
                                       thlogFC,
                                       uniquechoice = NULL,
                                       swap) {
  if (!inherits(data, "QFeatures")) {
    stop("'data' must be of class QFeatures")
  }
  method <- match.arg(method)
  ttest_type <- match.arg(ttest_type)
  contrastchoice <- match.arg(contrastchoice)
  
  
  se <- data[[length(data)]]
  conditions <- DaparToolshed::design_qf(data)$Condition
  id <- DaparToolshed::idcol(se)
  type <- DaparToolshed::typeDataset(se)
  base_cols <- plainRowDataCols(se)
  
  AllPairwiseComp_stacked <- NULL
  contrast_list <- NULL
  
  hist_args <- list("Method" = method)
  if (method == "ttests") {
    hist_args["t-test options"] <- ttest_type
  }
  
  hist_args["Contrast type"] <- contrastchoice
  if (contrastchoice == "Stacked") {
    # Make test
    AllPairwiseCompOvO <- makeTest(data = data, method = method, design = "Unique_OvO", ttest_type = ttest_type, testANOVA = "TukeyNoMTC")
    AllPairwiseCompOvA <- makeTest(data = data, method = method, design = "Unique_OvA", ttest_type = ttest_type, testANOVA = "TukeyNoMTC")
    
    # AllPairwiseComp #w/ tt
    # Voir pour le swap
    ##### 
    # !! C'est pas ok ici faudra voir en fonction de ce qu'on prend pour les stacked genre tous ou juste OvO/OvA
    AllPairwiseComp_stacked$logFC <- data.frame(cond1_vs_cond2_logFC = unlist(AllPairwiseComp$logFC, use.names = FALSE))
    AllPairwiseComp_stacked$P_Value <- data.frame(cond1_vs_cond2_pval = unlist(AllPairwiseComp$P_Value, use.names = FALSE))
    
    # AllPairwiseComp_stacked #quand c'est stack
    comparison <- "stacked"
    res_pval <- AllPairwiseComp_stacked
    
    contrastnames <- sub("_logFC$", "", colnames(AllPairwiseComp$logFC))
    stk <- stackContrasts(se, conditions, contrastnames)
    contrast_list <- rep(contrastnames, each = nrow(se))
    
    push_pval_data <- SummarizedExperiment::SummarizedExperiment(stk$assay)
    rd <- do.call(rbind, replicate(length(contrastnames),
                                   SummarizedExperiment::rowData(se)[, base_cols],
                                   simplify = FALSE
    ))
    SummarizedExperiment::rowData(push_pval_data) <- rd
    SummarizedExperiment::rowData(push_pval_data)$qMetacell <- stk$meta
    SummarizedExperiment::rowData(push_pval_data)$Contrast <- contrast_list
    
    cond <- rep(c("cond1", "cond2"), each = stk$n_max)
    names_condition <- unique(cond)
    rowdatacolname <- c(base_cols, "Contrast")
    
  } else { 
    # Make test
    design <- switch(contrastchoice, 
                     "Unique_OvO" = "OnevsOne", 
                     "Unique_OvA" = "OnevsAll")
    AllPairwiseComp <- makeTest(data = data, method = method, design = design, ttest_type = ttest_type, testANOVA = "TukeyNoMTC")
    
    # Swap contrast
    for (i in which(swap)) {
      # Reverse logFC sign
      AllPairwiseComp$logFC[[i]] <- -AllPairwiseComp$logFC[[i]]
      
      # Reverse comparison name
      old_name <- names(AllPairwiseComp$logFC)[i]
      new_name <- sub("^(.+)_vs_(.+)_logFC$", "\\2_vs_\\1_logFC", old_name)
      names(AllPairwiseComp$logFC)[i] <- new_name
      
      # Rename corresponding p-value column
      old_pval_name <- names(AllPairwiseComp$P_Value)[i]
      new_pval_name <- sub("^(.+)_vs_(.+)_pval$", "\\2_vs_\\1_pval", old_pval_name)
      names(AllPairwiseComp$P_Value)[i] <- new_pval_name
    }
    
    # Select contrast
    stopifnot(!is.null(uniquechoice), length(uniquechoice) == 1)
    pval_col <- paste0(uniquechoice, "_pval")
    fc_col <- paste0(uniquechoice, "_logFC")
    if (!all(c(pval_col, fc_col) %in% c(
      colnames(AllPairwiseComp$P_Value),
      colnames(AllPairwiseComp$logFC)
    ))) {
      txt <- paste0("Unknown contrast: ", uniquechoice)
      stop(txt)
    }
    
    comparison <- uniquechoice
    res_pval <- list(
      logFC   = AllPairwiseComp$logFC[, fc_col, drop = FALSE],
      P_Value = AllPairwiseComp$P_Value[, pval_col, drop = FALSE]
    )
    
    names_condition <- strsplit(uniquechoice, "_vs_", fixed = TRUE)[[1]]
    col_cond <- which(conditions %in% names_condition)
    
    push_pval_data <- se[, col_cond]
    SummarizedExperiment::rowData(push_pval_data)$qMetacell <-
      SummarizedExperiment::rowData(se)$qMetacell[, col_cond, drop = FALSE]
    
    cond <- conditions[col_cond]
    rowdatacolname <- base_cols
    
    hist_args["Contrast choice"] <- uniquechoice
  }
  
  # Common to both branches
  DaparToolshed::idcol(push_pval_data) <- id
  DaparToolshed::typeDataset(push_pval_data) <- type
  DaparToolshed::HypothesisTest(push_pval_data) <- as.data.frame(cbind(res_pval$logFC, res_pval$P_Value))
  
  hist_args["Fold-change threshold"] <- thlogFC
  SummaryDT <- data.frame(
    query = "-",
    nbPushed = "0",
    TotalPushed = "0",
    TotalNonPushed = nrow(AllPairwiseComp$P_Value),
    stringsAsFactors = FALSE
  )
  
  
  for (nm in names(hist_args)) {
    history <- Prostar2::Add2History(
      history, "DifferentialAnalysis",
      "Hypothesistest", nm, hist_args[[nm]]
    )
  }
  
  return(list(history = history,
              AllPairwiseComp = AllPairwiseComp,
              AllPairwiseComp_stacked = AllPairwiseComp_stacked,
              res_pval_tmp = res_pval$P_Value,
              rowdatacolname = rowdatacolname,
              push_pval_data = push_pval_data,
              cond = cond,
              contrast_list = contrast_list,
              comparison = comparison,
              names_condition = names_condition,
              SummaryDT = SummaryDT,
              tooltipInfo = id))
}


#' @title PipelinePeptide hypothesis test sub-step Cluster
#'
#' @description Do what has to be done at the end of the hypothesis test 
#' sub-step of the PipelinePeptide for the Cluster scenario
#'
#' @param data A `QFeatures`
#' @param history The history
#' @param method A `character(1)`, the chosen method. Either `ttests`, `ANOVA` 
#' or `Limma`
#' @param ttest_type A `character(1)`, the type of t-test. Must choose between : 
#' `"Student"` and `"Welch"`
#'
#' @return A list containing the history and necessary values
#'
#' @examples
#' NULL
#'
#' @export
#'
hypothesisTestPeptCluster <- function(data,
                                      history,
                                      method = c("Limma", "ttests", "ANOVA"),
                                      ttest_type = c("Student", "Welch")) {
  if (!inherits(data, "QFeatures")) {
    stop("'data' must be of class QFeatures")
  }
  method <- match.arg(method)
  ttest_type <- match.arg(ttest_type)
  
  hist_args <- list("Method" = method)
  if (method == "ttests") {
    hist_args["t-test options"] <- ttest_type
  }
  
  AllPairwiseComp <- makeTest(data = data, method = method, design = "anova1way", ttest_type = ttest_type, testANOVA = "Omnibus")
  clusters <- rep("-", nrow(data[[length(data)]]))
  
  for (nm in names(hist_args)) {
    history <- Prostar2::Add2History(
      history, "DifferentialAnalysis",
      "Hypothesistest", nm, hist_args[[nm]]
    )
  }
  
  return(list(history = history,
              AllPairwiseComp = AllPairwiseComp,
              clusters = clusters))
}


#' @title PipelinePeptide hypothesis test sub-step Aggregation
#'
#' @description Do what has to be done at the end of the hypothesis test 
#' sub-step of the PipelinePeptide for the Aggregation scenario
#'
#' @param data A `QFeatures`
#' @param history The history
#' @param method A `character(1)`, the chosen method. Either `ttests`, `ANOVA` 
#' or `Limma`
#' @param ttest_type A `character(1)`, the type of t-test. Must choose between : 
#' `"Student"` and `"Welch"`
#'
#' @return A list containing the history and necessary values
#'
#' @examples
#' NULL
#'
#' @export
#'
hypothesisTestPeptAggregation <- function(data,
                                          history,
                                          method = c("Limma", "ttests", "ANOVA"),
                                          ttest_type = c("Student", "Welch")) {
  if (!inherits(data, "QFeatures")) {
    stop("'data' must be of class QFeatures")
  }
  method <- match.arg(method)
  ttest_type <- match.arg(ttest_type)
  
  hist_args <- list("Method" = method)
  if (method == "ttests") {
    hist_args["t-test options"] <- ttest_type
  }
  
  AllPairwiseComp <- makeTest(data = data, method = method, design = "OnevsOne", ttest_type = ttest_type, testANOVA = "TukeyNoMTC")
  
  for (nm in names(hist_args)) {
    history <- Prostar2::Add2History(
      history, "DifferentialAnalysis",
      "Hypothesistest", nm, hist_args[[nm]]
    )
  }
  
  return(list(history = history,
              AllPairwiseComp = AllPairwiseComp))
}
