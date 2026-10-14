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
#' @param method A `character(1)`, the chosen method. Either `ANOVA` or `Limma`
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


#' @title PipelinePeptide fold change sub-step
#'
#' @description Do what has to be done at the end of the fold change sub-step
#' of the PipelinePeptide
#'
#' @param data A `QFeatures`
#' @param history The history
#' @param contrastchoice A `character(1)`, either `"Stacked"` or `"Unique"`
#' @param contrastnames A `character()`, names of contrasts
#' @param res_pval_FC A `data.frame`, containing `logFC` and `P_Value` columns
#' @param res_pval_FC_stacked A `data.frame`, with stacked `logFC` and `P_Value`
#' @param thlogFC A `numeric(1)`, threshold for log fold change
#' @param uniquechoice A `character(1)`, contrast name
#'
#' @return A list containing the history and necessary values
#'
#' @examples
#' NULL
#'
#' @export
#'
foldChangePept <- function(data,
                           history,
                           contrastchoice = c("Stacked", "Unique"),
                           contrastnames,
                           res_pval_FC,
                           res_pval_FC_stacked,
                           thlogFC,
                           uniquechoice = NULL) {
  contrastchoice <- match.arg(contrastchoice)

  se <- data[[length(data)]]
  conditions <- DaparToolshed::design_qf(data)$Condition
  id <- DaparToolshed::idcol(se)
  type <- DaparToolshed::typeDataset(se)
  base_cols <- plainRowDataCols(se)

  hist_args <- list(Contrast_Type = contrastchoice)

  if (contrastchoice == "Stacked") {
    res_pval_FC_complete <- res_pval_FC
    res_pval <- res_pval_FC_stacked
    comparison <- "stacked"

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
  } else { # Unique
    stopifnot(!is.null(uniquechoice), length(uniquechoice) == 1)
    pval_col <- paste0(uniquechoice, "_pval")
    fc_col <- paste0(uniquechoice, "_logFC")
    if (!all(c(pval_col, fc_col) %in% c(
      colnames(res_pval_FC$P_Value),
      colnames(res_pval_FC$logFC)
    ))) {
      txt <- paste0("Unknown contrast: ", uniquechoice)
      stop(txt)
    }

    res_pval_FC_complete <- NULL
    contrast_list <- NULL
    comparison <- uniquechoice
    res_pval <- list(
      P_Value = res_pval_FC$P_Value[, pval_col, drop = FALSE],
      logFC   = res_pval_FC$logFC[, fc_col, drop = FALSE]
    )

    names_condition <- strsplit(uniquechoice, "_vs_", fixed = TRUE)[[1]]
    col_cond <- which(conditions %in% names_condition)

    push_pval_data <- se[, col_cond]
    SummarizedExperiment::rowData(push_pval_data)$qMetacell <-
      SummarizedExperiment::rowData(se)$qMetacell[, col_cond, drop = FALSE]

    cond <- conditions[col_cond]
    rowdatacolname <- base_cols
    hist_args$Contrast_Choice <- uniquechoice
  }

  # Common to both branches
  DaparToolshed::idcol(push_pval_data) <- id
  DaparToolshed::typeDataset(push_pval_data) <- type
  DaparToolshed::HypothesisTest(push_pval_data) <-
    as.data.frame(cbind(res_pval$logFC, res_pval$P_Value))

  hist_args$Foldchange_Threshold <- thlogFC
  for (nm in names(hist_args)) {
    history <- Prostar2::Add2History(
      history, "DifferentialAnalysis",
      "Foldchange", nm, hist_args[[nm]]
    )
  }

  SummaryDT <- data.frame(
    query = "-",
    nbPushed = "-",
    nbRemaining = nrow(res_pval$P_Value),
    stringsAsFactors = FALSE
  )

  return(list(
    history = history,
    res_pval_FC_complete = res_pval_FC_complete,
    comparison = comparison,
    contrast_list = contrast_list,
    cond = cond,
    names_condition = names_condition,
    push_pval_data = push_pval_data,
    rowdatacolname = rowdatacolname,
    res_pval_tmp = res_pval$P_Value,
    tooltipInfo = id,
    SummaryDT = SummaryDT
  ))
}
