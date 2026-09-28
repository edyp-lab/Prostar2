#' @title Protein normalization
#'
#' @description Perform normalization for proteins
#'
#' @param data A `QFeatures`
#' @param method A `character(1)`, normalization method to be used. Must choose 
#' between : `GlobalQuantileAlignment`, `SumByColumns`, `QuantileCentering`, 
#' `MeanCentering`, `LOESS` and `vsn` 
#' @param history The history
#' @param quantile A `numeric(1)`, quantile of the intensity distribution that 
#' is used as reference. Must be a float between 0 and 1
#' @param type A `character(1)`, indicates whether the method is applied to the 
#' entire dataset at once (`overall`) or whether each condition is normalized 
#' independently (`within conditions`)
#' @param scaling A `logical(1)`, whether variance reduction is performed
#' @param subset.norm Selection of the proteins to which normalization will 
#' be applied
#' @param span A `numeric(1)`, proportion of the other analytes considered to 
#' perform the regression. Must be a float between 0 and 1
#'
#' @return A `list` containing the normalized dataset and the history
#'
#' @examples
#' data(subR25prot, package = "DaparToolshed")
#' normalizationProt(subR25prot,
#'                   method = "GlobalQuantileAlignment")
#'
#' @export
#'
normalizationProt <- function(data,
                              method,
                              history = NULL,
                              quantile = NULL,
                              type = NULL,
                              scaling = NULL,
                              subset.norm = NULL,
                              span = NULL) {
  if (is.null(history)){
    history <- MagellanNTK::InitializeHistory()
  }
  
  try({
    .conds <- SummarizedExperiment::colData(data)[, "Condition"]
    qdata <- SummarizedExperiment::assay(data, length(data))

    switch(method,
      G_noneStr = {
        .tmp <- data[[length(data)]]
      },
      GlobalQuantileAlignment = {
        .tmp <- DaparToolshed::GlobalQuantileAlignment(qdata)
        history <- Prostar2::Add2History(history, "Normalization", "Normalization", "method", method)
      },
      QuantileCentering = {
        quant <- NA
        if (!is.null(quantile)) {
          quant <- as.numeric(quantile)
        }

        .tmp <- DaparToolshed::QuantileCentering(
          qData = qdata,
          conds = .conds,
          type = type,
          subset.norm = subset.norm,
          quantile = quant
        )

        history <- Prostar2::Add2History(history, "Normalization", "Normalization", "method", method)
        history <- Prostar2::Add2History(history, "Normalization", "Normalization", "quantile", quant)
        history <- Prostar2::Add2History(history, "Normalization", "Normalization", "type", type)
        history <- Prostar2::Add2History(history, "Normalization", "Normalization", "subset.norm", subset.norm)
      },
      MeanCentering = {
        .tmp <- DaparToolshed::MeanCentering(
          qData = qdata,
          conds = .conds,
          type = type,
          scaling = scaling,
          subset.norm = subset.norm
        )

        history <- Prostar2::Add2History(history, "Normalization", "Normalization", "method", method)
        history <- Prostar2::Add2History(history, "Normalization", "Normalization", "varReduction", scaling)
        history <- Prostar2::Add2History(history, "Normalization", "Normalization", "type", type)
        history <- Prostar2::Add2History(history, "Normalization", "Normalization", "subset.norm", subset.norm)
      },
      SumByColumns = {
        .tmp <- DaparToolshed::SumByColumns(
          qData = qdata,
          conds = .conds,
          type = type,
          subset.norm = subset.norm
        )

        history <- Prostar2::Add2History(history, "Normalization", "Normalization", "method", method)
        history <- Prostar2::Add2History(history, "Normalization", "Normalization", "type", type)
        history <- Prostar2::Add2History(history, "Normalization", "Normalization", "subset.norm", subset.norm)
      },
      LOESS = {
        .tmp <- DaparToolshed::LOESS(
          qData = qdata,
          conds = .conds,
          type = type,
          span = as.numeric(span)
        )

        history <- Prostar2::Add2History(history, "Normalization", "Normalization", "method", method)
        history <- Prostar2::Add2History(history, "Normalization", "Normalization", "type", type)
        history <- Prostar2::Add2History(history, "Normalization", "Normalization", "spanLOESS", as.numeric(span))
      },
      vsn = {
        .tmp <- DaparToolshed::vsn(
          qData = qdata,
          conds = .conds,
          type = type
        )

        history <- Prostar2::Add2History(history, "Normalization", "Normalization", "method", method)
        history <- Prostar2::Add2History(history, "Normalization", "Normalization", "type", type)
      }
    )
  })
  
  return(list(data = .tmp, history = history))
}
