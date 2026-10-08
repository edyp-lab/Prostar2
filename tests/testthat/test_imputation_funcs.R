library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

# try() prints errors to stderr: keep the test output clean
quiet_try <- function(env = parent.frame()) {
  withr::local_options(show.error.messages = FALSE, .local_envir = env)
}

# QFeatures with one assay (6 features x 4 samples) containing NAs
make_qf_imp <- function() {
  mat <- matrix(
    c(1, 2, NA, 4,
      5, NA, 7, 8,
      9, 10, 11, NA,
      12, 13, 14, 15,
      NA, 17, 18, 19,
      20, 21, 22, 23),
    nrow = 6, byrow = TRUE,
    dimnames = list(paste0("f", 1:6), paste0("S", 1:4))
  )
  coldata <- S4Vectors::DataFrame(
    Condition = c("A", "A", "B", "B"),
    row.names = paste0("S", 1:4)
  )
  se <- SummarizedExperiment::SummarizedExperiment(assays = list(counts = mat))
  QFeatures::QFeatures(list(prot = se), colData = coldata)
}

# Mocks Prostar2::Add2History() and MagellanNTK::InitializeHistory().
#   cap$calls : arguments of every Add2History() call (without the history)
#   cap$first : list(history) received by the first Add2History() call
#   cap$init  : TRUE if InitializeHistory() was called
mock_history_imp <- function(cap, env = parent.frame()) {
  testthat::local_mocked_bindings(
    Add2History = function(history, ...) {
      args <- list(...)
      if (is.null(cap$first)) cap$first <- list(history)
      cap$calls <- c(cap$calls, list(args))
      c(history, list(args))
    },
    .package = "Prostar2",
    .env = env
  )
  testthat::local_mocked_bindings(
    InitializeHistory = function(...) {
      cap$init <- TRUE
      list()
    },
    .package = "MagellanNTK",
    .env = env
  )
}

recorded_params <- function(cap) {
  vapply(cap$calls, function(x) x[[3]], character(1))
}

recorded_value <- function(cap, param) {
  cap$calls[[which(recorded_params(cap) == param)]][[4]]
}

# Mocks the DaparToolshed wrappers used for proteins. Each one records its
# arguments in 'cap' and returns the string "imputed".
mock_wrappers <- function(cap, env = parent.frame()) {
  testthat::local_mocked_bindings(
    wrapperImputeSLSA = function(...) {
      cap$slsa <- list(...)
      "imputed"
    },
    wrapperImputeDetQuant = function(...) {
      cap$detq <- list(...)
      "imputed"
    },
    wrapperImputeKNN = function(...) {
      cap$knn <- list(...)
      "imputed"
    },
    wrapperImputeFixedValue = function(...) {
      cap$fixed <- list(...)
      "imputed"
    },
    design_qf = function(...) {
      cap$design <- list(...)
      data.frame(Condition = c("A", "A", "B", "B"))
    },
    .package = "DaparToolshed",
    .env = env
  )
}

# 20 samples x 40 peptides. Mean abundance increases with the peptide index
# and the number of NA decreases (from 10 to 1), so every missing-value rate
# is > 0 and the regression can be fitted.
make_pirat_plot_data <- function() {
  n_row <- 20
  n_col <- 40
  mat <- sapply(seq_len(n_col), function(j) {
    x <- j + seq_len(n_row) / n_row
    x[seq_len(ceiling(10 * (n_col + 1 - j) / n_col))] <- NA
    x
  })
  colnames(mat) <- paste0("pept", seq_len(n_col))
  list(peptides_ab = mat)
}

# Matrix with the NA replaced by 0 (what the fake imputers return)
filled_assay <- function(qf) {
  m <- SummarizedExperiment::assay(qf[[length(qf)]])
  m[is.na(m)] <- 0
  m
}

# Mocks the Shiny helpers used by imputationPept(), plus the history.
#   cap$progress : TRUE if incProgress() was called
#   cap$logs     : arguments given to show_log_console() (without the block)
mock_pept <- function(cap, env = parent.frame()) {
  testthat::local_mocked_bindings(
    incProgress = function(...) {
      cap$progress <- c(cap$progress, list(list(...)))
      invisible(NULL)
    },
    show_log_console = function(...) {
      # list(...) forces the block, which creates `Pirat_dataimput`
      args <- list(...)
      cap$logs <- args[names(args) != ""]
      invisible("log")
    },
    .package = "Prostar2",
    .env = env
  )
  mock_history_imp(cap, env = env)
}

# ---- Tests -----------------------------------------------------------------

## ----- GetPOVimputMet/GetMECimputMet -----
test_that("GetPOVimputMet returns the named list of POV methods", {
  res <- GetPOVimputMet()
  
  expect_type(res, "list")
  expect_identical(
    res,
    list("slsa" = "slsa", "Det quantile" = "detQuantile", "KNN" = "KNN")
  )
})

test_that("GetMECimputMet returns the named list of MEC methods", {
  res <- GetMECimputMet()
  
  expect_type(res, "list")
  expect_identical(
    res,
    list("Det quantile" = "detQuantile", "Fixed value" = "fixedValue")
  )
})

test_that("every method of GetPOVimputMet is handled by imputationProtPOV", {
  qf <- make_qf_imp()
  
  for (m in unlist(GetPOVimputMet())) {
    cap <- new.env()
    mock_wrappers(cap)
    mock_history_imp(cap)
    
    res <- imputationProtPOV(qf, method = m, quantile = 2, factor = 1, n = 3)
    
    expect_identical(res$data, "imputed")
  }
})

test_that("every method of GetMECimputMet is handled by imputationProtMEC", {
  qf <- make_qf_imp()
  
  for (m in unlist(GetMECimputMet())) {
    cap <- new.env()
    mock_wrappers(cap)
    mock_history_imp(cap)
    
    res <- imputationProtMEC(qf, method = m, quantile = 2, factor = 1,
                             fixVal = 0)
    
    expect_identical(res$data, "imputed")
  }
})





## ----- imputationProtPOV -----
test_that("imputationProtPOV returns a list with data and history", {
  cap <- new.env(); mock_wrappers(cap); mock_history_imp(cap)
  
  res <- imputationProtPOV(make_qf_imp(), method = "slsa")
  
  expect_type(res, "list")
  expect_named(res, c("data", "history"))
})

test_that("imputationProtPOV 'slsa' calls wrapperImputeSLSA on the last assay", {
  cap <- new.env(); mock_wrappers(cap); mock_history_imp(cap)
  qf <- make_qf_imp()
  
  res <- imputationProtPOV(qf, method = "slsa")
  
  expect_identical(res$data, "imputed")
  expect_identical(cap$slsa$obj, qf[[length(qf)]])
  expect_identical(cap$design[[1]], qf)
  expect_equal(cap$slsa$design, data.frame(Condition = c("A", "A", "B", "B")))
  expect_null(cap$detq)
  expect_null(cap$knn)
})

test_that("imputationProtPOV 'slsa' records only the algorithm", {
  cap <- new.env(); mock_wrappers(cap); mock_history_imp(cap)
  
  res <- imputationProtPOV(make_qf_imp(), method = "slsa")
  
  expect_identical(recorded_params(cap), "algorithm")
  expect_identical(cap$calls[[1]][[1]], "Imputation")
  expect_identical(cap$calls[[1]][[2]], "POVImputation")
  expect_identical(recorded_value(cap, "algorithm"), "slsa")
  expect_length(res$history, 1)
})

test_that("imputationProtPOV 'detQuantile' passes quantile / 100, factor and na.type", {
  cap <- new.env(); mock_wrappers(cap); mock_history_imp(cap)
  qf <- make_qf_imp()
  
  res <- imputationProtPOV(qf, method = "detQuantile", quantile = 2.5,
                           factor = 1.5)
  
  expect_identical(res$data, "imputed")
  expect_identical(cap$detq$obj, qf[[length(qf)]])
  expect_equal(cap$detq$qval, 0.025)
  expect_identical(cap$detq$factor, 1.5)
  expect_identical(cap$detq$na.type, "Missing POV")
  expect_null(cap$slsa)
  expect_null(cap$knn)
})

test_that("imputationProtPOV 'detQuantile' records 4 entries in the history", {
  cap <- new.env(); mock_wrappers(cap); mock_history_imp(cap)
  
  res <- imputationProtPOV(make_qf_imp(), method = "detQuantile",
                           quantile = 2.5, factor = 1.5)
  
  expect_identical(recorded_params(cap),
                   c("algorithm", "quantile", "factor", "na.type"))
  for (cl in cap$calls) {
    expect_identical(cl[[1]], "Imputation")
    expect_identical(cl[[2]], "POVImputation")
  }
  expect_identical(recorded_value(cap, "algorithm"), "detQuantile")
  expect_identical(recorded_value(cap, "quantile"), 2.5)
  expect_identical(recorded_value(cap, "factor"), 1.5)
  expect_identical(recorded_value(cap, "na.type"), "Missing POV")
  expect_length(res$history, 4)
})

test_that("imputationProtPOV 'KNN' uses the conditions as groups and n as K", {
  cap <- new.env(); mock_wrappers(cap); mock_history_imp(cap)
  qf <- make_qf_imp()
  
  res <- imputationProtPOV(qf, method = "KNN", n = 4)
  
  expect_identical(res$data, "imputed")
  expect_identical(cap$knn$obj, qf[[length(qf)]])
  expect_identical(cap$knn$grp, c("A", "A", "B", "B"))
  expect_identical(cap$knn$K, 4)
  expect_null(cap$slsa)
  expect_null(cap$detq)
})

test_that("imputationProtPOV 'KNN' records the algorithm and K", {
  cap <- new.env(); mock_wrappers(cap); mock_history_imp(cap)
  
  res <- imputationProtPOV(make_qf_imp(), method = "KNN", n = 4)
  
  expect_identical(recorded_params(cap), c("algorithm", "K"))
  expect_identical(recorded_value(cap, "algorithm"), "KNN")
  expect_identical(recorded_value(cap, "K"), 4)
  expect_length(res$history, 2)
})

test_that("imputationProtPOV initialises the history when it is NULL", {
  cap <- new.env(); mock_wrappers(cap); mock_history_imp(cap)
  
  imputationProtPOV(make_qf_imp(), method = "slsa", history = NULL)
  
  expect_true(cap$init)
  expect_identical(cap$first[[1]], list())
})

test_that("imputationProtPOV keeps an existing history", {
  cap <- new.env(); mock_wrappers(cap); mock_history_imp(cap)
  init <- list(previous = "step")
  
  res <- imputationProtPOV(make_qf_imp(), method = "slsa", history = init)
  
  expect_null(cap$init)
  expect_identical(cap$first[[1]], init)
  expect_identical(res$history[["previous"]], "step")
  expect_length(res$history, 2)   # previous step + 1 new entry
})

test_that("imputationProtPOV errors when the method is unknown", {
  quiet_try()
  cap <- new.env(); mock_wrappers(cap); mock_history_imp(cap)
  
  # switch() returns NULL, so `.tmp` is never created
  expect_error(imputationProtPOV(make_qf_imp(), method = "unknown"))
  expect_null(cap$slsa)
  expect_null(cap$detq)
  expect_null(cap$knn)
})

test_that("imputationProtPOV errors when the imputation fails", {
  quiet_try()
  cap <- new.env(); mock_history_imp(cap)
  testthat::local_mocked_bindings(
    wrapperImputeSLSA = function(...) stop("imputation failed"),
    design_qf = function(...) data.frame(Condition = c("A", "A", "B", "B")),
    .package = "DaparToolshed"
  )
  
  # try() swallows the failure, then `.tmp` does not exist
  expect_error(imputationProtPOV(make_qf_imp(), method = "slsa"))
  expect_null(cap$calls)
})





## ----- imputationProtMEC -----
test_that("imputationProtMEC returns a list with data and history", {
  cap <- new.env(); mock_wrappers(cap); mock_history_imp(cap)
  
  res <- imputationProtMEC(make_qf_imp(), method = "fixedValue", fixVal = 3)
  
  expect_type(res, "list")
  expect_named(res, c("data", "history"))
})

test_that("imputationProtMEC 'detQuantile' passes quantile / 100, factor and na.type", {
  cap <- new.env(); mock_wrappers(cap); mock_history_imp(cap)
  qf <- make_qf_imp()
  
  res <- imputationProtMEC(qf, method = "detQuantile", quantile = 5,
                           factor = 2)
  
  expect_identical(res$data, "imputed")
  expect_identical(cap$detq$obj, qf[[length(qf)]])
  expect_equal(cap$detq$qval, 0.05)
  expect_identical(cap$detq$factor, 2)
  expect_identical(cap$detq$na.type, "Missing MEC")
  expect_null(cap$fixed)
})

test_that("imputationProtMEC 'detQuantile' records 4 entries in the history", {
  cap <- new.env(); mock_wrappers(cap); mock_history_imp(cap)
  
  res <- imputationProtMEC(make_qf_imp(), method = "detQuantile",
                           quantile = 5, factor = 2)
  
  expect_identical(recorded_params(cap),
                   c("algorithm", "quantile", "factor", "na.type"))
  for (cl in cap$calls) {
    expect_identical(cl[[1]], "Imputation")
    expect_identical(cl[[2]], "MECImputation")
  }
  expect_identical(recorded_value(cap, "algorithm"), "detQuantile")
  expect_identical(recorded_value(cap, "quantile"), 5)
  expect_identical(recorded_value(cap, "factor"), 2)
  expect_identical(recorded_value(cap, "na.type"), "Missing MEC")
  expect_length(res$history, 4)
})

test_that("imputationProtMEC 'fixedValue' calls wrapperImputeFixedValue", {
  cap <- new.env(); mock_wrappers(cap); mock_history_imp(cap)
  qf <- make_qf_imp()
  
  res <- imputationProtMEC(qf, method = "fixedValue", fixVal = 3)
  
  expect_identical(res$data, "imputed")
  expect_identical(cap$fixed$obj, qf[[length(qf)]])
  expect_identical(cap$fixed$fixVal, 3)
  expect_identical(cap$fixed$na.type, "Missing MEC")
  expect_null(cap$detq)
})

test_that("imputationProtMEC 'fixedValue' records 3 entries in the history", {
  cap <- new.env(); mock_wrappers(cap); mock_history_imp(cap)
  
  res <- imputationProtMEC(make_qf_imp(), method = "fixedValue", fixVal = 3)
  
  expect_identical(recorded_params(cap), c("algorithm", "fixVal", "na.type"))
  expect_identical(recorded_value(cap, "algorithm"), "fixedValue")
  expect_identical(recorded_value(cap, "fixVal"), 3)
  expect_identical(recorded_value(cap, "na.type"), "Missing MEC")
  expect_length(res$history, 3)
})

test_that("imputationProtMEC initialises or keeps the history", {
  cap <- new.env(); mock_wrappers(cap); mock_history_imp(cap)
  imputationProtMEC(make_qf_imp(), method = "fixedValue", fixVal = 3)
  expect_true(cap$init)
  
  cap2 <- new.env(); mock_wrappers(cap2); mock_history_imp(cap2)
  init <- list(previous = "step")
  res <- imputationProtMEC(make_qf_imp(), method = "fixedValue", fixVal = 3,
                           history = init)
  
  expect_null(cap2$init)
  expect_identical(cap2$first[[1]], init)
  expect_identical(res$history[["previous"]], "step")
})

test_that("imputationProtMEC errors when the method is unknown", {
  quiet_try()
  cap <- new.env(); mock_wrappers(cap); mock_history_imp(cap)
  
  expect_error(imputationProtMEC(make_qf_imp(), method = "slsa"))
  expect_null(cap$detq)
  expect_null(cap$fixed)
})

test_that("imputationProtMEC errors when the imputation fails", {
  quiet_try()
  cap <- new.env(); mock_history_imp(cap)
  testthat::local_mocked_bindings(
    wrapperImputeFixedValue = function(...) stop("imputation failed"),
    .package = "DaparToolshed"
  )
  
  expect_error(imputationProtMEC(make_qf_imp(), "fixedValue", fixVal = 3))
  expect_null(cap$calls)
})





## ----- missmechPiratPlot -----
test_that("missmechPiratPlot draws the plot without error", {
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())
  
  expect_no_error(missmechPiratPlot(make_pirat_plot_data()))
})

test_that("missmechPiratPlot returns NULL", {
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())
  
  expect_null(missmechPiratPlot(make_pirat_plot_data()))
})

test_that("missmechPiratPlot sets the plot margins", {
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())
  
  missmechPiratPlot(make_pirat_plot_data())
  
  expect_equal(graphics::par("mar"), c(4, 4, 1, 1))
})

test_that("missmechPiratPlot does not modify its input", {
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())
  dat <- make_pirat_plot_data()
  before <- dat
  
  missmechPiratPlot(dat)
  
  expect_identical(dat, before)
})

test_that("missmechPiratPlot errors with fewer than 10 peptides", {
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())
  dat <- make_pirat_plot_data()
  dat$peptides_ab <- dat$peptides_ab[, 1:5]
  
  expect_error(missmechPiratPlot(dat))
})

test_that("missmechPiratPlot errors when peptides_ab is missing", {
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())
  
  expect_error(missmechPiratPlot(list()))
})





## ----- imputationPept -----
test_that("imputationPept returns a list with data and history", {
  cap <- new.env(); mock_pept(cap)
  qf <- make_qf_imp()
  testthat::local_mocked_bindings(
    impSeq = function(...) filled_assay(qf),
    .package = "rrcovNA"
  )
  
  res <- imputationPept(qf, method = "impSeq")
  
  expect_type(res, "list")
  expect_named(res, c("data", "history"))
  expect_s4_class(res$data, "SummarizedExperiment")
})

test_that("imputationPept 'impSeq' imputes the last assay", {
  skip_if_not_installed("rrcovNA")
  cap <- new.env(); mock_pept(cap)
  qf <- make_qf_imp()
  received <- NULL
  testthat::local_mocked_bindings(
    impSeq = function(x, ...) {
      received <<- x
      filled_assay(qf)
    },
    .package = "rrcovNA"
  )
  
  res <- imputationPept(qf, method = "impSeq")
  
  expect_equal(received, SummarizedExperiment::assay(qf[[length(qf)]]))
  expect_equal(SummarizedExperiment::assay(res$data), filled_assay(qf))
  expect_false(anyNA(SummarizedExperiment::assay(res$data)))
})

test_that("imputationPept 'impSeq' keeps the dimnames", {
  skip_if_not_installed("rrcovNA")
  cap <- new.env(); mock_pept(cap)
  qf <- make_qf_imp()
  # The fake imputer returns an unnamed matrix
  testthat::local_mocked_bindings(
    impSeq = function(...) unname(filled_assay(qf)),
    .package = "rrcovNA"
  )
  
  res <- imputationPept(qf, method = "impSeq")
  
  expect_identical(rownames(res$data), paste0("f", 1:6))
  expect_identical(colnames(res$data), paste0("S", 1:4))
})

test_that("imputationPept 'impSeq' reports progress and records the algorithm", {
  skip_if_not_installed("rrcovNA")
  cap <- new.env(); mock_pept(cap)
  qf <- make_qf_imp()
  testthat::local_mocked_bindings(
    impSeq = function(...) filled_assay(qf),
    .package = "rrcovNA"
  )
  
  res <- imputationPept(qf, method = "impSeq")
  
  expect_length(cap$progress, 1)
  expect_equal(cap$progress[[1]][[1]], 0.5)
  expect_identical(recorded_params(cap), "algorithm")
  expect_identical(cap$calls[[1]][[1]], "Imputation")
  expect_identical(cap$calls[[1]][[2]], "Imputation")
  expect_identical(recorded_value(cap, "algorithm"), "impSeq")
  expect_length(res$history, 1)
})

test_that("imputationPept 'impSeq' does not modify the input dataset", {
  skip_if_not_installed("rrcovNA")
  cap <- new.env(); mock_pept(cap)
  qf <- make_qf_imp()
  before <- SummarizedExperiment::assay(qf[[1]])
  testthat::local_mocked_bindings(
    impSeq = function(...) filled_assay(qf),
    .package = "rrcovNA"
  )
  
  imputationPept(qf, method = "impSeq")
  
  expect_equal(SummarizedExperiment::assay(qf[[1]]), before)
  expect_true(anyNA(SummarizedExperiment::assay(qf[[1]])))
})

# test_that("imputationPept 'BPCA' rounds nPcs and imputes the last assay", {
#   skip_if_not_installed("pcaMethods")
#   cap <- new.env(); mock_pept(cap)
#   qf <- make_qf_imp()
#   pca_class <- methods::getClass("pcaRes", where = asNamespace("pcaMethods"))
#   testthat::local_mocked_bindings(
#     pca = function(object, method, nPcs, ...) {
#       cap$bpca <- list(object = object, method = method, nPcs = nPcs)
#       methods::new(pca_class, completeObs = filled_assay(qf))
#     },
#     .package = "pcaMethods"
#   )
#   
#   res <- imputationPept(qf, method = "BPCA", nPcs = 2.6)
#   
#   expect_equal(cap$bpca$object, SummarizedExperiment::assay(qf[[length(qf)]]))
#   expect_identical(cap$bpca$method, "bpca")
#   expect_equal(cap$bpca$nPcs, 3)
#   expect_equal(SummarizedExperiment::assay(res$data), filled_assay(qf))
# })
# 
# test_that("imputationPept 'BPCA' records the algorithm and the raw nPcs", {
#   skip_if_not_installed("pcaMethods")
#   cap <- new.env(); mock_pept(cap)
#   qf <- make_qf_imp()
#   pca_class <- methods::getClass("pcaRes", where = asNamespace("pcaMethods"))
#   testthat::local_mocked_bindings(
#     pca = function(...) methods::new(pca_class, completeObs = filled_assay(qf)),
#     .package = "pcaMethods"
#   )
#   
#   res <- imputationPept(qf, method = "BPCA", nPcs = 2.6)
#   
#   expect_identical(recorded_params(cap), c("algorithm", "nPcs"))
#   expect_identical(recorded_value(cap, "algorithm"), "BPCA")
#   expect_identical(recorded_value(cap, "nPcs"), 2.6)
#   expect_length(cap$progress, 1)
#   expect_length(res$history, 2)
# })
### ------------^^^issue w/ BPCA testing----------------
test_that("imputationPept 'Pirat' calls my_pipeline_llkimpute with its parameters", {
  skip_if_not_installed("Pirat")
  cap <- new.env(); mock_pept(cap)
  qf <- make_qf_imp()
  pirat_input <- list(peptides_ab = "dummy")
  testthat::local_mocked_bindings(
    my_pipeline_llkimpute = function(...) {
      cap$pirat <- list(...)
      list(data.imputed = t(filled_assay(qf)))
    },
    .package = "Pirat"
  )
  
  res <- imputationPept(qf, method = "Pirat", dataPirat = pirat_input,
                        extension = "T", alpha.factor = 2)
  
  expect_identical(cap$pirat[[1]], pirat_input)
  expect_identical(cap$pirat$alpha.factor, 2)
  expect_identical(cap$pirat$extension, "T")
  expect_true(cap$pirat$verbose)
  expect_equal(SummarizedExperiment::assay(res$data), filled_assay(qf))
})

test_that("imputationPept 'Pirat' wraps the call in show_log_console", {
  skip_if_not_installed("Pirat")
  cap <- new.env(); mock_pept(cap)
  qf <- make_qf_imp()
  testthat::local_mocked_bindings(
    my_pipeline_llkimpute = function(...) {
      list(data.imputed = t(filled_assay(qf)))
    },
    .package = "Pirat"
  )
  
  imputationPept(qf, method = "Pirat", dataPirat = list(), extension = "T",
                 alpha.factor = 2)
  
  expect_identical(cap$logs$prefix, "PIRAT")
  expect_identical(cap$logs$id_notif, "notif_message")
  expect_false(cap$logs$console_message)
  expect_equal(cap$progress[[1]][[1]], 0.5)
})

test_that("imputationPept 'Pirat' records algorithm, extension and alpha.factor", {
  skip_if_not_installed("Pirat")
  cap <- new.env(); mock_pept(cap)
  qf <- make_qf_imp()
  testthat::local_mocked_bindings(
    my_pipeline_llkimpute = function(...) {
      list(data.imputed = t(filled_assay(qf)))
    },
    .package = "Pirat"
  )
  
  res <- imputationPept(qf, method = "Pirat", dataPirat = list(),
                        extension = "T", alpha.factor = 2)
  
  expect_identical(recorded_params(cap),
                   c("algorithm", "extension", "alpha.factor"))
  expect_identical(recorded_value(cap, "algorithm"), "Pirat")
  expect_identical(recorded_value(cap, "extension"), "T")
  expect_identical(recorded_value(cap, "alpha.factor"), 2)
  expect_length(res$history, 3)
})

test_that("imputationPept 'Pirat' keeps the data when imputation gives nothing", {
  skip_if_not_installed("Pirat")
  quiet_try()
  cap <- new.env(); mock_pept(cap)
  qf <- make_qf_imp()
  info_msg <- NULL
  testthat::local_mocked_bindings(
    my_pipeline_llkimpute = function(...) list(data.imputed = NULL),
    .package = "Pirat"
  )
  testthat::local_mocked_bindings(
    info = function(text, ...) info_msg <<- text,
    .package = "shinyjs"
  )
  
  res <- imputationPept(qf, method = "Pirat", dataPirat = list(),
                        extension = "T", alpha.factor = 2)
  
  # The user is informed, req() stops silently and try() catches it
  expect_identical(info_msg, "Error when imputing with Pirat")
  expect_identical(res$data, qf[[length(qf)]])
  expect_true(anyNA(SummarizedExperiment::assay(res$data)))
  expect_null(cap$calls)
  expect_length(res$history, 0)
})

test_that("imputationPept returns the original dataset when the method is unknown", {
  cap <- new.env(); mock_pept(cap)
  qf <- make_qf_imp()
  
  res <- imputationPept(qf, method = "unknown")
  
  expect_identical(res$data, qf[[length(qf)]])
  expect_null(cap$calls)
  expect_null(cap$progress)
})

test_that("imputationPept returns the original dataset when the imputation fails", {
  skip_if_not_installed("rrcovNA")
  quiet_try()
  cap <- new.env(); mock_pept(cap)
  qf <- make_qf_imp()
  testthat::local_mocked_bindings(
    impSeq = function(...) stop("imputation failed"),
    .package = "rrcovNA"
  )
  
  res <- imputationPept(qf, method = "impSeq")
  
  # try() swallows the error; the history entry comes after the imputation
  expect_identical(res$data, qf[[length(qf)]])
  expect_null(cap$calls)
})

test_that("imputationPept initialises the history when it is NULL", {
  skip_if_not_installed("rrcovNA")
  cap <- new.env(); mock_pept(cap)
  qf <- make_qf_imp()
  testthat::local_mocked_bindings(
    impSeq = function(...) filled_assay(qf),
    .package = "rrcovNA"
  )
  
  imputationPept(qf, method = "impSeq", history = NULL)
  
  expect_true(cap$init)
  expect_identical(cap$first[[1]], list())
})

test_that("imputationPept keeps an existing history", {
  skip_if_not_installed("rrcovNA")
  cap <- new.env(); mock_pept(cap)
  qf <- make_qf_imp()
  init <- list(previous = "step")
  testthat::local_mocked_bindings(
    impSeq = function(...) filled_assay(qf),
    .package = "rrcovNA"
  )
  
  res <- imputationPept(qf, method = "impSeq", history = init)
  
  expect_null(cap$init)
  expect_identical(cap$first[[1]], init)
  expect_identical(res$history[["previous"]], "step")
  expect_length(res$history, 2)
})
