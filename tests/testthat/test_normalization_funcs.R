library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

# try() prints errors to stderr: keep the test output clean
quiet_try_norm <- function(env = parent.frame()) {
  withr::local_options(show.error.messages = FALSE, .local_envir = env)
}

# QFeatures with one assay (5 features x 4 samples) and 2 conditions
make_qf_norm <- function() {
  mat <- matrix(
    seq_len(20) + 0.5, nrow = 5,
    dimnames = list(paste0("f", 1:5), paste0("S", 1:4))
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
mock_history_norm <- function(cap, env = parent.frame()) {
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

# Mocks the DaparToolshed normalisation functions. Each one records its
# arguments in 'cap' and returns the string "normalized".
mock_norm_fun <- function(cap, env = parent.frame()) {
  testthat::local_mocked_bindings(
    GlobalQuantileAlignment = function(...) {
      cap$gqa <- list(...)
      "normalized"
    },
    QuantileCentering = function(...) {
      cap$qc <- list(...)
      "normalized"
    },
    MeanCentering = function(...) {
      cap$mc <- list(...)
      "normalized"
    },
    SumByColumns = function(...) {
      cap$sbc <- list(...)
      "normalized"
    },
    LOESS = function(...) {
      cap$loess <- list(...)
      "normalized"
    },
    vsn = function(...) {
      cap$vsn <- list(...)
      "normalized"
    },
    .package = "DaparToolshed",
    .env = env
  )
}

recorded_params <- function(cap) {
  vapply(cap$calls, function(x) x[[3]], character(1))
}

recorded_value <- function(cap, param) {
  cap$calls[[which(recorded_params(cap) == param)]][[4]]
}

# Sets up both mocks and returns the capture environment
setup_norm <- function(env = parent.frame()) {
  cap <- new.env()
  mock_norm_fun(cap, env = env)
  mock_history_norm(cap, env = env)
  cap
}

# Mocks normalizationProt() in Prostar2 and records its arguments
mock_prot_wrapper <- function(cap, value = list(data = "d", history = "h"),
                              env = parent.frame()) {
  testthat::local_mocked_bindings(
    normalizationProt = function(...) {
      cap$args <- list(...)
      value
    },
    .package = "Prostar2",
    .env = env
  )
}

# ---- Tests -----------------------------------------------------------------

## ----- normalizationProt -----
test_that("normalizationProt returns a list with data and history", {
  cap <- setup_norm()
  
  res <- normalizationProt(make_qf_norm(), method = "GlobalQuantileAlignment")
  
  expect_type(res, "list")
  expect_named(res, c("data", "history"))
})

test_that("normalizationProt initialises the history when it is NULL", {
  cap <- setup_norm()
  
  normalizationProt(make_qf_norm(), method = "GlobalQuantileAlignment",
                    history = NULL)
  
  expect_true(cap$init)
  expect_identical(cap$first[[1]], list())
})

test_that("normalizationProt keeps an existing history", {
  cap <- setup_norm()
  init <- list(previous = "step")
  
  res <- normalizationProt(make_qf_norm(), method = "GlobalQuantileAlignment",
                           history = init)
  
  expect_null(cap$init)
  expect_identical(cap$first[[1]], init)
  expect_identical(res$history[["previous"]], "step")
  expect_length(res$history, 2)   # previous step + 1 new entry
})

test_that("normalizationProt targets the 'Normalization' step in every entry", {
  cap <- setup_norm()
  
  normalizationProt(make_qf_norm(), method = "MeanCentering", type = "overall",
                    scaling = TRUE, subset.norm = 1:3)
  
  for (cl in cap$calls) {
    expect_identical(cl[[1]], "Normalization")
    expect_identical(cl[[2]], "Normalization")
  }
})

test_that("normalizationProt 'G_noneStr' returns the last assay unchanged", {
  cap <- setup_norm()
  qf <- make_qf_norm()
  
  res <- normalizationProt(qf, method = "G_noneStr")
  
  expect_identical(res$data, qf[[length(qf)]])
  expect_s4_class(res$data, "SummarizedExperiment")
})

test_that("normalizationProt 'G_noneStr' calls no normalisation and records nothing", {
  cap <- setup_norm()
  
  res <- normalizationProt(make_qf_norm(), method = "G_noneStr")
  
  for (nm in c("gqa", "qc", "mc", "sbc", "loess", "vsn")) {
    expect_null(cap[[nm]])
  }
  expect_null(cap$calls)
  expect_length(res$history, 0)
})

test_that("normalizationProt 'GlobalQuantileAlignment' receives the last assay matrix", {
  cap <- setup_norm()
  qf <- make_qf_norm()
  
  res <- normalizationProt(qf, method = "GlobalQuantileAlignment")
  
  expect_identical(res$data, "normalized")
  expect_length(cap$gqa, 1)
  expect_equal(cap$gqa[[1]], SummarizedExperiment::assay(qf, length(qf)))
})

test_that("normalizationProt 'GlobalQuantileAlignment' records only the method", {
  cap <- setup_norm()
  
  res <- normalizationProt(make_qf_norm(), method = "GlobalQuantileAlignment")
  
  expect_identical(recorded_params(cap), "method")
  expect_identical(recorded_value(cap, "method"), "GlobalQuantileAlignment")
  expect_length(res$history, 1)
})

test_that("normalizationProt uses the last assay when there are several", {
  cap <- setup_norm()
  qf <- make_qf_norm()
  se2 <- qf[["prot"]]
  SummarizedExperiment::assay(se2) <- SummarizedExperiment::assay(se2) * 100
  qf <- QFeatures::addAssay(qf, se2, name = "prot2")
  
  normalizationProt(qf, method = "GlobalQuantileAlignment")
  
  expect_equal(cap$gqa[[1]], SummarizedExperiment::assay(qf, 2))
})

# ---- normalizationProt: QuantileCentering ----------------------------------

test_that("normalizationProt 'QuantileCentering' passes its arguments", {
  cap <- setup_norm()
  qf <- make_qf_norm()
  
  res <- normalizationProt(qf, method = "QuantileCentering",
                           quantile = "0.15", type = "overall",
                           subset.norm = 1:3)
  
  expect_identical(res$data, "normalized")
  expect_equal(cap$qc$qData, SummarizedExperiment::assay(qf, length(qf)))
  expect_identical(cap$qc$conds, c("A", "A", "B", "B"))
  expect_identical(cap$qc$type, "overall")
  expect_identical(cap$qc$subset.norm, 1:3)
  # quantile is converted to numeric
  expect_identical(cap$qc$quantile, 0.15)
})

test_that("normalizationProt 'QuantileCentering' uses NA when quantile is NULL", {
  cap <- setup_norm()
  
  normalizationProt(make_qf_norm(), method = "QuantileCentering",
                    type = "overall")
  
  expect_true(is.na(cap$qc$quantile))
  expect_true(is.na(recorded_value(cap, "quantile")))
})

test_that("normalizationProt 'QuantileCentering' records 4 entries", {
  cap <- setup_norm()
  
  res <- normalizationProt(make_qf_norm(), method = "QuantileCentering",
                           quantile = 0.15, type = "within conditions",
                           subset.norm = 1:3)
  
  expect_identical(recorded_params(cap),
                   c("method", "quantile", "type", "subset.norm"))
  expect_identical(recorded_value(cap, "method"), "QuantileCentering")
  expect_identical(recorded_value(cap, "quantile"), 0.15)
  expect_identical(recorded_value(cap, "type"), "within conditions")
  expect_identical(recorded_value(cap, "subset.norm"), 1:3)
  expect_length(res$history, 4)
})

test_that("normalizationProt 'MeanCentering' passes its arguments", {
  cap <- setup_norm()
  qf <- make_qf_norm()
  
  res <- normalizationProt(qf, method = "MeanCentering", type = "overall",
                           scaling = TRUE, subset.norm = 2:4)
  
  expect_identical(res$data, "normalized")
  expect_equal(cap$mc$qData, SummarizedExperiment::assay(qf, length(qf)))
  expect_identical(cap$mc$conds, c("A", "A", "B", "B"))
  expect_identical(cap$mc$type, "overall")
  expect_true(cap$mc$scaling)
  expect_identical(cap$mc$subset.norm, 2:4)
})

test_that("normalizationProt 'MeanCentering' records scaling as 'varReduction'", {
  cap <- setup_norm()
  
  res <- normalizationProt(make_qf_norm(), method = "MeanCentering",
                           type = "overall", scaling = TRUE, subset.norm = 2:4)
  
  expect_identical(recorded_params(cap),
                   c("method", "varReduction", "type", "subset.norm"))
  expect_identical(recorded_value(cap, "method"), "MeanCentering")
  expect_true(recorded_value(cap, "varReduction"))
  expect_identical(recorded_value(cap, "type"), "overall")
  expect_identical(recorded_value(cap, "subset.norm"), 2:4)
  expect_length(res$history, 4)
})

test_that("normalizationProt 'SumByColumns' passes its arguments", {
  cap <- setup_norm()
  qf <- make_qf_norm()
  
  res <- normalizationProt(qf, method = "SumByColumns", type = "overall",
                           subset.norm = 1:2)
  
  expect_identical(res$data, "normalized")
  expect_equal(cap$sbc$qData, SummarizedExperiment::assay(qf, length(qf)))
  expect_identical(cap$sbc$conds, c("A", "A", "B", "B"))
  expect_identical(cap$sbc$type, "overall")
  expect_identical(cap$sbc$subset.norm, 1:2)
})

test_that("normalizationProt 'SumByColumns' records 3 entries", {
  cap <- setup_norm()
  
  res <- normalizationProt(make_qf_norm(), method = "SumByColumns",
                           type = "overall", subset.norm = 1:2)
  
  expect_identical(recorded_params(cap), c("method", "type", "subset.norm"))
  expect_identical(recorded_value(cap, "method"), "SumByColumns")
  expect_length(res$history, 3)
})

test_that("normalizationProt 'LOESS' passes its arguments and converts span", {
  cap <- setup_norm()
  qf <- make_qf_norm()
  
  res <- normalizationProt(qf, method = "LOESS", type = "overall",
                           span = "0.7")
  
  expect_identical(res$data, "normalized")
  expect_equal(cap$loess$qData, SummarizedExperiment::assay(qf, length(qf)))
  expect_identical(cap$loess$conds, c("A", "A", "B", "B"))
  expect_identical(cap$loess$type, "overall")
  expect_identical(cap$loess$span, 0.7)
})

test_that("normalizationProt 'LOESS' records the numeric span as 'spanLOESS'", {
  cap <- setup_norm()
  
  res <- normalizationProt(make_qf_norm(), method = "LOESS",
                           type = "overall", span = "0.7")
  
  expect_identical(recorded_params(cap), c("method", "type", "spanLOESS"))
  expect_identical(recorded_value(cap, "method"), "LOESS")
  expect_identical(recorded_value(cap, "spanLOESS"), 0.7)
  expect_length(res$history, 3)
})

test_that("normalizationProt 'vsn' passes its arguments", {
  cap <- setup_norm()
  qf <- make_qf_norm()
  
  res <- normalizationProt(qf, method = "vsn", type = "within conditions")
  
  expect_identical(res$data, "normalized")
  expect_equal(cap$vsn$qData, SummarizedExperiment::assay(qf, length(qf)))
  expect_identical(cap$vsn$conds, c("A", "A", "B", "B"))
  expect_identical(cap$vsn$type, "within conditions")
})

test_that("normalizationProt 'vsn' records 2 entries", {
  cap <- setup_norm()
  
  res <- normalizationProt(make_qf_norm(), method = "vsn", type = "overall")
  
  expect_identical(recorded_params(cap), c("method", "type"))
  expect_identical(recorded_value(cap, "method"), "vsn")
  expect_identical(recorded_value(cap, "type"), "overall")
  expect_length(res$history, 2)
})

test_that("normalizationProt only calls the function of the chosen method", {
  methods <- list(
    GlobalQuantileAlignment = "gqa",
    QuantileCentering       = "qc",
    MeanCentering           = "mc",
    SumByColumns            = "sbc",
    LOESS                   = "loess",
    vsn                     = "vsn"
  )
  
  for (m in names(methods)) {
    cap <- setup_norm()
    normalizationProt(make_qf_norm(), method = m, type = "overall",
                      span = 0.5, scaling = FALSE)
    
    called <- Filter(function(nm) !is.null(cap[[nm]]), unlist(methods))
    expect_identical(unname(unlist(called)), methods[[m]], info = m)
  }
})

test_that("normalizationProt errors when the method is unknown", {
  quiet_try_norm()
  cap <- setup_norm()
  
  # switch() returns NULL, so `.tmp` is never created
  expect_error(normalizationProt(make_qf_norm(), method = "unknown"))
  expect_null(cap$calls)
})

test_that("normalizationProt errors when the normalisation fails", {
  quiet_try_norm()
  cap <- new.env()
  mock_history_norm(cap)
  testthat::local_mocked_bindings(
    GlobalQuantileAlignment = function(...) stop("normalisation failed"),
    .package = "DaparToolshed"
  )
  
  # try() swallows the failure, then `.tmp` does not exist
  expect_error(
    normalizationProt(make_qf_norm(), method = "GlobalQuantileAlignment")
  )
  expect_null(cap$calls)
})

test_that("normalizationProt errors when colData has no 'Condition' column", {
  quiet_try_norm()
  cap <- setup_norm()
  qf <- make_qf_norm()
  SummarizedExperiment::colData(qf)$Condition <- NULL
  
  expect_error(normalizationProt(qf, method = "GlobalQuantileAlignment"))
  expect_null(cap$gqa)
})

test_that("normalizationProt does not modify its input", {
  cap <- setup_norm()
  qf <- make_qf_norm()
  before <- SummarizedExperiment::assay(qf, 1)
  
  normalizationProt(qf, method = "G_noneStr")
  
  expect_equal(SummarizedExperiment::assay(qf, 1), before)
  expect_length(qf, 1)
})





## ----- normalizationPept -----
test_that("normalizationPept forwards every argument to normalizationProt", {
  cap <- new.env()
  mock_prot_wrapper(cap)
  qf <- make_qf_norm()
  init <- list(previous = "step")
  
  normalizationPept(qf, method = "MeanCentering", history = init,
                    quantile = 0.1, type = "overall", scaling = TRUE,
                    subset.norm = 1:3, span = 0.4)
  
  expect_identical(cap$args$data, qf)
  expect_identical(cap$args$method, "MeanCentering")
  expect_identical(cap$args$history, init)
  expect_identical(cap$args$quantile, 0.1)
  expect_identical(cap$args$type, "overall")
  expect_true(cap$args$scaling)
  expect_identical(cap$args$subset.norm, 1:3)
  expect_identical(cap$args$span, 0.4)
})

test_that("normalizationPept forwards the default values", {
  cap <- new.env()
  mock_prot_wrapper(cap)
  
  normalizationPept(make_qf_norm(), method = "G_noneStr")
  
  expect_null(cap$args$history)
  expect_null(cap$args$quantile)
  expect_null(cap$args$type)
  expect_null(cap$args$scaling)
  expect_null(cap$args$subset.norm)
  expect_null(cap$args$span)
})

test_that("normalizationPept returns exactly what normalizationProt returns", {
  cap <- new.env()
  value <- list(data = "some data", history = "some history")
  mock_prot_wrapper(cap, value)
  
  expect_identical(normalizationPept(make_qf_norm(), method = "vsn"), value)
})

test_that("normalizationPept gives the same result as normalizationProt", {
  cap <- setup_norm()
  qf <- make_qf_norm()
  
  res_pept <- normalizationPept(qf, method = "LOESS", type = "overall",
                                span = 0.6)
  cap_prot <- setup_norm()
  res_prot <- normalizationProt(qf, method = "LOESS", type = "overall",
                                span = 0.6)
  
  expect_identical(res_pept, res_prot)
})

test_that("normalizationPept normalises with the real code path and records the history", {
  cap <- setup_norm()
  
  res <- normalizationPept(make_qf_norm(), method = "vsn", type = "overall")
  
  expect_named(res, c("data", "history"))
  expect_identical(res$data, "normalized")
  expect_identical(recorded_params(cap), c("method", "type"))
  expect_length(res$history, 2)
})

test_that("normalizationPept errors when the method is unknown", {
  quiet_try_norm()
  setup_norm()
  
  expect_error(normalizationPept(make_qf_norm(), method = "unknown"))
})

