library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

# Build a small QFeatures with two assays; the last one mimics the
# aggregated (protein) level, with one empty row (all 0) and two filled rows.
make_qf <- function(with_empty_row = TRUE) {
  mat_pept <- matrix(
    c(10, 20, 30, 40,
      11, 21, 31, 41,
      12, 22, 32, 42),
    nrow = 3, byrow = TRUE,
    dimnames = list(paste0("pept", 1:3), paste0("S", 1:4))
  )
  mat_prot <- matrix(
    c(5, 6, 7, 8,
      1, 2, 3, 4,
      0, 0, 0, 0),
    nrow = 3, byrow = TRUE,
    dimnames = list(paste0("prot", 1:3), paste0("S", 1:4))
  )
  if (!with_empty_row) mat_prot <- mat_prot[1:2, , drop = FALSE]
  
  coldata <- S4Vectors::DataFrame(
    Condition = c("A", "A", "B", "B"),
    row.names = paste0("S", 1:4)
  )
  
  se_pept <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = mat_pept)
  )
  se_prot <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = mat_prot)
  )
  
  QFeatures::QFeatures(
    list(peptides = se_pept, proteins = se_prot),
    colData = coldata
  )
}

# Default arguments for aggregationPept()
default_args <- function(qf, ...) {
  args <- list(
    data         = qf,
    history      = NULL,
    sharePept    = "Yes_As_Specific",
    operator     = "Sum",
    considerPept = "allPeptides",
    ponderation  = "Global",
    n            = 3,
    aggCol       = c("Protein_name"),
    maxIter      = 500,
    rmEmptyLines = FALSE
  )
  modifyList(args, list(...))
}

# Build a QFeatures whose last assay carries an 'adjacencyMatrix' in its rowData
make_qf_adj <- function() {
  adj <- matrix(
    c(1, 0, 0,
      1, 0, 0,
      0, 1, 1,
      0, 0, 1),
    nrow = 4, byrow = TRUE,
    dimnames = list(paste0("pept", 1:4), paste0("prot", 1:3))
  )
  
  mat <- matrix(
    seq_len(16), nrow = 4,
    dimnames = list(paste0("pept", 1:4), paste0("S", 1:4))
  )
  coldata <- S4Vectors::DataFrame(
    Condition = c("A", "A", "B", "B"),
    row.names = paste0("S", 1:4)
  )
  
  se_first <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = mat)
  )
  se_last <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = mat)
  )
  SummarizedExperiment::rowData(se_last)[["adjacencyMatrix"]] <- adj
  
  qf <- QFeatures::QFeatures(
    list(first = se_first, last = se_last),
    colData = coldata
  )
  list(qf = qf, adj = adj)
}

# Fake result of DaparToolshed::getProteinsStats()
fake_stats <- function(nbPeptides = 4,
                       nbSpecificPeptides = 3,
                       nbSharedPeptides = 1,
                       nbProt = 3,
                       protOnlyUniquePep = "prot1",
                       protOnlySharedPep = "prot2",
                       protMixPep = "prot3") {
  list(
    nbPeptides = nbPeptides,
    nbSpecificPeptides = nbSpecificPeptides,
    nbSharedPeptides = nbSharedPeptides,
    nbProt = nbProt,
    protOnlyUniquePep = protOnlyUniquePep,
    protOnlySharedPep = protOnlySharedPep,
    protMixPep = protMixPep
  )
}

# ---- Tests -----------------------------------------------------------------

## ----- aggregationPept -----
test_that("aggregationPept returns a list with 'data' and 'history'", {
  qf <- make_qf()
  
  testthat::local_mocked_bindings(
    RunAggregation = function(...) qf,
    .package = "DaparToolshed"
  )
  testthat::local_mocked_bindings(
    Add2History = function(history, ...) history,
    .package = "Prostar2"
  )
  
  res <- do.call(aggregationPept, default_args(qf))
  
  expect_type(res, "list")
  expect_named(res, c("data", "history"))
  expect_s4_class(res$data, "QFeatures")
})

test_that("aggregationPept forwards its arguments to RunAggregation", {
  qf <- make_qf()
  captured <- NULL
  
  testthat::local_mocked_bindings(
    RunAggregation = function(...) {
      captured <<- list(...)
      qf
    },
    .package = "DaparToolshed"
  )
  testthat::local_mocked_bindings(
    Add2History = function(history, ...) history,
    .package = "Prostar2"
  )
  
  args <- default_args(
    qf,
    sharePept    = "Yes_Iterative_Redistribution",
    operator     = "Mean",
    considerPept = "topN",
    ponderation  = "Condition",
    n            = 5,
    aggCol       = c("Protein_name", "Other_col"),
    maxIter      = 100
  )
  do.call(aggregationPept, args)
  
  expect_identical(captured$qf, qf)
  expect_identical(captured$includeSharedPeptides, "Yes_Iterative_Redistribution")
  expect_identical(captured$operator, "Mean")
  expect_identical(captured$considerPeptides, "topN")
  expect_identical(captured$adjMatrix, "adjacencyMatrix")
  expect_identical(captured$ponderation, "Condition")
  expect_identical(captured$n, 5)
  expect_identical(captured$aggregated_col, c("Protein_name", "Other_col"))
  expect_identical(captured$max_iter, 100)
})

test_that("aggregationPept stops silently when RunAggregation returns NULL", {
  testthat::local_mocked_bindings(
    RunAggregation = function(...) NULL,
    .package = "DaparToolshed"
  )
  testthat::local_mocked_bindings(
    Add2History = function(history, ...) history,
    .package = "Prostar2"
  )
  
  # shiny::req() raises a 'shiny.silent.error' on a NULL value
  expect_error(
    do.call(aggregationPept, default_args(make_qf())),
    class = "shiny.silent.error"
  )
})

test_that("aggregationPept keeps empty lines when rmEmptyLines = FALSE", {
  qf <- make_qf(with_empty_row = TRUE)
  
  testthat::local_mocked_bindings(
    RunAggregation = function(...) qf,
    .package = "DaparToolshed"
  )
  testthat::local_mocked_bindings(
    Add2History = function(history, ...) history,
    .package = "Prostar2"
  )
  
  res <- do.call(aggregationPept, default_args(qf, rmEmptyLines = FALSE))
  last <- length(res$data)
  
  expect_equal(nrow(res$data[[last]]), 3)
  expect_true("prot3" %in% rownames(res$data[[last]]))
})

test_that("aggregationPept removes empty lines when rmEmptyLines = TRUE", {
  qf <- make_qf(with_empty_row = TRUE)
  
  testthat::local_mocked_bindings(
    RunAggregation = function(...) qf,
    .package = "DaparToolshed"
  )
  testthat::local_mocked_bindings(
    Add2History = function(history, ...) history,
    .package = "Prostar2"
  )
  
  res <- do.call(aggregationPept, default_args(qf, rmEmptyLines = TRUE))
  last <- length(res$data)
  mat <- SummarizedExperiment::assay(res$data[[last]])
  
  # The all-zero protein is removed
  expect_equal(nrow(mat), 2)
  expect_false("prot3" %in% rownames(mat))
  # No NA left (zeros put back / no NA introduced)
  expect_false(anyNA(mat))
  # Other lines are untouched
  expect_equal(mat["prot1", ], c(S1 = 5, S2 = 6, S3 = 7, S4 = 8))
  # The peptide-level assay is not modified
  expect_equal(nrow(res$data[["peptides"]]), 3)
})

test_that("aggregationPept does not alter data when no row is empty and rmEmptyLines = TRUE", {
  qf <- make_qf(with_empty_row = FALSE)
  
  testthat::local_mocked_bindings(
    RunAggregation = function(...) qf,
    .package = "DaparToolshed"
  )
  testthat::local_mocked_bindings(
    Add2History = function(history, ...) history,
    .package = "Prostar2"
  )
  
  res <- do.call(aggregationPept, default_args(qf, rmEmptyLines = TRUE))
  last <- length(res$data)
  
  expect_equal(
    SummarizedExperiment::assay(res$data[[last]]),
    SummarizedExperiment::assay(qf[[length(qf)]])
  )
})

test_that("aggregationPept records the 5 parameters in the history", {
  qf <- make_qf()
  calls <- list()
  
  testthat::local_mocked_bindings(
    RunAggregation = function(...) qf,
    .package = "DaparToolshed"
  )
  testthat::local_mocked_bindings(
    Add2History = function(history, ...) {
      calls[[length(calls) + 1]] <<- list(...)
      c(history, list(tail(list(...), 1)[[1]]))
    },
    .package = "Prostar2"
  )
  
  args <- default_args(
    qf,
    sharePept    = "No",
    operator     = "medianPolish",
    considerPept = "topN",
    ponderation  = "Sample",
    n            = 7
  )
  res <- do.call(aggregationPept, args)
  
  expect_length(calls, 5)
  
  # Every call targets the 'Aggregation' step
  for (cl in calls) {
    expect_identical(cl[[1]], "Aggregation")
    expect_identical(cl[[2]], "Aggregation")
  }
  
  # Parameter names, in order
  expect_identical(
    vapply(calls, function(x) x[[3]], character(1)),
    c("includeSharedPeptides", "operator", "considerPeptides",
      "ponderation", "topN")
  )
  
  # Parameter values, in order
  expect_identical(calls[[1]][[4]], "No")
  expect_identical(calls[[2]][[4]], "medianPolish")
  expect_identical(calls[[3]][[4]], "topN")
  expect_identical(calls[[4]][[4]], "Sample")
  expect_identical(calls[[5]][[4]], 7)
  
  # History is updated sequentially
  expect_length(res$history, 5)
})

test_that("aggregationPept passes the existing history through Add2History", {
  qf <- make_qf()
  first_history <- NULL
  init_history <- list(previous = "step")
  
  testthat::local_mocked_bindings(
    RunAggregation = function(...) qf,
    .package = "DaparToolshed"
  )
  testthat::local_mocked_bindings(
    Add2History = function(history, ...) {
      if (is.null(first_history)) first_history <<- history
      history
    },
    .package = "Prostar2"
  )
  
  res <- do.call(aggregationPept, default_args(qf, history = init_history))
  
  expect_identical(first_history, init_history)
  expect_identical(res$history, init_history)
})

test_that("aggregationPept stops when the wrong arguments value are used", {
  qf <- make_qf()
  first_history <- NULL
  
  testthat::local_mocked_bindings(
    RunAggregation = function(...) qf,
    .package = "DaparToolshed"
  )
  testthat::local_mocked_bindings(
    Add2History = function(history, ...) {
      if (is.null(first_history)) first_history <<- history
      history
    },
    .package = "Prostar2"
  )
  
  args <- default_args(qf, sharePept    = "test")
  expect_error(do.call(aggregationPept, args)) 
  
  args <- default_args(qf, operator = "test")
  expect_error(do.call(aggregationPept, args))  
  
  args <- default_args(qf, considerPept = "test")
  expect_error(do.call(aggregationPept, args))
  
  args <- default_args(qf, ponderation  = "test")
  expect_error(do.call(aggregationPept, args))
  
  args <- default_args(qf, considerPept = "topN", n = NULL)
  expect_error(do.call(aggregationPept, args))
})





## ----- aggregStatTables -----
test_that("aggregStatTables returns a list with 'pept' and 'prot' data.frames", {
  obj <- make_qf_adj()
  
  testthat::local_mocked_bindings(
    getProteinsStats = function(...) fake_stats(),
    .package = "DaparToolshed"
  )
  
  res <- aggregStatTables(obj$qf)
  
  expect_type(res, "list")
  expect_named(res, c("pept", "prot"))
  expect_s3_class(res$pept, "data.frame")
  expect_s3_class(res$prot, "data.frame")
  expect_named(res$pept, c("Description", "Count", "Percentage"))
  expect_named(res$prot, c("Description", "Count", "Percentage"))
  expect_equal(nrow(res$pept), 3)
  expect_equal(nrow(res$prot), 4)
})

test_that("aggregStatTables uses the adjacency matrix of the last assay", {
  obj <- make_qf_adj()
  received <- NULL
  
  testthat::local_mocked_bindings(
    getProteinsStats = function(x, ...) {
      received <<- x
      fake_stats()
    },
    .package = "DaparToolshed"
  )
  
  aggregStatTables(obj$qf)
  
  expect_equal(unname(as.matrix(received)), unname(obj$adj))
})

test_that("aggregStatTables builds the peptide table correctly", {
  obj <- make_qf_adj()
  
  testthat::local_mocked_bindings(
    getProteinsStats = function(...) fake_stats(),
    .package = "DaparToolshed"
  )
  
  tab <- aggregStatTables(obj$qf)$pept
  
  expect_identical(
    tab$Description,
    c("Total number of peptides",
      "Number of specific peptides",
      "Number of shared peptides")
  )
  expect_equal(tab$Count, c(4, 3, 1))
  expect_equal(tab$Percentage, c(100, 75, 25))
})

test_that("aggregStatTables builds the protein table correctly", {
  obj <- make_qf_adj()
  
  testthat::local_mocked_bindings(
    getProteinsStats = function(...) {
      fake_stats(
        nbProt = 10,
        protOnlyUniquePep = paste0("p", 1:5),
        protOnlySharedPep = paste0("p", 6:7),
        protMixPep = paste0("p", 8:10)
      )
    },
    .package = "DaparToolshed"
  )
  
  tab <- aggregStatTables(obj$qf)$prot
  
  expect_identical(
    tab$Description,
    c("Total number of proteins",
      "Number of proteins with only specific peptides",
      "Number of proteins with only shared peptides",
      "Number of proteins with both specific and shared peptides")
  )
  # Counts come from the lengths of the protein vectors
  expect_equal(tab$Count, c(10, 5, 2, 3))
  expect_equal(tab$Percentage, c(100, 50, 20, 30))
})

test_that("aggregStatTables rounds percentages to 2 decimals", {
  obj <- make_qf_adj()
  
  testthat::local_mocked_bindings(
    getProteinsStats = function(...) {
      fake_stats(nbPeptides = 3, nbSpecificPeptides = 1, nbSharedPeptides = 2)
    },
    .package = "DaparToolshed"
  )
  
  tab <- aggregStatTables(obj$qf)$pept
  
  expect_equal(tab$Percentage, c(100, 33.33, 66.67))
})

test_that("aggregStatTables always gives 100% for the total rows", {
  obj <- make_qf_adj()
  
  testthat::local_mocked_bindings(
    getProteinsStats = function(...) {
      fake_stats(nbPeptides = 7, nbProt = 9,
                 protOnlyUniquePep = "a", protOnlySharedPep = "b",
                 protMixPep = "c")
    },
    .package = "DaparToolshed"
  )
  
  res <- aggregStatTables(obj$qf)
  
  expect_equal(res$pept$Percentage[1], 100)
  expect_equal(res$prot$Percentage[1], 100)
})

test_that("aggregStatTables handles datasets without shared peptides", {
  obj <- make_qf_adj()
  
  testthat::local_mocked_bindings(
    getProteinsStats = function(...) {
      fake_stats(
        nbPeptides = 4, nbSpecificPeptides = 4, nbSharedPeptides = 0,
        nbProt = 2,
        protOnlyUniquePep = c("prot1", "prot2"),
        protOnlySharedPep = character(0),
        protMixPep = character(0)
      )
    },
    .package = "DaparToolshed"
  )
  
  res <- aggregStatTables(obj$qf)
  
  expect_equal(res$pept$Count, c(4, 4, 0))
  expect_equal(res$pept$Percentage, c(100, 100, 0))
  expect_equal(res$prot$Count, c(2, 2, 0, 0))
  expect_equal(res$prot$Percentage, c(100, 100, 0, 0))
})

test_that("aggregStatTables works with the real getProteinsStats", {
  skip_if_not_installed("DaparToolshed")
  obj <- make_qf_adj()
  
  # pept1, pept2: only prot1 (specific)
  # pept3: prot2 + prot3 (shared)
  # pept4: only prot3 (specific)
  # => prot1 only specific, prot2 only shared, prot3 mixed
  res <- aggregStatTables(obj$qf)
  
  expect_equal(res$pept$Count, c(4, 3, 1))
  expect_equal(res$prot$Count, c(3, 1, 1, 1))
  expect_equal(res$prot$Percentage, c(100, 33.33, 33.33, 33.33))
})

test_that("aggregStatTables stops if data is not a QFeatures", {
  skip_if_not_installed("DaparToolshed")
  
  expect_error(aggregStatTables("test"))
})
