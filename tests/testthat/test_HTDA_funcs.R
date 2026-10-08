library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

# 2 conditions (A, A, B, B), 3 features
# means A: 2, 2, 15  | means B: 7, 5, 5  -> A - B = -5, -3, 10
mat_2c <- function() {
  matrix(
    c(1, 3, 5, 9,
      2, 2, 4, 6,
      10, 20, 0, 10),
    nrow = 3, byrow = TRUE,
    dimnames = list(paste0("f", 1:3), paste0("S", 1:4))
  )
}

# 3 conditions (A, A, B, B, C, C), 2 features
# f1 means: A = 2, B = 6, C = 10 | f2 means: A = 0, B = 0, C = 6
mat_3c <- function() {
  matrix(
    c(1, 3, 5, 7, 9, 11,
      0, 0, 0, 0, 6, 6),
    nrow = 2, byrow = TRUE,
    dimnames = list(paste0("f", 1:2), paste0("S", 1:6))
  )
}

# QFeatures with 2 assays. The last one holds 'mat', the first one holds
# different values, to check that the last assay is the one used.
make_qf_fc <- function(mat, cond) {
  coldata <- S4Vectors::DataFrame(Condition = cond, row.names = colnames(mat))
  se_last  <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = mat))
  se_first <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = mat * 100))
  QFeatures::QFeatures(list(first = se_first, last = se_last),
                       colData = coldata)
}

# Runs checkLimma() with a given number of conditions and design level
run_checkLimma <- function(n_conds, level, cap = new.env()) {
  qf <- make_qf_fc(mat_2c(), c("A", "A", "B", "B"))
  testthat::with_mocked_bindings(
    checkLimma(qf),
    design_qf = function(...) {
      data.frame(Condition = rep(paste0("C", seq_len(n_conds)), each = 2))
    },
    getDesignLevel = function(design) {
      cap$design <- design
      level
    },
    .package = "DaparToolshed"
  )
}

make_logFC <- function() {
  matrix(
    c(1, 2, 3, 4,
      -1, -2, -3, -4,
      0.5, 0.25, 0, 10),
    nrow = 4,
    dimnames = list(paste0("f", 1:4),
                    c("A_vs_B_logFC", "A_vs_C_logFC", "B_vs_C_logFC"))
  )
}

# Mocks every dependency of hypothesisTestProt() and records what happens in
# 'cap':
#   cap$limma, cap$ttest : arguments received by the test functions
#   cap$calls            : arguments of every Add2History() call (without history)
#   cap$first            : list(history) received by the first Add2History()
#   cap$init             : TRUE if InitializeHistory() was called
mock_htp <- function(cap,
                     limma = function(...) "limma_res",
                     ttest = function(...) "ttest_res",
                     env = parent.frame()) {
  testthat::local_mocked_bindings(
    limmaCompleteTest = function(...) {
      cap$limma <- list(...)
      limma(...)
    },
    compute_t_tests = function(...) {
      cap$ttest <- list(...)
      ttest(...)
    },
    .package = "DaparToolshed",
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
}

# Mocks that fail
warns <- function(...) warning("a warning")
fails <- function(...) stop("an error")

# QFeatures with 2 assays and 6 samples (A, A, B, B, C, C).
# The last assay has 4 features and a 'qMetacell' matrix in its rowData
# (one column per sample), plus a 'Protein' column.
# The first assay holds values 100 x bigger, to check which assay is used.
make_qf_gda <- function() {
  samples <- paste0("S", 1:6)
  coldata <- S4Vectors::DataFrame(
    Condition = rep(c("A", "B", "C"), each = 2),
    row.names = samples
  )
  mat <- matrix(seq_len(24), nrow = 4,
                dimnames = list(paste0("f", 1:4), samples))
  
  se_first <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = mat * 100))
  se_last <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = mat))
  SummarizedExperiment::rowData(se_last)$qMetacell <- matrix(
    seq_len(24) + 1000, nrow = 4, dimnames = list(NULL, samples))
  SummarizedExperiment::rowData(se_last)$Protein <- paste0("P", 1:4)
  
  QFeatures::QFeatures(list(first = se_first, last = se_last),
                       colData = coldata)
}

# Mocks DaparToolshed::design_qf(): returns the colData as a data.frame and
# records the call in 'cap'
mock_design <- function(cap, env = parent.frame()) {
  testthat::local_mocked_bindings(
    design_qf = function(object, ...) {
      cap$called <- TRUE
      cap$data <- object
      as.data.frame(SummarizedExperiment::colData(object))
    },
    .package = "DaparToolshed",
    .env = env
  )
}

# SummarizedExperiment with 'n' rows
make_se_n <- function(n = 5) {
  SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = matrix(0, nrow = n, ncol = 2))
  )
}

# Fake store for the p-values table of DaparToolshed::HypothesisTest().
# The getter reads 'store$ht', the setter writes to it.
mock_hypothesis <- function(store, env = parent.frame()) {
  testthat::local_mocked_bindings(
    HypothesisTest = function(object, ...) store$ht,
    `HypothesisTest<-` = function(object, value) {
      store$ht <- value
      object
    },
    .package = "DaparToolshed",
    .env = env
  )
}

make_ht <- function(n = 5) {
  data.frame(
    A_vs_B_pval = rep(0.01, n),
    A_vs_C_pval = rep(0.02, n)
  )
}

# Initial 'pushed p-values' table: a placeholder row, as the other functions
# expect the first row to be a header-like row
make_dt0 <- function() {
  data.frame(
    comparison = "-", query = "-", nbpushed = "-",
    totalpushed = "-", totalnonpushed = "-",
    stringsAsFactors = FALSE
  )
}

make_dt_upd <- function() {
  data.frame(
    comparison = c("-", "A_vs_B", "A_vs_C", "A_vs_B"),
    query      = c("-", "q1", "q2", "q3"),
    nbpushed   = c("-", "1", "2", "3"),
    stringsAsFactors = FALSE
  )
}

# Mocks Prostar2::Add2History(); records every call (without the history) in
# 'cap$calls' and the history received by the first call in 'cap$first'
mock_history <- function(cap, env = parent.frame()) {
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
}

make_dt_pw <- function(queries) {
  data.frame(
    comparison = rep("A_vs_B", length(queries)),
    query = queries,
    stringsAsFactors = FALSE
  )
}

# Mocks Prostar2::Add2History(); records every call (without the history) in
# 'cap$calls' and the history received by the first call in 'cap$first'
mock_history_cal <- function(cap, env = parent.frame()) {
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
}

# Names of the recorded parameters, in order
recorded_params <- function(cap) {
  vapply(cap$calls, function(x) x[[3]], character(1))
}

# Value recorded for a given parameter
recorded_value <- function(cap, param) {
  idx <- which(recorded_params(cap) == param)
  cap$calls[[idx]][[4]]
}

# p-value table as returned by build_pval_table() for comparison "A_vs_B"
make_pval_tbl <- function() {
  data.frame(
    id = paste0("p", 1:4),
    `logFC (A_vs_B)` = c(1, 2, 3, 4),
    `P_Value (A_vs_B)` = c(0.5, 0.01, 0.9, 0.04),
    `Log_PValue (A_vs_B)` = c(0.301, 2, 0.046, 1.398),
    `Adjusted_PValue (A_vs_B)` = c(0.5, 0.01, NA, 0.2),
    `isDifferential (A_vs_B)` = c(0, 1, 0, 1),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# Mocks Prostar2::Add2History(): records every call (without the history) in
# 'cap$calls' and the history received by the first call in 'cap$first'
mock_history_fdr <- function(cap, env = parent.frame()) {
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
}

# -log10(P_Value): 3, 2, 1.301, 0.301 | abs(logFC): 2, 0.5, 1.5, 3
make_count_tbl <- function() {
  data.frame(
    P_Value = c(0.001, 0.01, 0.05, 0.5),
    logFC   = c(2, -0.5, -1.5, 3)
  )
}

make_fdr_tbl <- function() {
  data.frame(
    Log_PValue      = c(3, 2, 1, 0.5),
    logFC           = c(2, 0.5, 1.5, 3),
    Adjusted_PValue = c(0.01, 0.3, 0.2, 0.5)
  )
}

# SummarizedExperiment with 'n' proteins and two annotation columns
make_se_pval <- function(n = 5) {
  mat <- matrix(
    seq_len(n * 2), nrow = n,
    dimnames = list(paste0("p", seq_len(n)), c("S1", "S2"))
  )
  se <- SummarizedExperiment::SummarizedExperiment(assays = list(counts = mat))
  SummarizedExperiment::rowData(se)$Protein <- paste0("Prot", seq_len(n))
  SummarizedExperiment::rowData(se)$Description <- paste0("Desc", seq_len(n))
  se
}

# Fake HypothesisTest() result for one comparison
make_ht_pval <- function(pval, logfc, comp = "A_vs_B") {
  ht <- data.frame(logfc, pval)
  names(ht) <- paste0(comp, c("_logFC", "_pval"))
  ht
}

# Mocks HypothesisTest() and diffAnaComputeAdjustedPValues().
# The fake adjusted p-values are the p-values divided by 2.
#   cap$data     : object given to HypothesisTest()
#   cap$adj_args : arguments given to diffAnaComputeAdjustedPValues()
mock_pval_deps <- function(cap, ht, env = parent.frame()) {
  testthat::local_mocked_bindings(
    HypothesisTest = function(object, ...) {
      cap$data <- object
      ht
    },
    diffAnaComputeAdjustedPValues = function(...) {
      args <- list(...)
      cap$adj_args <- args
      args[[1]] / 2
    },
    .package = "DaparToolshed",
    .env = env
  )
}

# 5 proteins: #4 has a pushed p-value (> 1), #2 has a logFC under threshold
pval5  <- c(0.001, 0.01, 0.5, 1.00000000001, 0.03)
logfc5 <- c(2, 0.5, -1.5, 3, -2)

mock_init_complete <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    initComplete = function(...) DT::JS("function(settings, json) {}"),
    .package = "MagellanNTK",
    .env = env
  )
}

dt_ids <- function(widget) as.character(unlist(widget$x$data[[1]]))

# Saves the workbook, unzips it and returns the paths
save_and_unzip <- function(wb, env = parent.frame()) {
  xlsx <- withr::local_tempfile(fileext = ".xlsx", .local_envir = env)
  openxlsx::saveWorkbook(wb, xlsx, overwrite = TRUE)
  dir <- withr::local_tempdir(.local_envir = env)
  utils::unzip(xlsx, exdir = dir)
  list(xlsx = xlsx, dir = dir)
}

read_xml_file <- function(path) paste(readLines(path, warn = FALSE), collapse = "")

# ---- Tests -----------------------------------------------------------------

## ----- getlogFC -----
test_that("getlogFC OnevsOne computes the difference of group means", {
  qf <- make_qf_fc(mat_2c(), c("A", "A", "B", "B"))
  
  res <- getlogFC(qf, type = "OnevsOne")
  
  expected <- matrix(
    c(-5, -3, 10), ncol = 1,
    dimnames = list(paste0("f", 1:3), "A_vs_B_logFC")
  )
  expect_equal(res, expected)
})

test_that("getlogFC uses OnevsOne by default", {
  qf <- make_qf_fc(mat_2c(), c("A", "A", "B", "B"))
  
  expect_equal(getlogFC(qf), getlogFC(qf, type = "OnevsOne"))
})

test_that("getlogFC uses the last assay", {
  qf <- make_qf_fc(mat_2c(), c("A", "A", "B", "B"))
  
  # The first assay is 100 x bigger: the result would be 100 x bigger too
  expect_equal(unname(getlogFC(qf)[, 1]), c(-5, -3, 10))
})

test_that("getlogFC OnevsOne gives all pairwise contrasts for 3 conditions", {
  qf <- make_qf_fc(mat_3c(), rep(c("A", "B", "C"), each = 2))
  
  res <- getlogFC(qf, "OnevsOne")
  
  expected <- matrix(
    c(-4, -8, -4,
      0, -6, -6),
    nrow = 2, byrow = TRUE,
    dimnames = list(paste0("f", 1:2),
                    c("A_vs_B_logFC", "A_vs_C_logFC", "B_vs_C_logFC"))
  )
  expect_equal(res, expected)
  expect_equal(ncol(res), choose(3, 2))
})

test_that("getlogFC orders contrasts by first appearance of the conditions", {
  qf <- make_qf_fc(mat_2c(), c("B", "B", "A", "A"))
  
  res <- getlogFC(qf, "OnevsOne")
  
  expect_identical(colnames(res), "B_vs_A_logFC")
  # B - A is the opposite of A - B
  expect_equal(unname(res[, 1]), c(-5, -3, 10))
})

test_that("getlogFC OnevsAll compares each condition to all the others", {
  qf <- make_qf_fc(mat_3c(), rep(c("A", "B", "C"), each = 2))
  
  res <- getlogFC(qf, "OnevsAll")
  
  expected <- matrix(
    c(-6,  0, 6,
      -3, -3, 6),
    nrow = 2, byrow = TRUE,
    dimnames = list(paste0("f", 1:2),
                    c("A_vs_(all-A)_logFC",
                      "B_vs_(all-B)_logFC",
                      "C_vs_(all-C)_logFC"))
  )
  expect_equal(res, expected)
})

test_that("getlogFC OnevsAll with 2 conditions gives opposite columns", {
  qf <- make_qf_fc(mat_2c(), c("A", "A", "B", "B"))
  
  res <- getlogFC(qf, "OnevsAll")
  
  expect_equal(ncol(res), 2)
  expect_equal(unname(res[, 1]), -unname(res[, 2]))
  expect_equal(unname(res[, 1]), c(-5, -3, 10))
})

test_that("getlogFC returns one row per feature, with the feature names", {
  qf <- make_qf_fc(mat_2c(), c("A", "A", "B", "B"))
  
  expect_identical(rownames(getlogFC(qf, "OnevsOne")), paste0("f", 1:3))
  expect_identical(rownames(getlogFC(qf, "OnevsAll")), paste0("f", 1:3))
})

test_that("getlogFC returns NULL for an unknown type", {
  qf <- make_qf_fc(mat_2c(), c("A", "A", "B", "B"))
  
  expect_null(getlogFC(qf, type = "Other"))
})

test_that("getlogFC works on a real dataset", {
  skip_if_not_installed("DaparToolshed")
  utils::data("subR25prot", package = "DaparToolshed", envir = environment())
  
  conds <- unique(SummarizedExperiment::colData(subR25prot)$Condition)
  
  res <- getlogFC(subR25prot)
  
  expect_true(is.matrix(res))
  expect_equal(ncol(res), choose(length(conds), 2))
  expect_equal(nrow(res), nrow(subR25prot[[length(subR25prot)]]))
  expect_true(all(grepl("_vs_.*_logFC$", colnames(res))))
})





## ----- checkLimma -----
test_that("checkLimma returns a single logical", {
  res <- run_checkLimma(2, 1)
  
  expect_type(res, "logical")
  expect_length(res, 1)
})

test_that("checkLimma is TRUE for a level 1 design up to 26 conditions", {
  expect_true(run_checkLimma(2, 1))
  expect_true(run_checkLimma(10, 1))
  expect_true(run_checkLimma(26, 1))   # boundary
})

test_that("checkLimma is FALSE for a level 1 design with more than 26 conditions", {
  expect_false(run_checkLimma(27, 1))
  expect_false(run_checkLimma(40, 1))
})

test_that("checkLimma is TRUE for level 2 and 3 designs with fewer than 10 conditions", {
  expect_true(run_checkLimma(2, 2))
  expect_true(run_checkLimma(9, 2))    # boundary
  expect_true(run_checkLimma(2, 3))
  expect_true(run_checkLimma(9, 3))    # boundary
})

test_that("checkLimma is FALSE for level 2 and 3 designs with 10 conditions or more", {
  expect_false(run_checkLimma(10, 2))
  expect_false(run_checkLimma(10, 3))
  expect_false(run_checkLimma(26, 2))
})

test_that("checkLimma is FALSE for other design levels", {
  expect_false(run_checkLimma(2, 4))
  expect_false(run_checkLimma(2, 0))
})

test_that("checkLimma gives the colData of the dataset to getDesignLevel", {
  cap <- new.env()
  qf <- make_qf_fc(mat_2c(), c("A", "A", "B", "B"))
  
  run_checkLimma(2, 1, cap)
  
  expect_equal(cap$design, SummarizedExperiment::colData(qf))
})

test_that("checkLimma works on a real dataset", {
  skip_if_not_installed("DaparToolshed")
  utils::data("subR25prot", package = "DaparToolshed", envir = environment())
  
  res <- checkLimma(subR25prot)
  
  expect_type(res, "logical")
  expect_length(res, 1)
  expect_false(is.na(res))
})





## ----- swapConditions -----
test_that("swapConditions returns a list with 'name' and 'values'", {
  res <- swapConditions(make_logFC(), 1)
  
  expect_type(res, "list")
  expect_named(res, c("name", "values"))
})

test_that("swapConditions swaps the two conditions in the name", {
  lf <- make_logFC()
  
  expect_identical(swapConditions(lf, 1)$name, "B_vs_A_logFC")
  expect_identical(swapConditions(lf, 2)$name, "C_vs_A_logFC")
  expect_identical(swapConditions(lf, 3)$name, "C_vs_B_logFC")
})

test_that("swapConditions negates the values of the selected column", {
  lf <- make_logFC()
  
  res <- swapConditions(lf, 2)
  
  expect_equal(res$values, -lf[, 2])
  #expect_equal(unname(res$values), c(-5, -6, -7, -8)[0] + -unname(lf[, 2]))
  expect_identical(names(res$values), rownames(lf))
})

test_that("swapConditions only uses the selected column", {
  lf <- make_logFC()
  
  expect_equal(swapConditions(lf, 1)$values, -lf[, 1])
  expect_equal(swapConditions(lf, 3)$values, -lf[, 3])
})

test_that("swapConditions does not modify its input", {
  lf <- make_logFC()
  before <- lf
  
  swapConditions(lf, 1)
  
  expect_identical(lf, before)
})

test_that("swapConditions works with a data.frame", {
  df <- as.data.frame(make_logFC())
  
  res <- swapConditions(df, 1)
  
  expect_identical(res$name, "B_vs_A_logFC")
  expect_equal(unname(res$values), -df[, 1])
})

test_that("swapConditions handles condition names containing underscores", {
  lf <- matrix(1:2, ncol = 1,
               dimnames = list(c("f1", "f2"), "Cond_1_vs_Cond_2_logFC"))
  
  expect_identical(swapConditions(lf, 1)$name, "Cond_2_vs_Cond_1_logFC")
})

test_that("swapConditions swaps the OnevsAll contrast names", {
  lf <- matrix(1:2, ncol = 1,
               dimnames = list(c("f1", "f2"), "A_vs_(all-A)_logFC"))
  
  expect_identical(swapConditions(lf, 1)$name, "(all-A)_vs_A_logFC")
})

test_that("swapConditions applied twice gives back the original contrast", {
  lf <- make_logFC()
  
  first <- swapConditions(lf, 1)
  lf2 <- lf
  colnames(lf2)[1] <- first$name
  lf2[, 1] <- first$values
  second <- swapConditions(lf2, 1)
  
  expect_identical(second$name, "A_vs_B_logFC")
  expect_equal(second$values, lf[, 1])
})

test_that("swapConditions errors when i is out of range", {
  expect_error(swapConditions(make_logFC(), 10))
})





## ----- hypothesisTestProt -----
test_that("hypothesisTestProt returns AllPairwiseComp, history and message", {
  qf <- make_qf_fc(mat_2c(), c("A", "A", "B", "B"))
  mock_htp(cap <- new.env())
  
  res <- hypothesisTestProt(qf, method = "Limma", logFC_thr = 1,
                            design = "OnevsOne")
  
  expect_type(res, "list")
  expect_named(res, c("AllPairwiseComp", "history", "message"))
})

test_that("hypothesisTestProt with Limma calls limmaCompleteTest", {
  qf <- make_qf_fc(mat_2c(), c("A", "A", "B", "B"))
  cap <- new.env()
  mock_htp(cap)
  
  res <- hypothesisTestProt(qf, method = "Limma", logFC_thr = 1,
                            design = "OnevsAll")
  
  expect_identical(res$AllPairwiseComp, "limma_res")
  expect_equal(cap$limma$qData, SummarizedExperiment::assay(qf, length(qf)))
  expect_equal(cap$limma$sTab, SummarizedExperiment::colData(qf))
  expect_identical(cap$limma$comp.type, "OnevsAll")
  expect_null(cap$ttest)
})

test_that("hypothesisTestProt with ttests calls compute_t_tests", {
  qf <- make_qf_fc(mat_2c(), c("A", "A", "B", "B"))
  cap <- new.env()
  mock_htp(cap)
  
  res <- hypothesisTestProt(qf, method = "ttests", logFC_thr = 1,
                            design = "OnevsOne", ttest_type = "Welch")
  
  expect_identical(res$AllPairwiseComp, "ttest_res")
  expect_identical(cap$ttest$obj, qf)
  expect_identical(cap$ttest$i, length(qf))
  expect_identical(cap$ttest$contrast, "OnevsOne")
  expect_identical(cap$ttest$type, "Welch")
  expect_null(cap$limma)
})

test_that("hypothesisTestProt returns NULL when the test emits a warning", {
  qf <- make_qf_fc(mat_2c(), c("A", "A", "B", "B"))
  mock_htp(cap <- new.env(), limma = warns, ttest = warns)
  
  res_l <- hypothesisTestProt(qf, "Limma", NULL, 1, "OnevsOne")
  res_t <- hypothesisTestProt(qf, "ttests", NULL, 1, "OnevsOne", "Student")
  
  expect_null(res_l$AllPairwiseComp)
  expect_null(res_t$AllPairwiseComp)
})

test_that("hypothesisTestProt returns NULL when the test fails", {
  qf <- make_qf_fc(mat_2c(), c("A", "A", "B", "B"))
  mock_htp(cap <- new.env(), limma = fails, ttest = fails)
  
  res_l <- hypothesisTestProt(qf, "Limma", NULL, 1, "OnevsOne")
  res_t <- hypothesisTestProt(qf, "ttests", NULL, 1, "OnevsOne", "Student")
  
  expect_null(res_l$AllPairwiseComp)
  expect_null(res_t$AllPairwiseComp)
})

test_that("hypothesisTestProt does not let errors or warnings propagate", {
  qf <- make_qf_fc(mat_2c(), c("A", "A", "B", "B"))
  mock_htp(cap <- new.env(), limma = fails, ttest = warns)
  
  expect_no_error(hypothesisTestProt(qf, "Limma", NULL, 1, "OnevsOne"))
  expect_no_warning(
    hypothesisTestProt(qf, "ttests", NULL, 1, "OnevsOne", "Student")
  )
})

test_that("hypothesisTestProt returns NULL for an unknown method", {
  qf <- make_qf_fc(mat_2c(), c("A", "A", "B", "B"))
  cap <- new.env()
  mock_htp(cap)
  
  res <- hypothesisTestProt(qf, "Unknown", NULL, 1, "OnevsOne")
  
  expect_null(res$AllPairwiseComp)
  expect_null(cap$limma)
  expect_null(cap$ttest)
})

test_that("hypothesisTestProt still updates the history when the test fails", {
  qf <- make_qf_fc(mat_2c(), c("A", "A", "B", "B"))
  cap <- new.env()
  mock_htp(cap, limma = fails)
  
  res <- hypothesisTestProt(qf, "Limma", NULL, 1, "OnevsOne")
  
  expect_null(res$AllPairwiseComp)
  expect_length(cap$calls, 3)
  expect_length(res$history, 3)
})

test_that("hypothesisTestProt initialises the history when it is NULL", {
  qf <- make_qf_fc(mat_2c(), c("A", "A", "B", "B"))
  cap <- new.env()
  mock_htp(cap)
  
  hypothesisTestProt(qf, "Limma", history = NULL, logFC_thr = 1,
                     design = "OnevsOne")
  
  expect_true(cap$init)
  expect_identical(cap$first[[1]], list())   # value returned by InitializeHistory
})

test_that("hypothesisTestProt keeps an existing history", {
  qf <- make_qf_fc(mat_2c(), c("A", "A", "B", "B"))
  cap <- new.env()
  mock_htp(cap)
  init <- list(previous = "step")
  
  res <- hypothesisTestProt(qf, "Limma", history = init, logFC_thr = 1,
                            design = "OnevsOne")
  
  expect_null(cap$init)
  expect_identical(cap$first[[1]], init)
  expect_identical(res$history[["previous"]], "step")
  expect_length(res$history, 4)   # previous step + 3 new entries
})

test_that("hypothesisTestProt records 3 parameters in the history for Limma", {
  qf <- make_qf_fc(mat_2c(), c("A", "A", "B", "B"))
  cap <- new.env()
  mock_htp(cap)
  
  hypothesisTestProt(qf, "Limma", NULL, logFC_thr = "0.75",
                     design = "OnevsAll")
  
  expect_length(cap$calls, 3)
  for (cl in cap$calls) {
    expect_identical(cl[[1]], "HypothesisTest")
    expect_identical(cl[[2]], "HypothesisTest")
  }
  expect_identical(
    vapply(cap$calls, function(x) x[[3]], character(1)),
    c("method", "design", "thlogFC")
  )
  expect_identical(cap$calls[[1]][[4]], "Limma")
  expect_identical(cap$calls[[2]][[4]], "OnevsAll")
  # logFC_thr is stored as a number
  expect_identical(cap$calls[[3]][[4]], 0.75)
})

test_that("hypothesisTestProt also records the t-test type for ttests", {
  qf <- make_qf_fc(mat_2c(), c("A", "A", "B", "B"))
  cap <- new.env()
  mock_htp(cap)
  
  res <- hypothesisTestProt(qf, "ttests", NULL, logFC_thr = 1,
                            design = "OnevsOne", ttest_type = "Welch")
  
  expect_length(cap$calls, 4)
  expect_identical(
    vapply(cap$calls, function(x) x[[3]], character(1)),
    c("method", "design", "thlogFC", "ttestOptions")
  )
  expect_identical(cap$calls[[1]][[4]], "ttests")
  expect_identical(cap$calls[[4]][[4]], "Welch")
  expect_length(res$history, 4)
})

test_that("hypothesisTestProt does not record ttestOptions for Limma", {
  qf <- make_qf_fc(mat_2c(), c("A", "A", "B", "B"))
  cap <- new.env()
  mock_htp(cap)
  
  hypothesisTestProt(qf, "Limma", NULL, 1, "OnevsOne", ttest_type = "Welch")
  
  params <- vapply(cap$calls, function(x) x[[3]], character(1))
  expect_false("ttestOptions" %in% params)
})

test_that("hypothesisTestProt: current behaviour of the 'message' element", {
  # The handlers assign `message <- e$message` in their OWN environment, so
  # the `message` returned by the function is never the error text: it is
  # base::message(). If the handlers are fixed (e.g. with `<<-`), update this
  # test to expect the warning/error text instead.
  qf <- make_qf_fc(mat_2c(), c("A", "A", "B", "B"))
  mock_htp(cap <- new.env(), limma = fails)
  
  res <- hypothesisTestProt(qf, "Limma", NULL, 1, "OnevsOne")
  
  expect_true(is.function(res$message))
})





## ----- GetdatasetToAnalyze -----
test_that("GetdatasetToAnalyze returns the whole last assay for an 'all-' comparison", {
  qf <- make_qf_gda()
  cap <- new.env()
  mock_design(cap)
  
  res <- GetdatasetToAnalyze(qf, "A_vs_(all-A)")
  
  expect_s4_class(res, "SummarizedExperiment")
  expect_identical(res, qf[[length(qf)]])
  expect_equal(ncol(res), 6)
  # design_qf() is not needed in this branch
  expect_false(isTRUE(cap$called))
})

test_that("GetdatasetToAnalyze detects 'all-' wherever it is in the name", {
  qf <- make_qf_gda()
  mock_design(new.env())
  
  res <- GetdatasetToAnalyze(qf, "(all-B)_vs_B")
  
  expect_equal(ncol(res), 6)
})

test_that("GetdatasetToAnalyze keeps only the samples of the two conditions", {
  qf <- make_qf_gda()
  mock_design(new.env())
  
  res <- GetdatasetToAnalyze(qf, "A_vs_B")
  
  expect_s4_class(res, "SummarizedExperiment")
  expect_identical(colnames(res), paste0("S", 1:4))
  expect_equal(nrow(res), 4)
})

test_that("GetdatasetToAnalyze works for non-adjacent conditions", {
  qf <- make_qf_gda()
  mock_design(new.env())
  
  res <- GetdatasetToAnalyze(qf, "A_vs_C")
  
  expect_identical(colnames(res), c("S1", "S2", "S5", "S6"))
})

test_that("GetdatasetToAnalyze does not depend on the order of the conditions", {
  qf <- make_qf_gda()
  mock_design(new.env())
  
  res_ab <- GetdatasetToAnalyze(qf, "A_vs_B")
  res_ba <- GetdatasetToAnalyze(qf, "B_vs_A")
  
  expect_identical(colnames(res_ab), colnames(res_ba))
})

test_that("GetdatasetToAnalyze uses the last assay", {
  qf <- make_qf_gda()
  mock_design(new.env())
  
  res <- GetdatasetToAnalyze(qf, "A_vs_B")
  
  expect_equal(
    SummarizedExperiment::assay(res),
    SummarizedExperiment::assay(qf[["last"]])[, 1:4]
  )
})

test_that("GetdatasetToAnalyze subsets the qMetacell matrix like the samples", {
  qf <- make_qf_gda()
  mock_design(new.env())
  orig <- SummarizedExperiment::rowData(qf[["last"]])$qMetacell
  
  res_ab <- GetdatasetToAnalyze(qf, "A_vs_B")
  res_ac <- GetdatasetToAnalyze(qf, "A_vs_C")
  
  q_ab <- SummarizedExperiment::rowData(res_ab)$qMetacell
  q_ac <- SummarizedExperiment::rowData(res_ac)$qMetacell
  
  expect_equal(dim(q_ab), c(4, 4))
  expect_equal(q_ab, orig[, 1:4])
  expect_equal(dim(q_ac), c(4, 4))
  expect_equal(q_ac, orig[, c(1, 2, 5, 6)])
  # qMetacell has as many columns as the dataset
  expect_equal(ncol(q_ab), ncol(res_ab))
})

test_that("GetdatasetToAnalyze keeps the other rowData columns", {
  qf <- make_qf_gda()
  mock_design(new.env())
  
  res <- GetdatasetToAnalyze(qf, "A_vs_B")
  
  expect_identical(SummarizedExperiment::rowData(res)$Protein, paste0("P", 1:4))
})

test_that("GetdatasetToAnalyze gives the dataset to design_qf", {
  qf <- make_qf_gda()
  cap <- new.env()
  mock_design(cap)
  
  GetdatasetToAnalyze(qf, "A_vs_B")
  
  expect_true(cap$called)
  expect_identical(cap$data, qf)
})

test_that("GetdatasetToAnalyze does not modify the input object", {
  qf <- make_qf_gda()
  mock_design(new.env())
  
  GetdatasetToAnalyze(qf, "A_vs_B")
  
  expect_equal(ncol(qf[["last"]]), 6)
  expect_equal(ncol(SummarizedExperiment::rowData(qf[["last"]])$qMetacell), 6)
})





## ----- pushPvalues -----
test_that("pushPvalues returns data, dt and pushed", {
  store <- new.env(); store$ht <- make_ht()
  mock_hypothesis(store)
  
  res <- pushPvalues(make_se_n(), ind = 1, command = "delete", query = "q",
                     comparison = "A_vs_B", dt = make_dt0(), pushed = list())
  
  expect_type(res, "list")
  expect_named(res, c("data", "dt", "pushed"))
  expect_s4_class(res$data, "SummarizedExperiment")
  expect_s3_class(res$dt, "data.frame")
})

test_that("pushPvalues with 'delete' pushes the given indices", {
  store <- new.env(); store$ht <- make_ht()
  mock_hypothesis(store)
  
  pushPvalues(make_se_n(), ind = c(1, 3), command = "delete", query = "q",
              comparison = "A_vs_B", dt = make_dt0(), pushed = list())
  
  pv <- store$ht$A_vs_B_pval
  # Pushed values are set to 1.00000000001 (> 1)
  expect_true(all(pv[c(1, 3)] == 1.00000000001))
  expect_true(all(pv[c(1, 3)] > 1))
  # Other p-values are untouched
  expect_equal(pv[c(2, 4, 5)], rep(0.01, 3))
})

test_that("pushPvalues with 'keep' pushes every index except the given ones", {
  store <- new.env(); store$ht <- make_ht()
  mock_hypothesis(store)
  
  res <- pushPvalues(make_se_n(), ind = c(1, 2), command = "keep", query = "q",
                     comparison = "A_vs_B", dt = make_dt0(), pushed = list())
  
  pv <- store$ht$A_vs_B_pval
  expect_true(all(pv[3:5] > 1))
  expect_equal(pv[1:2], c(0.01, 0.01))
  expect_equal(res$pushed, list(A_vs_B = 3:5))
})

test_that("pushPvalues only changes the column of the given comparison", {
  store <- new.env(); store$ht <- make_ht()
  mock_hypothesis(store)
  
  pushPvalues(make_se_n(), ind = 1:2, command = "delete", query = "q",
              comparison = "A_vs_B", dt = make_dt0(), pushed = list())
  
  expect_equal(store$ht$A_vs_C_pval, rep(0.02, 5))
})

test_that("pushPvalues records the pushed indices per comparison", {
  store <- new.env(); store$ht <- make_ht()
  mock_hypothesis(store)
  
  res <- pushPvalues(make_se_n(), ind = c(1, 3), command = "delete",
                     query = "q", comparison = "A_vs_B",
                     dt = make_dt0(), pushed = list())
  
  expect_equal(res$pushed, list(A_vs_B = c(1, 3)))
})

test_that("pushPvalues adds a row with the counts to dt", {
  store <- new.env(); store$ht <- make_ht()
  mock_hypothesis(store)
  dt0 <- make_dt0()
  
  res <- pushPvalues(make_se_n(5), ind = c(1, 3), command = "delete",
                     query = "my query", comparison = "A_vs_B",
                     dt = dt0, pushed = list())
  
  expect_equal(nrow(res$dt), nrow(dt0) + 1)
  expect_named(res$dt, names(dt0))
  # comparison, query, nbpushed, totalpushed, totalnonpushed
  expect_identical(
    unlist(res$dt[2, ], use.names = FALSE),
    c("A_vs_B", "my query", "2", "2", "3")
  )
  # The existing row is kept
  expect_identical(unlist(res$dt[1, ], use.names = FALSE), rep("-", 5))
})

test_that("pushPvalues accumulates the pushed indices of the same comparison", {
  store <- new.env(); store$ht <- make_ht()
  mock_hypothesis(store)
  se <- make_se_n(5)
  
  first <- pushPvalues(se, ind = c(1, 2), command = "delete", query = "q1",
                       comparison = "A_vs_B", dt = make_dt0(), pushed = list())
  second <- pushPvalues(se, ind = c(2, 3), command = "delete", query = "q2",
                        comparison = "A_vs_B", dt = first$dt,
                        pushed = first$pushed)
  
  # Index 2 was already pushed: unique indices are 1, 2, 3
  expect_equal(sort(second$pushed$A_vs_B), c(1, 2, 3))
  expect_equal(nrow(second$dt), 3)
  # nbpushed = 2 (this call), totalpushed = 3, totalnonpushed = 5 - 3
  expect_identical(
    unlist(second$dt[3, ], use.names = FALSE),
    c("A_vs_B", "q2", "2", "3", "2")
  )
  expect_true(all(store$ht$A_vs_B_pval[1:3] > 1))
  expect_equal(store$ht$A_vs_B_pval[4:5], c(0.01, 0.01))
})

test_that("pushPvalues keeps the comparisons independent", {
  store <- new.env(); store$ht <- make_ht()
  mock_hypothesis(store)
  se <- make_se_n(5)
  
  first <- pushPvalues(se, ind = c(1, 2), command = "delete", query = "q1",
                       comparison = "A_vs_B", dt = make_dt0(), pushed = list())
  second <- pushPvalues(se, ind = c(2, 3), command = "delete", query = "q2",
                        comparison = "A_vs_C", dt = first$dt,
                        pushed = first$pushed)
  
  expect_named(second$pushed, c("A_vs_B", "A_vs_C"))
  expect_equal(second$pushed$A_vs_B, c(1, 2))
  expect_equal(second$pushed$A_vs_C, c(2, 3))
  # Totals of the new comparison start from 0
  expect_identical(
    unlist(second$dt[3, ], use.names = FALSE),
    c("A_vs_C", "q2", "2", "2", "3")
  )
  expect_true(all(store$ht$A_vs_C_pval[2:3] > 1))
  expect_equal(store$ht$A_vs_C_pval[c(1, 4, 5)], rep(0.02, 3))
})

test_that("pushPvalues errors on an invalid command", {
  store <- new.env(); store$ht <- make_ht()
  mock_hypothesis(store)
  
  expect_error(
    pushPvalues(make_se_n(), 1, "other", "q", "A_vs_B", make_dt0(), list())
  )
  expect_error(
    pushPvalues(make_se_n(), 1, NULL, "q", "A_vs_B", make_dt0(), list())
  )
})

test_that("pushPvalues: 'keep' with no index pushes nothing (current behaviour)", {
  # seq_len(n)[-integer(0)] is integer(0) in R, not seq_len(n): keeping an
  # empty selection therefore pushes no p-value, instead of all of them.
  store <- new.env(); store$ht <- make_ht()
  mock_hypothesis(store)
  
  res <- pushPvalues(make_se_n(5), ind = integer(0), command = "keep",
                     query = "q", comparison = "A_vs_B",
                     dt = make_dt0(), pushed = list())
  
  expect_equal(store$ht$A_vs_B_pval, rep(0.01, 5))
  expect_identical(
    unlist(res$dt[2, ], use.names = FALSE),
    c("A_vs_B", "q", "0", "0", "5")
  )
})





## ----- updatePushedDT -----
test_that("updatePushedDT keeps the first row and the rows of the comparison", {
  res <- updatePushedDT(make_dt_upd(), "A_vs_B")
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3)
  expect_identical(res$query, c("-", "q1", "q3"))
  expect_identical(res$nbpushed, c("-", "1", "3"))
})

test_that("updatePushedDT removes the 'comparison' column", {
  res <- updatePushedDT(make_dt_upd(), "A_vs_B")
  
  expect_named(res, c("query", "nbpushed"))
})

test_that("updatePushedDT works for another comparison", {
  res <- updatePushedDT(make_dt_upd(), "A_vs_C")
  
  expect_identical(res$query, c("-", "q2"))
})

test_that("updatePushedDT only returns the first row when nothing matches", {
  res <- updatePushedDT(make_dt_upd(), "B_vs_C")
  
  expect_equal(nrow(res), 1)
  expect_identical(res$query, "-")
})

test_that("updatePushedDT does not modify its input", {
  dt <- make_dt_upd()
  before <- dt
  
  updatePushedDT(dt, "A_vs_B")
  
  expect_identical(dt, before)
})





## ----- Get_Pairwisecomparison_Names -----
test_that("Get_Pairwisecomparison_Names removes the _logFC and _pval suffixes", {
  df <- data.frame(
    A_vs_B_logFC = 1, A_vs_B_pval = 1,
    A_vs_C_logFC = 1, A_vs_C_pval = 1
  )
  
  expect_identical(Get_Pairwisecomparison_Names(df), c("A_vs_B", "A_vs_C"))
})

test_that("Get_Pairwisecomparison_Names returns unique names", {
  df <- data.frame(A_vs_B_logFC = 1, A_vs_B_pval = 1)
  
  expect_length(Get_Pairwisecomparison_Names(df), 1)
})

test_that("Get_Pairwisecomparison_Names keeps the order of appearance", {
  df <- data.frame(
    B_vs_C_logFC = 1, A_vs_B_logFC = 1,
    B_vs_C_pval = 1, A_vs_B_pval = 1
  )
  
  expect_identical(Get_Pairwisecomparison_Names(df), c("B_vs_C", "A_vs_B"))
})

test_that("Get_Pairwisecomparison_Names handles OnevsAll names", {
  m <- matrix(1, nrow = 1, ncol = 2,
              dimnames = list(NULL, c("A_vs_(all-A)_logFC", "A_vs_(all-A)_pval")))
  
  expect_identical(Get_Pairwisecomparison_Names(m), "A_vs_(all-A)")
})

test_that("Get_Pairwisecomparison_Names leaves other column names unchanged", {
  df <- data.frame(A_vs_B_logFC = 1, A_vs_B_pval = 1, other = 1)
  
  expect_identical(Get_Pairwisecomparison_Names(df), c("A_vs_B", "other"))
})

test_that("Get_Pairwisecomparison_Names returns character(0) without column names", {
  expect_identical(Get_Pairwisecomparison_Names(data.frame()), character(0))
  expect_identical(Get_Pairwisecomparison_Names(matrix(1:4, 2)), character(0))
})





## ----- pairwiseComparisonProt -----
test_that("pairwiseComparisonProt returns a list with the history", {
  mock_history(new.env())
  
  res <- pairwiseComparisonProt(NULL, "A_vs_B", make_dt_pw("-"), list())
  
  expect_type(res, "list")
  expect_named(res, "history")
})

test_that("pairwiseComparisonProt records 3 entries in the history", {
  cap <- new.env()
  mock_history(cap)
  pushed <- list(A_vs_B = c(1, 2))
  
  res <- pairwiseComparisonProt(NULL, "A_vs_B", make_dt_pw("-"), pushed)
  
  expect_length(cap$calls, 3)
  expect_length(res$history, 3)
  for (cl in cap$calls) {
    expect_identical(cl[[1]], "DA")
    expect_identical(cl[[2]], "Pairwisecomparison")
  }
  expect_identical(
    vapply(cap$calls, function(x) x[[3]], character(1)),
    c("Comparison", "Push pval query", "Nb pushed pval")
  )
  expect_identical(cap$calls[[1]][[4]], "A_vs_B")
  expect_identical(cap$calls[[3]][[4]], pushed)
})

test_that("pairwiseComparisonProt uses '-' when no query was recorded", {
  cap <- new.env()
  mock_history(cap)
  
  # Only the header row
  pairwiseComparisonProt(NULL, "A_vs_B", make_dt_pw("-"), list())
  expect_identical(cap$calls[[2]][[4]], "-")
})

test_that("pairwiseComparisonProt uses '-' for an empty dt", {
  cap <- new.env()
  mock_history(cap)
  
  pairwiseComparisonProt(NULL, "A_vs_B", make_dt_pw(character(0)), list())
  
  expect_identical(cap$calls[[2]][[4]], "-")
})

test_that("pairwiseComparisonProt records the query, ignoring the header row", {
  cap <- new.env()
  mock_history(cap)
  
  pairwiseComparisonProt(NULL, "A_vs_B", make_dt_pw(c("-", "my query")), list())
  
  expect_identical(cap$calls[[2]][[4]], "my query")
})

test_that("pairwiseComparisonProt: current behaviour with several queries", {
  # paste(..., sep = " ; ") does not collapse: with 2 queries or more, the
  # query passed to the history is a vector, not one string joined with " ; ".
  # If `collapse = " ; "` is used, expect "q1 ; q2" instead.
  cap <- new.env()
  mock_history(cap)
  
  pairwiseComparisonProt(NULL, "A_vs_B", make_dt_pw(c("-", "q1", "q2")), list())
  
  expect_identical(cap$calls[[2]][[4]], c("q1", "q2"))
})

test_that("pairwiseComparisonProt passes the existing history to Add2History", {
  cap <- new.env()
  mock_history(cap)
  init <- list(previous = "step")
  
  res <- pairwiseComparisonProt(init, "A_vs_B", make_dt_pw("-"), list())
  
  expect_identical(cap$first[[1]], init)
  expect_identical(res$history[["previous"]], "step")
  expect_length(res$history, 4)   # previous step + 3 new entries
})





## ----- GetCalibMethod -----
test_that("GetCalibMethod returns a named character vector", {
  res <- GetCalibMethod()
  
  expect_type(res, "character")
  expect_false(is.null(names(res)))
})

test_that("GetCalibMethod returns the expected methods in order", {
  expect_identical(
    unname(GetCalibMethod()),
    c("Benjamini-Hochberg",
      "st.boot", "st.spline",
      "langaas", "jiang", "histo",
      "pounds", "abh", "slim",
      "numeric value")
  )
})

test_that("GetCalibMethod names are identical to the values", {
  res <- GetCalibMethod()
  
  expect_identical(names(res), unname(res))
})

test_that("GetCalibMethod has 10 unique methods", {
  res <- GetCalibMethod()
  
  expect_length(res, 10)
  expect_false(anyDuplicated(res) > 0)
})

test_that("GetCalibMethod includes the methods handled by get_calibration_method", {
  res <- GetCalibMethod()
  
  expect_true(all(c("Benjamini-Hochberg", "numeric value") %in% res))
})

test_that("GetCalibMethod is deterministic", {
  expect_identical(GetCalibMethod(), GetCalibMethod())
})





## ----- getPValueNonPushed -----
test_that("getPValueNonPushed removes p-values greater than 1", {
  res <- getPValueNonPushed(c(0.01, 0.5, 1.00000000001, 0.9, 1.5))
  
  expect_equal(res, c(0.01, 0.5, 0.9))
})

test_that("getPValueNonPushed returns the input when no p-value is pushed", {
  pval <- c(0.01, 0.5, 0.99)
  
  expect_identical(getPValueNonPushed(pval), pval)
})

test_that("getPValueNonPushed keeps p-values equal to 1", {
  expect_equal(getPValueNonPushed(c(0.2, 1, 1)), c(0.2, 1, 1))
})

test_that("getPValueNonPushed removes all pushed p-values", {
  res <- getPValueNonPushed(c(1.00000000001, 1.00000000001))
  
  expect_length(res, 0)
  expect_type(res, "double")
})

test_that("getPValueNonPushed keeps the order of the remaining values", {
  res <- getPValueNonPushed(c(0.9, 2, 0.1, 3, 0.5))
  
  expect_equal(res, c(0.9, 0.1, 0.5))
})

test_that("getPValueNonPushed handles an empty vector", {
  expect_equal(getPValueNonPushed(numeric(0)), numeric(0))
})

test_that("getPValueNonPushed keeps NA values", {
  # which() ignores NA, so NA is neither pushed nor removed
  res <- getPValueNonPushed(c(0.1, NA, 2, 0.3))
  
  expect_equal(res, c(0.1, NA, 0.3))
})

test_that("getPValueNonPushed keeps the names of the remaining values", {
  pval <- c(a = 0.1, b = 2, c = 0.3)
  
  expect_equal(getPValueNonPushed(pval), c(a = 0.1, c = 0.3))
})





## ----- get_calibration_method -----
test_that("get_calibration_method returns 1 for Benjamini-Hochberg", {
  res <- get_calibration_method("Benjamini-Hochberg")
  
  expect_identical(res, 1)
  expect_type(res, "double")
})

test_that("get_calibration_method ignores numeric_value for Benjamini-Hochberg", {
  expect_identical(get_calibration_method("Benjamini-Hochberg", 0.3), 1)
})

test_that("get_calibration_method returns the numeric value for 'numeric value'", {
  expect_identical(get_calibration_method("numeric value", 0.25), 0.25)
})

test_that("get_calibration_method converts a character numeric_value to numeric", {
  res <- get_calibration_method("numeric value", "0.4")
  
  expect_type(res, "double")
  expect_equal(res, 0.4)
})

test_that("get_calibration_method returns an empty numeric for a NULL numeric_value", {
  # as.numeric(NULL) is numeric(0)
  res <- get_calibration_method("numeric value")
  
  expect_identical(res, numeric(0))
})

test_that("get_calibration_method returns NA with a warning for a non numeric value", {
  expect_warning(
    res <- get_calibration_method("numeric value", "abc")
  )
  expect_true(is.na(res))
})

test_that("get_calibration_method returns the method name for other methods", {
  others <- setdiff(
    unname(GetCalibMethod()),
    c("Benjamini-Hochberg", "numeric value")
  )
  
  for (m in others) {
    expect_identical(get_calibration_method(m), m)
  }
})

test_that("get_calibration_method ignores numeric_value for other methods", {
  expect_identical(get_calibration_method("langaas", 0.5), "langaas")
})

test_that("get_calibration_method returns an unknown method unchanged", {
  expect_identical(get_calibration_method("something"), "something")
})

test_that("get_calibration_method errors when the method is NULL or empty", {
  expect_error(get_calibration_method(NULL))
  expect_error(get_calibration_method(character(0)))
})

test_that("get_calibration_method works with every value of GetCalibMethod", {
  for (m in GetCalibMethod()) {
    res <- get_calibration_method(m, 0.5)
    expect_true(is.character(res) || is.numeric(res))
    expect_length(res, 1)
  }
})





## ----- pvalCalibrationProt -----
test_that("pvalCalibrationProt returns a list with the history", {
  mock_history_cal(new.env())
  
  res <- pvalCalibrationProt(NULL, "langaas", 0.5, 0.2, 0.1)
  
  expect_type(res, "list")
  expect_named(res, "history")
})

test_that("pvalCalibrationProt records 6 entries when pi0 and h1concent are given", {
  cap <- new.env()
  mock_history_cal(cap)
  
  res <- pvalCalibrationProt(NULL, "langaas", 0.5, 0.2, 0.1)
  
  expect_length(cap$calls, 6)
  expect_length(res$history, 6)
  expect_identical(
    recorded_params(cap),
    c("Calibration method", "pi0", "h1.concentration",
      "Uniformity underestimation", "Non-DA protein proportion",
      "DA protein concentration")
  )
})

test_that("pvalCalibrationProt targets the 'Pvaluecalibration' step of 'DA'", {
  cap <- new.env()
  mock_history_cal(cap)
  
  pvalCalibrationProt(NULL, "langaas", 0.5, 0.2, 0.1)
  
  for (cl in cap$calls) {
    expect_identical(cl[[1]], "DA")
    expect_identical(cl[[2]], "Pvaluecalibration")
  }
})

test_that("pvalCalibrationProt records the raw values", {
  cap <- new.env()
  mock_history_cal(cap)
  
  pvalCalibrationProt(NULL, "langaas", 0.5, 0.2, 0.1)
  
  expect_identical(recorded_value(cap, "Calibration method"), "langaas")
  expect_identical(recorded_value(cap, "pi0"), 0.5)
  expect_identical(recorded_value(cap, "h1.concentration"), 0.2)
  expect_identical(recorded_value(cap, "Uniformity underestimation"), 0.1)
})

test_that("pvalCalibrationProt records pi0 and h1concent as percentages", {
  cap <- new.env()
  mock_history_cal(cap)
  
  pvalCalibrationProt(NULL, "langaas", 0.5, 0.2, 0.1)
  
  expect_equal(recorded_value(cap, "Non-DA protein proportion"), 50)
  expect_equal(recorded_value(cap, "DA protein concentration"), 20)
})

test_that("pvalCalibrationProt rounds the percentages to 2 decimals", {
  cap <- new.env()
  mock_history_cal(cap)
  
  pvalCalibrationProt(NULL, "langaas", 0.123456, 0.987654, 0.1)
  
  expect_equal(recorded_value(cap, "Non-DA protein proportion"), 12.35)
  expect_equal(recorded_value(cap, "DA protein concentration"), 98.77)
})

test_that("pvalCalibrationProt skips the pi0 entries when pi0 is NULL", {
  cap <- new.env()
  mock_history_cal(cap)
  
  res <- pvalCalibrationProt(NULL, "Benjamini-Hochberg", NULL, 0.2, 0.1)
  
  expect_identical(
    recorded_params(cap),
    c("Calibration method", "h1.concentration",
      "Uniformity underestimation", "DA protein concentration")
  )
  expect_false("pi0" %in% recorded_params(cap))
  expect_false("Non-DA protein proportion" %in% recorded_params(cap))
  expect_length(res$history, 4)
})

test_that("pvalCalibrationProt skips the DA concentration when h1concent is NULL", {
  cap <- new.env()
  mock_history_cal(cap)
  
  res <- pvalCalibrationProt(NULL, "langaas", 0.5, NULL, 0.1)
  
  expect_identical(
    recorded_params(cap),
    c("Calibration method", "pi0", "h1.concentration",
      "Uniformity underestimation", "Non-DA protein proportion")
  )
  expect_false("DA protein concentration" %in% recorded_params(cap))
  expect_length(res$history, 5)
})

test_that("pvalCalibrationProt still records h1.concentration when it is NULL", {
  # The 'h1.concentration' entry is not guarded by is.null()
  cap <- new.env()
  mock_history_cal(cap)
  
  pvalCalibrationProt(NULL, "langaas", 0.5, NULL, 0.1)
  
  expect_true("h1.concentration" %in% recorded_params(cap))
  expect_null(recorded_value(cap, "h1.concentration"))
})

test_that("pvalCalibrationProt records only 3 entries when pi0 and h1concent are NULL", {
  cap <- new.env()
  mock_history_cal(cap)
  
  res <- pvalCalibrationProt(NULL, "Benjamini-Hochberg", NULL, NULL, 0.1)
  
  expect_identical(
    recorded_params(cap),
    c("Calibration method", "h1.concentration", "Uniformity underestimation")
  )
  expect_length(res$history, 3)
})

test_that("pvalCalibrationProt passes the existing history to Add2History", {
  cap <- new.env()
  mock_history_cal(cap)
  init <- list(previous = "step")
  
  res <- pvalCalibrationProt(init, "langaas", 0.5, 0.2, 0.1)
  
  expect_identical(cap$first[[1]], init)
  expect_identical(res$history[["previous"]], "step")
  expect_length(res$history, 7)   # previous step + 6 new entries
})

test_that("pvalCalibrationProt errors when pi0 is not numeric", {
  mock_history_cal(new.env())
  
  expect_error(pvalCalibrationProt(NULL, "langaas", "abc", 0.2, 0.1))
})

test_that("pvalCalibrationProt works with the output of get_calibration_method", {
  cap <- new.env()
  mock_history_cal(cap)
  
  calibmet <- get_calibration_method("numeric value", 0.3)
  res <- pvalCalibrationProt(NULL, "numeric value", calibmet, 0.2, 0.1)
  
  expect_equal(recorded_value(cap, "pi0"), 0.3)
  expect_equal(recorded_value(cap, "Non-DA protein proportion"), 30)
  expect_length(res$history, 6)
})





## ----- fdr_warning_text -----
test_that("fdr_warning_text returns a message when fewer than 1 false discovery is expected", {
  res <- fdr_warning_text(fdr = 0.1, n_significant = 5)
  
  expect_type(res, "character")
  expect_length(res, 1)
  expect_identical(
    res,
    paste0("With such a dataset size (5 selected discoveries), an FDR of 10% ",
           "should be cautiously interpreted as strictly less than one ",
           "discovery (0.5) is expected to be false")
  )
})

test_that("fdr_warning_text rounds the FDR percentage and the expected false discoveries", {
  res <- fdr_warning_text(fdr = 0.01234, n_significant = 10)
  
  expect_match(res, "(10 selected discoveries)", fixed = TRUE)
  expect_match(res, "FDR of 1.23%", fixed = TRUE)
  expect_match(res, "discovery (0.12)", fixed = TRUE)
})

test_that("fdr_warning_text returns NULL when at least 1 false discovery is expected", {
  expect_null(fdr_warning_text(fdr = 0.05, n_significant = 100))
  expect_null(fdr_warning_text(fdr = 1, n_significant = 10))
})

test_that("fdr_warning_text returns NULL at the boundary (expected = 1)", {
  expect_null(fdr_warning_text(fdr = 0.5, n_significant = 2))
})

test_that("fdr_warning_text warns when there is no selected protein", {
  res <- fdr_warning_text(fdr = 0.05, n_significant = 0)
  
  expect_type(res, "character")
  expect_match(res, "(0 selected discoveries)", fixed = TRUE)
})

test_that("fdr_warning_text warns when the FDR is 0", {
  expect_type(fdr_warning_text(fdr = 0, n_significant = 50), "character")
})

test_that("fdr_warning_text errors on NA input", {
  expect_error(fdr_warning_text(NA_real_, 10))
  expect_error(fdr_warning_text(0.05, NA_real_))
})





## ----- count_selected_prot -----
test_that("count_selected_prot counts proteins passing both thresholds", {
  # pval: 1, 2 | logFC: 1, 3, 4 -> only protein 1 passes both
  expect_equal(count_selected_prot(make_count_tbl(), thpval = 2, thlogfc = 1), 1)
})

test_that("count_selected_prot uses only the p-value threshold when thlogfc is NULL", {
  expect_equal(count_selected_prot(make_count_tbl(), thpval = 2, thlogfc = NULL), 2)
})

test_that("count_selected_prot uses only the logFC threshold when thpval is NULL", {
  expect_equal(count_selected_prot(make_count_tbl(), thpval = NULL, thlogfc = 1), 3)
})

test_that("count_selected_prot selects everything with thresholds at 0", {
  expect_equal(count_selected_prot(make_count_tbl(), 0, 0), 4)
})

test_that("count_selected_prot selects nothing with very high thresholds", {
  expect_equal(count_selected_prot(make_count_tbl(), 10, 10), 0)
})

test_that("count_selected_prot thresholds are inclusive", {
  tbl <- make_count_tbl()
  
  expect_equal(count_selected_prot(tbl, NULL, 3), 1)            # abs(logFC) == 3
  expect_equal(count_selected_prot(tbl, -log10(0.01), NULL), 2) # same expression
})

test_that("count_selected_prot ignores NA values", {
  tbl <- data.frame(P_Value = c(NA, 0.001), logFC = c(2, 2))
  
  expect_equal(count_selected_prot(tbl, 2, 1), 1)
})

test_that("count_selected_prot returns a single number", {
  res <- count_selected_prot(make_count_tbl(), 2, 1)
  
  expect_true(is.numeric(res))
  expect_length(res, 1)
})

test_that("count_selected_prot errors when both thresholds are NULL", {
  # `sel` is never defined in this case
  expect_error(count_selected_prot(make_count_tbl(), NULL, NULL))
})





## ----- count_significant -----
test_that("count_significant counts the differential proteins of a comparison", {
  expect_equal(count_significant(make_pval_tbl(), "A_vs_B"), 2)
})

test_that("count_significant returns 0 when no protein is differential", {
  tbl <- make_pval_tbl()
  tbl[["isDifferential (A_vs_B)"]] <- 0
  
  expect_equal(count_significant(tbl, "A_vs_B"), 0)
})

test_that("count_significant only looks at the column of the comparison", {
  tbl <- make_pval_tbl()
  tbl[["isDifferential (A_vs_C)"]] <- c(1, 1, 1, 0)
  
  expect_equal(count_significant(tbl, "A_vs_B"), 2)
  expect_equal(count_significant(tbl, "A_vs_C"), 3)
})

test_that("count_significant returns NA when the flag contains NA", {
  tbl <- make_pval_tbl()
  tbl[["isDifferential (A_vs_B)"]] <- c(1, NA, 0, 1)
  
  expect_true(is.na(count_significant(tbl, "A_vs_B")))
})

test_that("count_significant returns 0 for an unknown comparison (current behaviour)", {
  # NULL == 1 is logical(0), and sum(logical(0)) is 0
  expect_equal(count_significant(make_pval_tbl(), "unknown"), 0)
})





## ----- compute_fdr -----
test_that("compute_fdr returns the largest adjusted p-value among the selected proteins", {
  # selected by p-value: 1, 2, 3 | excluded by logFC (< 1): 2 -> 1, 3
  res <- compute_fdr(make_fdr_tbl(), thpval = 1, thlogfc = 1)
  
  expect_equal(res, 0.2)
  expect_true(is.numeric(res))
  expect_length(res, 1)
})

test_that("compute_fdr keeps proteins whose |logFC| equals the threshold", {
  # With thlogfc = 0.5, protein 2 (|logFC| = 0.5) is not excluded
  expect_equal(compute_fdr(make_fdr_tbl(), thpval = 1, thlogfc = 0.5), 0.3)
})

test_that("compute_fdr thpval is inclusive", {
  # protein 3 has Log_PValue == 1
  expect_equal(compute_fdr(make_fdr_tbl(), thpval = 1, thlogfc = 1), 0.2)
  expect_equal(compute_fdr(make_fdr_tbl(), thpval = 1.0001, thlogfc = 1), 0.01)
})

test_that("compute_fdr returns 1 when no protein is selected", {
  expect_equal(compute_fdr(make_fdr_tbl(), thpval = 10, thlogfc = 1), 1)
})

test_that("compute_fdr returns 1 when every selected protein is excluded by logFC", {
  expect_equal(compute_fdr(make_fdr_tbl(), thpval = 1, thlogfc = 10), 1)
})

test_that("compute_fdr selects all proteins with thresholds at 0", {
  expect_equal(compute_fdr(make_fdr_tbl(), thpval = 0, thlogfc = 0), 0.5)
})

test_that("compute_fdr ignores NA adjusted p-values", {
  tbl <- make_fdr_tbl()
  tbl$Adjusted_PValue[3] <- NA
  
  expect_equal(compute_fdr(tbl, thpval = 1, thlogfc = 1), 0.01)
})

test_that("compute_fdr ignores NA Log_PValue", {
  tbl <- make_fdr_tbl()
  tbl$Log_PValue[1] <- NA
  
  # protein 1 is no longer selected -> only protein 3 remains
  expect_equal(compute_fdr(tbl, thpval = 1, thlogfc = 1), 0.2)
})

test_that("compute_fdr returns -Inf with a warning when all selected adjusted p-values are NA", {
  # Current behaviour: max(numeric(0), na.rm = TRUE) is -Inf
  tbl <- make_fdr_tbl()
  tbl$Adjusted_PValue <- NA_real_
  
  expect_warning(res <- compute_fdr(tbl, thpval = 1, thlogfc = 1))
  expect_identical(res, -Inf)
})





## ----- build_pval_table -----
test_that("build_pval_table returns a data.frame with the expected columns", {
  se <- make_se_pval(5)
  mock_pval_deps(new.env(), make_ht_pval(pval5, logfc5))
  
  res <- build_pval_table(se, "A_vs_B", thlogfc = 1, thpval = 1.5,
                          calibration_method = "langaas",
                          tooltip_info = "Protein")
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 5)
  expect_identical(
    colnames(res),
    c("id", "logFC (A_vs_B)", "P_Value (A_vs_B)", "Log_PValue (A_vs_B)",
      "Adjusted_PValue (A_vs_B)", "isDifferential (A_vs_B)", "Protein")
  )
})

test_that("build_pval_table fills id, logFC, p-values and log p-values", {
  se <- make_se_pval(5)
  mock_pval_deps(new.env(), make_ht_pval(pval5, logfc5))
  
  res <- build_pval_table(se, "A_vs_B", 1, 1.5, "langaas", "Protein")
  
  expect_identical(res$id, paste0("p", 1:5))
  expect_equal(res[["logFC (A_vs_B)"]], c(2, 0.5, -1.5, 3, -2))
  expect_equal(res[["P_Value (A_vs_B)"]], signif(pval5, 4))
  expect_equal(res[["Log_PValue (A_vs_B)"]], signif(-log10(pval5), 4))
})

test_that("build_pval_table flags the differential proteins", {
  # Log_PValue >= 1.5 : 1, 2, 5 | |logFC| >= 1 : 1, 3, 4, 5 -> 1, 5
  se <- make_se_pval(5)
  mock_pval_deps(new.env(), make_ht_pval(pval5, logfc5))
  
  res <- build_pval_table(se, "A_vs_B", thlogfc = 1, thpval = 1.5,
                          "langaas", "Protein")
  
  expect_equal(res[["isDifferential (A_vs_B)"]], c(1, 0, 0, 0, 1))
})

test_that("build_pval_table gives the right p-values to the adjustment function", {
  cap <- new.env()
  se <- make_se_pval(5)
  mock_pval_deps(cap, make_ht_pval(pval5, logfc5))
  
  build_pval_table(se, "A_vs_B", 1, 1.5, "langaas", "Protein")
  
  # Pushed p-value (#4) removed, p-value with logFC under threshold (#2) set to 1
  expect_equal(cap$adj_args[[1]], c(0.001, 1, 0.5, 0.03))
  expect_identical(cap$adj_args[[2]], "langaas")
})

test_that("build_pval_table leaves the adjusted p-value of pushed proteins as NA", {
  se <- make_se_pval(5)
  mock_pval_deps(new.env(), make_ht_pval(pval5, logfc5))
  
  res <- build_pval_table(se, "A_vs_B", 1, 1.5, "langaas", "Protein")
  
  # The mock returns pval / 2 for proteins 1, 2, 3, 5
  expect_equal(res[["Adjusted_PValue (A_vs_B)"]],
               c(0.0005, 0.5, 0.25, NA, 0.015))
})

test_that("build_pval_table keeps the pushed p-value in the table", {
  se <- make_se_pval(5)
  mock_pval_deps(new.env(), make_ht_pval(pval5, logfc5))
  
  res <- build_pval_table(se, "A_vs_B", 1, 1.5, "langaas", "Protein")
  
  # signif(1.00000000001, 4) is 1
  expect_equal(res[["P_Value (A_vs_B)"]][4], 1)
  expect_equal(res[["isDifferential (A_vs_B)"]][4], 0)
})

test_that("build_pval_table works when no p-value is pushed", {
  se <- make_se_pval(3)
  cap <- new.env()
  mock_pval_deps(cap, make_ht_pval(c(0.001, 0.01, 0.5), c(2, 0.5, -1.5)))
  
  res <- build_pval_table(se, "A_vs_B", 1, 1.5, "langaas", "Protein")
  
  # #2 has |logFC| < 1 -> its p-value is set to 1 before adjustment
  expect_equal(cap$adj_args[[1]], c(0.001, 1, 0.5))
  expect_equal(res[["Adjusted_PValue (A_vs_B)"]], c(0.0005, 0.5, 0.25))
  expect_false(anyNA(res[["Adjusted_PValue (A_vs_B)"]]))
})

test_that("build_pval_table does not push p-values when all logFC are above the threshold", {
  se <- make_se_pval(3)
  cap <- new.env()
  mock_pval_deps(cap, make_ht_pval(c(0.001, 0.01, 0.5), c(2, 3, -4)))
  
  build_pval_table(se, "A_vs_B", 1, 1.5, "langaas", "Protein")
  
  expect_equal(cap$adj_args[[1]], c(0.001, 0.01, 0.5))
})

test_that("build_pval_table gives the dataset to HypothesisTest", {
  cap <- new.env()
  se <- make_se_pval(3)
  mock_pval_deps(cap, make_ht_pval(c(0.001, 0.01, 0.5), c(2, 3, -4)))
  
  build_pval_table(se, "A_vs_B", 1, 1.5, "langaas", "Protein")
  
  expect_identical(cap$data, se)
})

test_that("build_pval_table uses the columns of the requested comparison", {
  se <- make_se_pval(3)
  ht <- cbind(
    make_ht_pval(c(0.001, 0.01, 0.5), c(2, 3, 4), "A_vs_B"),
    make_ht_pval(c(0.2, 0.3, 0.4), c(-5, -6, -7), "A_vs_C")
  )
  mock_pval_deps(new.env(), ht)
  
  res <- build_pval_table(se, "A_vs_C", 1, 0.1, "langaas", "Protein")
  
  expect_true("logFC (A_vs_C)" %in% colnames(res))
  expect_false("logFC (A_vs_B)" %in% colnames(res))
  expect_equal(res[["logFC (A_vs_C)"]], c(-5, -6, -7))
  expect_equal(res[["P_Value (A_vs_C)"]], c(0.2, 0.3, 0.4))
})

test_that("build_pval_table rounds logFC to 3 decimals then to 4 significant digits", {
  se <- make_se_pval(3)
  mock_pval_deps(new.env(),
                 make_ht_pval(c(0.001, 0.01, 0.5), c(12.3456, 0.0004, -1.23456)))
  
  res <- build_pval_table(se, "A_vs_B", 1, 1.5, "langaas", "Protein")
  
  expect_equal(res[["logFC (A_vs_B)"]], c(12.35, 0, -1.235))
})

test_that("build_pval_table adds one column for a single tooltip", {
  se <- make_se_pval(3)
  mock_pval_deps(new.env(), make_ht_pval(c(0.001, 0.01, 0.5), c(2, 3, -4)))
  
  res <- build_pval_table(se, "A_vs_B", 1, 1.5, "langaas", "Protein")
  
  expect_identical(colnames(res)[ncol(res)], "Protein")
  expect_identical(res$Protein, paste0("Prot", 1:3))
})

test_that("build_pval_table adds one column per tooltip", {
  se <- make_se_pval(3)
  mock_pval_deps(new.env(), make_ht_pval(c(0.001, 0.01, 0.5), c(2, 3, -4)))
  
  res <- build_pval_table(se, "A_vs_B", 1, 1.5, "langaas",
                          c("Protein", "Description"))
  
  expect_equal(ncol(res), 8)
  expect_identical(tail(colnames(res), 2), c("Protein", "Description"))
  expect_identical(res$Description, paste0("Desc", 1:3))
})

test_that("build_pval_table errors on an unknown tooltip column", {
  se <- make_se_pval(3)
  mock_pval_deps(new.env(), make_ht_pval(c(0.001, 0.01, 0.5), c(2, 3, -4)))
  
  expect_error(
    build_pval_table(se, "A_vs_B", 1, 1.5, "langaas", "unknown_column")
  )
})





## ----- makeDTselectedProt -----
test_that("makeDTselectedProt returns a DT datatable", {
  skip_if_not_installed("DT")
  mock_init_complete()
  
  res <- makeDTselectedProt(make_pval_tbl(), view_adj = TRUE, comparison = "A_vs_B")
  
  expect_s3_class(res, "datatables")
  expect_s3_class(res, "htmlwidget")
})

test_that("makeDTselectedProt with view_adj sorts by adjusted p-value (NA last)", {
  skip_if_not_installed("DT")
  mock_init_complete()
  
  res <- makeDTselectedProt(make_pval_tbl(), view_adj = TRUE, comparison = "A_vs_B")
  
  # Adjusted p-values: p1 = 0.5, p2 = 0.01, p3 = NA, p4 = 0.2
  expect_identical(dt_ids(res), c("p2", "p4", "p1", "p3"))
})

test_that("makeDTselectedProt with view_adj puts differential proteins first among ties", {
  skip_if_not_installed("DT")
  mock_init_complete()
  tbl <- make_pval_tbl()[1:2, ]
  tbl[["Adjusted_PValue (A_vs_B)"]] <- c(0.1, 0.1)
  tbl[["isDifferential (A_vs_B)"]]  <- c(0, 1)
  
  res <- makeDTselectedProt(tbl, view_adj = TRUE, comparison = "A_vs_B")
  
  expect_identical(dt_ids(res), c("p2", "p1"))
})

test_that("makeDTselectedProt with view_adj disables the DT ordering", {
  skip_if_not_installed("DT")
  mock_init_complete()
  
  res <- makeDTselectedProt(make_pval_tbl(), TRUE, "A_vs_B")
  
  expect_false(res$x$options$ordering)
  expect_equal(res$x$options$columnDefs[[1]],
               list(width = "200px", targets = "_all"))
})

test_that("makeDTselectedProt without view_adj keeps the row order and hides two columns", {
  skip_if_not_installed("DT")
  mock_init_complete()
  
  res <- makeDTselectedProt(make_pval_tbl(), view_adj = FALSE, comparison = "A_vs_B")
  
  expect_identical(dt_ids(res), paste0("p", 1:4))
  expect_true(res$x$options$ordering)
  # Log_PValue and Adjusted_PValue are columns 4 and 5 (0-based: 3 and 4)
  expect_equal(
    res$x$options$columnDefs[[1]],
    list(width = "200px", targets = "_all"),
         list(targets = c(3, 4), visible = FALSE)
  )
})

test_that("makeDTselectedProt sets the table options", {
  skip_if_not_installed("DT")
  mock_init_complete()
  
  opts <- makeDTselectedProt(make_pval_tbl(), TRUE, "A_vs_B")$x$options
  
  expect_identical(opts$dom, "frtip")
  expect_equal(opts$pageLength, 100)
  expect_equal(opts$scrollY, 500)
  expect_true(opts$scrollX)
})

test_that("makeDTselectedProt does not show row names", {
  skip_if_not_installed("DT")
  mock_init_complete()
  
  res <- makeDTselectedProt(make_pval_tbl(), TRUE, "A_vs_B")
  
  expect_equal(length(res$x$data), ncol(make_pval_tbl()))
})

test_that("makeDTselectedProt colours the differential rows", {
  skip_if_not_installed("DT")
  mock_init_complete()
  
  res <- makeDTselectedProt(make_pval_tbl(), TRUE, "A_vs_B")
  
  expect_match(paste(deparse(res$x$options), collapse = " "), "E97D5E",
               fixed = TRUE)
})

test_that("makeDTselectedProt errors when the isDifferential column of the comparison is missing", {
  skip_if_not_installed("DT")
  mock_init_complete()
  
  expect_error(makeDTselectedProt(make_pval_tbl(), TRUE, "A_vs_C"))
})





## ----- build_pairwise_comparison_workbook -----
test_that("build_pairwise_comparison_workbook returns a workbook with one sheet", {
  skip_if_not_installed("openxlsx")
  
  wb <- build_pairwise_comparison_workbook(make_pval_tbl(), "A_vs_B")
  
  expect_true(inherits(wb, "Workbook"))
  expect_identical(names(wb), "DA result")
})

test_that("build_pairwise_comparison_workbook writes the table from row 3", {
  skip_if_not_installed("openxlsx")
  tbl <- make_pval_tbl()
  tbl[["Adjusted_PValue (A_vs_B)"]] <- c(0.5, 0.01, 0.3, 0.2)   # no NA
  
  files <- save_and_unzip(build_pairwise_comparison_workbook(tbl, "A_vs_B"))
  res <- openxlsx::read.xlsx(files$xlsx, sheet = 1, startRow = 3,
                             sep.names = " ")
  
  expect_identical(names(res), names(tbl))
  expect_equal(as.data.frame(res), tbl, ignore_attr = TRUE)
})

test_that("build_pairwise_comparison_workbook writes the comparison name above the table", {
  skip_if_not_installed("openxlsx")
  
  files <- save_and_unzip(
    build_pairwise_comparison_workbook(make_pval_tbl(), "A_vs_B")
  )
  top <- openxlsx::read.xlsx(files$xlsx, sheet = 1, rows = 1:2, colNames = FALSE)
  
  expect_true("A_vs_B" %in% unlist(top))
})

test_that("build_pairwise_comparison_workbook converts the comparison to character", {
  skip_if_not_installed("openxlsx")
  
  files <- save_and_unzip(
    build_pairwise_comparison_workbook(make_pval_tbl(), factor("A_vs_B"))
  )
  top <- openxlsx::read.xlsx(files$xlsx, sheet = 1, rows = 1:2, colNames = FALSE)
  
  expect_true("A_vs_B" %in% unlist(top))
})

test_that("build_pairwise_comparison_workbook colours only the differential cells", {
  skip_if_not_installed("openxlsx")
  
  files <- save_and_unzip(
    build_pairwise_comparison_workbook(make_pval_tbl(), "A_vs_B")
  )
  
  # The orange fill is defined in the styles
  styles <- read_xml_file(file.path(files$dir, "xl", "styles.xml"))
  expect_match(styles, "E97D5E", ignore.case = TRUE)
  
  # isDifferential is column 6 (F). Table header = row 3, data from row 4.
  # Differential proteins are the 2nd and 4th -> rows 5 and 7.
  sheet <- read_xml_file(file.path(files$dir, "xl", "worksheets", "sheet1.xml"))
  styled <- function(ref) {
    grepl(sprintf('<c r="%s"[^>]* s="[0-9]+"', ref), sheet)
  }
  
  expect_true(styled("F5"))
  expect_true(styled("F7"))
  expect_false(styled("F4"))
  expect_false(styled("F6"))
})

test_that("build_pairwise_comparison_workbook errors when the isDifferential column is missing", {
  skip_if_not_installed("openxlsx")
  
  expect_error(build_pairwise_comparison_workbook(make_pval_tbl(), "A_vs_C"))
})





## ----- fdrProt -----
test_that("fdrProt returns a list with the history", {
  mock_history_fdr(new.env())
  
  res <- fdrProt(NULL, thpval = 2, FDR = 0.05, nbSignif = 10)
  
  expect_type(res, "list")
  expect_named(res, "history")
})

test_that("fdrProt records 3 entries in the 'FDR' step of 'DA'", {
  cap <- new.env()
  mock_history_fdr(cap)
  
  res <- fdrProt(NULL, 2, 0.05, 10)
  
  expect_length(cap$calls, 3)
  expect_length(res$history, 3)
  for (cl in cap$calls) {
    expect_identical(cl[[1]], "DA")
    expect_identical(cl[[2]], "FDR")
  }
  expect_identical(
    vapply(cap$calls, function(x) x[[3]], character(1)),
    c("th pval", "% FDR", "Nb significant")
  )
})

test_that("fdrProt records the threshold and the number of significant proteins as given", {
  cap <- new.env()
  mock_history_fdr(cap)
  
  fdrProt(NULL, thpval = 2.5, FDR = 0.05, nbSignif = 10)
  
  expect_identical(cap$calls[[1]][[4]], 2.5)
  expect_identical(cap$calls[[3]][[4]], 10)
})

test_that("fdrProt records the FDR as a percentage", {
  cap <- new.env()
  mock_history_fdr(cap)
  
  fdrProt(NULL, 2, 0.05, 10)
  
  expect_equal(cap$calls[[2]][[4]], 5)
})

test_that("fdrProt rounds the FDR percentage to 2 decimals", {
  cap <- new.env()
  mock_history_fdr(cap)
  
  fdrProt(NULL, 2, 0.123456, 10)
  
  expect_equal(cap$calls[[2]][[4]], 12.35)
})

test_that("fdrProt passes the existing history to Add2History", {
  cap <- new.env()
  mock_history_fdr(cap)
  init <- list(previous = "step")
  
  res <- fdrProt(init, 2, 0.05, 10)
  
  expect_identical(cap$first[[1]], init)
  expect_identical(res$history[["previous"]], "step")
  expect_length(res$history, 4)   # previous step + 3 new entries
})

test_that("fdrProt errors when FDR is not numeric", {
  mock_history_fdr(new.env())
  
  expect_error(fdrProt(NULL, 2, "abc", 10))
})
