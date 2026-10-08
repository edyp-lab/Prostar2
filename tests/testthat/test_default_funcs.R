library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

# QFeatures with 3 assays (a1, a2, a3), each with 3 rows x 4 samples
make_qf3 <- function() {
  coldata <- S4Vectors::DataFrame(
    Condition = c("A", "A", "B", "B"),
    row.names = paste0("S", 1:4)
  )
  make_se <- function(offset) {
    SummarizedExperiment::SummarizedExperiment(
      assays = list(counts = matrix(
        seq_len(12) + offset, nrow = 3,
        dimnames = list(paste0("f", 1:3), paste0("S", 1:4))
      ))
    )
  }
  QFeatures::QFeatures(
    list(a1 = make_se(0), a2 = make_se(100), a3 = make_se(200)),
    colData = coldata
  )
}

# SummarizedExperiment compatible with the colData of make_qf3()
make_new_se <- function() {
  SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = matrix(
      seq_len(8) + 1000, nrow = 2,
      dimnames = list(paste0("n", 1:2), paste0("S", 1:4))
    ))
  )
}

# Calls keepDatasets() and muffles only the harmless MultiAssayExperiment
# "'experiments' dropped" warning raised when assays are removed
keep_quiet <- function(...) {
  withCallingHandlers(
    keepDatasets(...),
    warning = function(w) {
      if (grepl("experiments' dropped", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# ---- Tests -----------------------------------------------------------------

## ----- addDatasets -----
test_that("addDatasets adds a new assay with the given name", {
  qf <- make_qf3()
  se <- make_new_se()
  
  res <- addDatasets(qf, se, "new")
  
  expect_s4_class(res, "QFeatures")
  expect_length(res, 4)
  expect_identical(names(res), c("a1", "a2", "a3", "new"))
})

test_that("addDatasets keeps the content of the added dataset", {
  qf <- make_qf3()
  se <- make_new_se()
  
  res <- addDatasets(qf, se, "new")
  
  expect_equal(
    SummarizedExperiment::assay(res[["new"]]),
    SummarizedExperiment::assay(se)
  )
})

test_that("addDatasets does not modify the existing assays", {
  qf <- make_qf3()
  
  res <- addDatasets(qf, make_new_se(), "new")
  
  for (nm in names(qf)) {
    expect_equal(
      SummarizedExperiment::assay(res[[nm]]),
      SummarizedExperiment::assay(qf[[nm]])
    )
  }
})

test_that("addDatasets does not modify the input object", {
  qf <- make_qf3()
  
  addDatasets(qf, make_new_se(), "new")
  
  expect_length(qf, 3)
  expect_identical(names(qf), c("a1", "a2", "a3"))
})

test_that("addDatasets stops silently when object is not a QFeatures", {
  # shiny::req() raises a 'shiny.silent.error' when its condition is FALSE
  expect_error(
    addDatasets(list(a = 1), make_new_se(), "new"),
    class = "shiny.silent.error"
  )
  expect_error(
    addDatasets(NULL, make_new_se(), "new"),
    class = "shiny.silent.error"
  )
})

test_that("addDatasets stops silently when dataset is not a SummarizedExperiment", {
  qf <- make_qf3()
  
  expect_error(
    addDatasets(qf, matrix(1:4, 2), "new"),
    class = "shiny.silent.error"
  )
  expect_error(
    addDatasets(qf, NULL, "new"),
    class = "shiny.silent.error"
  )
})





## ----- keepDatasets -----
test_that("keepDatasets keeps a single assay", {
  qf <- make_qf3()
  
  res <- keep_quiet(qf, 2)
  
  expect_s4_class(res, "QFeatures")
  expect_length(res, 1)
  expect_identical(names(res), "a2")
})

test_that("keepDatasets keeps several assays", {
  qf <- make_qf3()
  
  res <- keep_quiet(qf, c(1, 3))
  
  expect_length(res, 2)
  expect_identical(names(res), c("a1", "a3"))
})

test_that("keepDatasets keeps a contiguous range", {
  qf <- make_qf3()
  
  res <- keep_quiet(qf, 1:2)
  
  expect_identical(names(res), c("a1", "a2"))
})

test_that("keepDatasets removes nothing when all assays are kept", {
  qf <- make_qf3()
  
  res <- keep_quiet(qf, 1:3)   # no removal, so no warning, but harmless
  
  expect_length(res, 3)
  expect_identical(names(res), names(qf))
})

test_that("keepDatasets does not alter the content of the kept assays", {
  qf <- make_qf3()
  
  res <- keep_quiet(qf, c(1, 3))
  
  expect_equal(
    SummarizedExperiment::assay(res[["a1"]]),
    SummarizedExperiment::assay(qf[["a1"]])
  )
  expect_equal(
    SummarizedExperiment::assay(res[["a3"]]),
    SummarizedExperiment::assay(qf[["a3"]])
  )
})

test_that("keepDatasets does not modify the input object", {
  qf <- make_qf3()
  
  keep_quiet(qf, 1)
  
  expect_length(qf, 3)
})

test_that("keepDatasets tolerates duplicated indices in range", {
  qf <- make_qf3()
  
  res <- keep_quiet(qf, c(2, 2))
  
  expect_identical(names(res), "a2")
})
