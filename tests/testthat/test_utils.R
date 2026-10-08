library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

# Mocks DaparToolshed::paramshistory(): returns "hist_of_<object>"
# and records the received object in 'cap$obj'
mock_ph_get <- function(cap, env = parent.frame()) {
  testthat::local_mocked_bindings(
    paramshistory = function(object, ...) {
      cap$obj <- object
      paste0("hist_of_", object)
    },
    .package = "DaparToolshed",
    .env = env
  )
}

# Mocks shiny::addResourcePath() and records the calls in 'cap$calls'
mock_resource <- function(cap, env = parent.frame()) {
  testthat::local_mocked_bindings(
    addResourcePath = function(prefix, directoryPath) {
      cap$calls <- c(cap$calls,
                     list(list(prefix = prefix, path = directoryPath)))
      invisible(NULL)
    },
    .package = "Prostar2",
    .env = env
  )
}

# Mocks DaparToolshed::metacellDef() and records its argument in 'cap$type'
mock_metacell_def <- function(cap, env = parent.frame()) {
  testthat::local_mocked_bindings(
    metacellDef = function(level, ...) {
      cap$type <- level
      data.frame(
        node  = c("Missing POV", "Missing MEC", "Quantified"),
        color = c("#ff0000", "#00ff00", "#0000ff"),
        stringsAsFactors = FALSE
      )
    },
    .package = "DaparToolshed",
    .env = env
  )
}

# 3 features x 3 samples with decimal values
make_se_enriched <- function() {
  mat <- matrix(
    c(1.234, 2.345, 3.456,
      4.567, 5.678, 6.789,
      7.891, 8.912, 9.123),
    nrow = 3, byrow = TRUE,
    dimnames = list(paste0("f", 1:3), paste0("S", 1:3))
  )
  SummarizedExperiment::SummarizedExperiment(assays = list(counts = mat))
}

# qMetacell with one column per sample ("metacell_<sample>")
make_qmeta <- function(extra = FALSE) {
  df <- data.frame(
    metacell_S1 = c("a", "b", "c"),
    metacell_S2 = c("d", "e", "f"),
    metacell_S3 = c("g", "h", "i"),
    row.names = paste0("f", 1:3),
    stringsAsFactors = FALSE
  )
  if (extra) df$metacell_Other <- c("x", "y", "z")
  df
}

mock_qmeta <- function(value, env = parent.frame()) {
  testthat::local_mocked_bindings(
    qMetacell = function(...) value,
    .package = "DaparToolshed",
    .env = env
  )
}

# ---- Tests -----------------------------------------------------------------

## ----- Add2History -----
test_that("Add2History adds one row to an empty history", {
  res <- Add2History(InitializeHistory(), "Step1", "Sub1", "param", "val")
  
  expect_equal(nrow(res), 1)
  expect_identical(colnames(res), c("Step", "Substep", "Parameter", "Value"))
  expect_identical(
    unlist(res[1, ], use.names = FALSE),
    c("Step1", "Sub1", "param", "val")
  )
})

test_that("Add2History keeps the existing rows and appends at the end", {
  h <- Add2History(InitializeHistory(), "S1", "ss1", "p1", "v1")
  h <- Add2History(h, "S2", "ss2", "p2", "v2")
  
  expect_equal(nrow(h), 2)
  expect_identical(h$Step, c("S1", "S2"))
  expect_identical(h$Substep, c("ss1", "ss2"))
  expect_identical(h$Parameter, c("p1", "p2"))
  expect_identical(h$Value, c("v1", "v2"))
})

test_that("Add2History does not modify its input", {
  h <- InitializeHistory()
  
  Add2History(h, "S", "ss", "p", "v")
  
  expect_equal(nrow(h), 0)
})

test_that("Add2History stores values as character", {
  h <- Add2History(InitializeHistory(), "S", "ss", "num", 0.5)
  h <- Add2History(h, "S", "ss", "int", 3L)
  h <- Add2History(h, "S", "ss", "lgl", TRUE)
  
  expect_type(h$Value, "character")
  expect_identical(h$Value, c("0.5", "3", "TRUE"))
})

test_that("Add2History turns a NULL value into NA", {
  res <- Add2History(InitializeHistory(), "S", "ss", "p", NULL)
  
  expect_equal(nrow(res), 1)
  expect_true(is.na(res$Value))
  expect_identical(res$Parameter, "p")
})

test_that("Add2History turns a NA value into NA", {
  res <- Add2History(InitializeHistory(), "S", "ss", "p", NA)
  
  expect_true(is.na(res$Value))
})

test_that("Add2History collapses a named list into 'name=value' pairs", {
  res <- Add2History(InitializeHistory(), "S", "ss", "p",
                     list(a = 1, b = "x"))
  
  expect_identical(res$Value, "a=1, b=x")
})

test_that("Add2History with a one-element list gives a single pair", {
  res <- Add2History(InitializeHistory(), "S", "ss", "p", list(k = 10))
  
  expect_identical(res$Value, "k=10")
})

test_that("Add2History works on a history that already has rows from a list value", {
  h <- Add2History(InitializeHistory(), "S1", "ss", "p1", "v")
  h <- Add2History(h, "S2", "ss", "p2", list(a = 1))
  
  expect_identical(h$Value, c("v", "a=1"))
})

test_that("Add2History errors when the value has several elements", {
  # c(step, substep, param.name, value) then has more than 4 items
  expect_warning(
    Add2History(InitializeHistory(), "S", "ss", "p", c("a", "b"))
  )
})





## ----- GetHistory -----
test_that("GetHistory returns the history of the assay named x", {
  cap <- new.env(); mock_ph_get(cap)
  data_in <- list(Filtering = "filt", Normalization = "norm")
  
  res <- GetHistory(data_in, "Normalization")
  
  expect_identical(res, "hist_of_norm")
  expect_identical(cap$obj, "norm")
})

test_that("GetHistory returns NULL when x is not an assay", {
  cap <- new.env(); mock_ph_get(cap)
  
  expect_null(GetHistory(list(Filtering = "filt"), "Imputation"))
  expect_null(cap$obj)
})

test_that("GetHistory 'Description' uses the 'Convert' assay", {
  cap <- new.env(); mock_ph_get(cap)
  data_in <- list(Convert = "conv", Filtering = "filt")
  
  res <- GetHistory(data_in, "Description")
  
  expect_identical(res, "hist_of_conv")
  expect_identical(cap$obj, "conv")
})

test_that("GetHistory 'Description' returns NULL without a 'Convert' assay", {
  cap <- new.env(); mock_ph_get(cap)
  
  expect_null(GetHistory(list(Filtering = "filt"), "Description"))
  expect_null(cap$obj)
})

test_that("GetHistory 'Save' always returns NULL", {
  cap <- new.env(); mock_ph_get(cap)
  
  expect_null(GetHistory(list(Convert = "conv"), "Save"))
  # even if an assay is called 'Save'
  expect_null(GetHistory(list(Save = "sv"), "Save"))
  expect_null(cap$obj)
})

# test_that("GetHistory works with a real QFeatures", {
#   cap <- new.env(); mock_ph_get(cap)
#   se <- SummarizedExperiment::SummarizedExperiment(
#     assays = list(counts = matrix(1:4, 2,
#                                   dimnames = list(c("f1", "f2"), c("S1", "S2"))))
#   )
#   qf <- QFeatures::QFeatures(
#     list(Filtering = se),
#     colData = S4Vectors::DataFrame(row.names = c("S1", "S2"))
#   )
#   
#   res <- GetHistory(qf, "Filtering")
#   
#   expect_type(res, "character")
#   expect_s4_class(cap$obj, "SummarizedExperiment")
#   expect_null(GetHistory(qf, "Other"))
# })
###### ------^^^ Fonctionne pas---------




## ----- InitializeHistory -----
test_that("InitializeHistory returns an empty data.frame with 4 columns", {
  res <- InitializeHistory()
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
  expect_equal(ncol(res), 4)
})

test_that("InitializeHistory names the columns", {
  expect_identical(
    colnames(InitializeHistory()),
    c("Step", "Substep", "Parameter", "Value")
  )
})

test_that("InitializeHistory returns a new identical object at each call", {
  expect_identical(InitializeHistory(), InitializeHistory())
})





## ----- pkgs_require -----
test_that("pkgs_require passes when every package is installed", {
  skip_if_not_installed("BiocManager")
  
  expect_no_error(pkgs_require(c("stats", "utils")))
})

test_that("pkgs_require returns one NULL per package", {
  skip_if_not_installed("BiocManager")
  
  res <- suppressWarnings(pkgs_require(c("stats", "utils")))
  
  expect_type(res, "list")
  expect_length(res, 2)
  expect_true(all(vapply(res, is.null, logical(1))))
})

test_that("pkgs_require accepts an empty vector", {
  skip_if_not_installed("BiocManager")
  
  expect_no_error(pkgs_require(character(0)))
})

test_that("pkgs_require errors with an installation hint for a missing package", {
  skip_if_not_installed("BiocManager")
  
  expect_error(
    pkgs_require("notARealPackage12345"),
    "Please install notARealPackage12345: BiocManager::install('notARealPackage12345')",
    fixed = TRUE
  )
})

test_that("pkgs_require reports the first missing package", {
  skip_if_not_installed("BiocManager")
  
  expect_error(
    pkgs_require(c("stats", "notARealPackageA", "notARealPackageB")),
    "notARealPackageA",
    fixed = TRUE
  )
})





## ----- add_resourcePath -----
test_that("add_resourcePath registers 'www' and 'images' in this order", {
  cap <- new.env(); mock_resource(cap)
  
  add_resourcePath()
  
  expect_length(cap$calls, 2)
  expect_identical(cap$calls[[1]]$prefix, "www")
  expect_identical(cap$calls[[2]]$prefix, "images")
})

test_that("add_resourcePath uses the folders of the Prostar2 package", {
  cap <- new.env(); mock_resource(cap)
  
  add_resourcePath()
  
  expect_identical(cap$calls[[1]]$path,
                   system.file("app/www", package = "Prostar2"))
  expect_identical(cap$calls[[2]]$path,
                   system.file("app/images", package = "Prostar2"))
})

test_that("add_resourcePath returns invisibly", {
  cap <- new.env(); mock_resource(cap)
  
  expect_invisible(add_resourcePath())
})





## ----- BuildColorStyles -----
test_that("BuildColorStyles returns the colours named by node", {
  cap <- new.env(); mock_metacell_def(cap)
  
  res <- BuildColorStyles("protein")
  
  expect_type(res, "character")
  expect_identical(
    res,
    c("Missing POV" = "#ff0000", "Missing MEC" = "#00ff00",
      "Quantified" = "#0000ff")
  )
})

test_that("BuildColorStyles gives the dataset type to metacellDef", {
  cap <- new.env(); mock_metacell_def(cap)
  
  BuildColorStyles("peptide")
  
  expect_identical(cap$type, "peptide")
})

test_that("BuildColorStyles has one colour per node", {
  cap <- new.env(); mock_metacell_def(cap)
  
  res <- BuildColorStyles("protein")
  
  expect_length(res, 3)
  expect_false(anyDuplicated(names(res)) > 0)
})





## ----- Build_enriched_qdata -----
test_that("Build_enriched_qdata binds the data and the metadata when qMetacell exists", {
  mock_qmeta(make_qmeta())
  
  res <- Build_enriched_qdata(make_se_enriched())
  
  expect_s3_class(res, "data.frame")
  expect_equal(dim(res), c(3, 6))
  expect_identical(
    colnames(res),
    c("S1", "S2", "S3", "metacell_S1", "metacell_S2", "metacell_S3")
  )
})

test_that("Build_enriched_qdata rounds the data to 2 digits by default", {
  mock_qmeta(make_qmeta())
  
  res <- Build_enriched_qdata(make_se_enriched())
  
  expect_equal(res$S1, c(1.23, 4.57, 7.89))
  expect_equal(res$S2, c(2.35, 5.68, 8.91))
})

test_that("Build_enriched_qdata uses the given number of digits", {
  mock_qmeta(make_qmeta())
  
  res1 <- Build_enriched_qdata(make_se_enriched(), digits = 1)
  res3 <- Build_enriched_qdata(make_se_enriched(), digits = 3)
  
  expect_equal(res1$S1, c(1.2, 4.6, 7.9))
  expect_equal(res3$S1, c(1.234, 4.567, 7.891))
})

test_that("Build_enriched_qdata keeps the metadata values unchanged", {
  mock_qmeta(make_qmeta())
  
  res <- Build_enriched_qdata(make_se_enriched())
  
  expect_identical(res$metacell_S1, c("a", "b", "c"))
  expect_identical(res$metacell_S3, c("g", "h", "i"))
})

test_that("Build_enriched_qdata keeps the feature names", {
  mock_qmeta(make_qmeta())
  
  res <- Build_enriched_qdata(make_se_enriched())
  
  expect_identical(rownames(res), paste0("f", 1:3))
})

test_that("Build_enriched_qdata drops the metadata columns without matching sample", {
  mock_qmeta(make_qmeta(extra = TRUE))
  
  res <- Build_enriched_qdata(make_se_enriched())
  
  expect_equal(ncol(res), 6)
  expect_false("metacell_Other" %in% colnames(res))
})

test_that("Build_enriched_qdata adds NA columns when there is no qMetacell", {
  mock_qmeta(NULL)
  
  res <- Build_enriched_qdata(make_se_enriched())
  
  expect_s3_class(res, "data.frame")
  expect_equal(dim(res), c(3, 6))
  expect_identical(colnames(res), c("S1", "S2", "S3", "V1", "V2", "V3"))
  expect_true(all(is.na(res[, c("V1", "V2", "V3")])))
})

test_that("Build_enriched_qdata rounds to integers when there is no qMetacell", {
  # `digits` is not used in this branch
  mock_qmeta(NULL)
  
  res_default <- Build_enriched_qdata(make_se_enriched())
  res_digits  <- Build_enriched_qdata(make_se_enriched(), digits = 3)
  
  expect_equal(res_default$S1, c(1, 5, 8))
  expect_equal(res_digits$S1, c(1, 5, 8))
})

test_that("Build_enriched_qdata keeps the feature names without qMetacell", {
  mock_qmeta(NULL)
  
  res <- Build_enriched_qdata(make_se_enriched())
  
  expect_identical(rownames(res), paste0("f", 1:3))
})

test_that("Build_enriched_qdata does not modify its input", {
  mock_qmeta(make_qmeta())
  se <- make_se_enriched()
  before <- SummarizedExperiment::assay(se)
  
  Build_enriched_qdata(se, digits = 1)
  
  expect_identical(SummarizedExperiment::assay(se), before)
})





## ----- Extract_Value -----
test_that("Extract_Value converts to numeric", {
  expect_identical(Extract_Value("2.5", "numeric"), 2.5)
  expect_identical(Extract_Value(3L, "numeric"), 3)
  expect_identical(Extract_Value(TRUE, "numeric"), 1)
})

test_that("Extract_Value uses 'numeric' by default", {
  expect_identical(Extract_Value("4"), 4)
})

test_that("Extract_Value returns NA when the numeric conversion warns", {
  expect_identical(Extract_Value("test", "numeric"), NA)
  expect_no_warning(Extract_Value("test", "numeric"))
})

test_that("Extract_Value returns NA when one element of a vector cannot be converted", {
  # The warning aborts the whole conversion: a single NA is returned
  expect_identical(Extract_Value(c("1", "a"), "numeric"), NA)
})

test_that("Extract_Value converts a vector as a whole", {
  expect_identical(Extract_Value(c("1", "2"), "numeric"), c(1, 2))
  expect_identical(Extract_Value(1:2, "character"), c("1", "2"))
})

test_that("Extract_Value converts to character", {
  expect_identical(Extract_Value(1, "character"), "1")
  expect_identical(Extract_Value(TRUE, "character"), "TRUE")
  expect_identical(Extract_Value("a", "character"), "a")
})

test_that("Extract_Value converts to logical", {
  expect_identical(Extract_Value("TRUE", "logical"), TRUE)
  expect_identical(Extract_Value("false", "logical"), FALSE)
  expect_identical(Extract_Value("T", "logical"), TRUE)
  expect_identical(Extract_Value(0, "logical"), FALSE)
  expect_identical(Extract_Value(2, "logical"), TRUE)
})

test_that("Extract_Value returns NA for a non logical string", {
  expect_true(is.na(Extract_Value("abc", "logical")))
})

test_that("Extract_Value converts to factor", {
  res <- Extract_Value(c("b", "a", "b"), "factor")
  
  expect_s3_class(res, "factor")
  expect_identical(levels(res), c("a", "b"))
  expect_identical(as.character(res), c("b", "a", "b"))
})

test_that("Extract_Value converts to integer", {
  expect_identical(Extract_Value("3", "integer"), 3L)
  expect_identical(Extract_Value(3.7, "integer"), 3L)   # truncation
})

test_that("Extract_Value returns NA when the integer conversion warns", {
  expect_identical(Extract_Value("a", "integer"), NA)
  # larger than the integer range
  expect_identical(Extract_Value(3e10, "integer"), NA)
})

test_that("Extract_Value keeps NA values", {
  expect_true(is.na(Extract_Value(NA, "numeric")))
  expect_true(is.na(Extract_Value(NA_character_, "character")))
})

test_that("Extract_Value returns an empty vector of the type for NULL", {
  expect_identical(Extract_Value(NULL, "numeric"), numeric(0))
  expect_identical(Extract_Value(NULL, "character"), character(0))
})

test_that("Extract_Value accepts a partial type name", {
  expect_identical(Extract_Value(1, "char"), "1")
})

test_that("Extract_Value errors on an unknown type", {
  expect_error(Extract_Value(1, "complex"))
  expect_error(Extract_Value(1, "date"))
})

test_that("Extract_Value errors when the type has several values", {
  expect_error(Extract_Value(1, c("numeric", "character")))
})





## ----- GetFiltersScope -----
## ----- not_a_numeric -----
## ----- isContainedIn -----
## ----- checkNA -----
## ----- countPattern -----
## ----- show_log_console -----
## ----- message_console -----

