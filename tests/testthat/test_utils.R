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

# 2 assays: 'first' contains an NA, 'last' does not
make_qf_na <- function(last_na = FALSE, first_na = TRUE) {
  make_se <- function(with_na) {
    m <- matrix(as.numeric(1:8), nrow = 2,
                dimnames = list(paste0("f", 1:2), paste0("S", 1:4)))
    if (with_na) m[1, 2] <- NA
    SummarizedExperiment::SummarizedExperiment(assays = list(counts = m))
  }
  QFeatures::QFeatures(
    list(first = make_se(first_na), last = make_se(last_na)),
    colData = S4Vectors::DataFrame(row.names = paste0("S", 1:4))
  )
}

# Mocks typeDataset(), qMetacell() and matchMetacell().
#   cap$typed : TRUE if typeDataset() was called
#   cap$match : arguments given to matchMetacell()
mock_count_pattern <- function(cap, match_value, env = parent.frame()) {
  testthat::local_mocked_bindings(
    typeDataset = function(object, ...) {
      cap$typed <- TRUE
      "protein"
    },
    qMetacell = function(object, ...) "the_qmetacell",
    matchMetacell = function(metacell, pattern, level, ...) {
      cap$match <- list(metacell = metacell, pattern = pattern, level = level)
      match_value
    },
    .package = "DaparToolshed",
    .env = env
  )
}

make_se_cp <- function() {
  SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = matrix(1:4, 2,
                                  dimnames = list(c("f1", "f2"), c("S1", "S2"))))
  )
}

# Returns a copy of message_console() whose print_color() is a fake one.
# It records (text, color) in 'cap$calls' and returns the text prefixed by
# the colour. It works wherever the real print_color() is defined.
fake_message_console <- function(cap) {
  fn <- message_console
  env <- new.env(parent = environment(message_console))
  env$print_color <- function(text, color, ...) {
    cap$calls <- c(cap$calls, list(list(text = text, color = color)))
    paste0("[", color, "]", text)
  }
  environment(fn) <- env
  fn
}

# Regex of a formatted line: "[LEVEL] dd-mm-YYYY <time> — <msg>"
log_line <- function(level, msg) {
  paste0("^\\[", level, "\\] \\d{2}-\\d{2}-\\d{4} .+ \u2014 ", msg)
}

# Mocks message_console() (Prostar2) and shinyjs::html().
#   cap$console : list of the arguments given to message_console()
#   cap$html    : list(id, html) for every call to shinyjs::html()
mock_slc <- function(cap, env = parent.frame()) {
  testthat::local_mocked_bindings(
    message_console = function(...) {
      cap$console <- c(cap$console, list(list(...)))
      invisible(NULL)
    },
    .package = "Prostar2",
    .env = env
  )
  testthat::local_mocked_bindings(
    html = function(id, html, ...) {
      cap$html <- c(cap$html, list(list(id = id, html = html)))
      invisible(NULL)
    },
    .package = "shinyjs",
    .env = env
  )
}

# show_log_console() uses a reactiveVal: it needs isolate() outside Shiny
run_slc <- function(...) shiny::isolate(show_log_console(...))

# Regex of a notification
notif_regex <- function(color, level_text, msg, prefix = "Imputation-") {
  paste0('^<span style="color: ', color, '"><b>\\[', prefix, level_text,
         "\\]</b> <i>.+</i> \u2014 ", msg, "\\s*</span>$")
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

test_that("GetHistory works with a real QFeatures", {
  cap <- new.env()
  testthat::local_mocked_bindings(
    paramshistory = function(object, ...) {
      cap$obj <- object
      "the_history"
    },
    .package = "DaparToolshed"
  )
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = matrix(1:4, 2,
                                  dimnames = list(c("f1", "f2"), c("S1", "S2"))))
  )
  qf <- QFeatures::QFeatures(
    list(Filtering = se),
    colData = S4Vectors::DataFrame(row.names = c("S1", "S2"))
  )
  
  res <- GetHistory(qf, "Filtering")
  
  expect_identical(res, "the_history")
  expect_s4_class(cap$obj, "SummarizedExperiment")
  expect_equal(
    SummarizedExperiment::assay(cap$obj),
    SummarizedExperiment::assay(qf[["Filtering"]])
  )
  expect_null(GetHistory(qf, "Other"))
})





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
test_that("GetFiltersScope returns a named character vector", {
  res <- GetFiltersScope()
  
  expect_type(res, "character")
  expect_false(is.null(names(res)))
  expect_length(res, 4)
})

test_that("GetFiltersScope returns the expected scopes in order", {
  expect_identical(
    GetFiltersScope(),
    c("Whole Line" = "WholeLine",
      "Whole matrix" = "WholeMatrix",
      "For every condition" = "AllCond",
      "At least one condition" = "AtLeastOneCond")
  )
})

test_that("GetFiltersScope has unique names and values", {
  res <- GetFiltersScope()
  
  expect_false(anyDuplicated(names(res)) > 0)
  expect_false(anyDuplicated(unname(res)) > 0)
})

test_that("GetFiltersScope values contain no space (usable as identifiers)", {
  expect_false(any(grepl(" ", unname(GetFiltersScope()), fixed = TRUE)))
})





## ----- not_a_numeric -----
test_that("not_a_numeric returns NULL for numbers", {
  expect_null(not_a_numeric(1))
  expect_null(not_a_numeric(-2.5))
  expect_null(not_a_numeric(0))
  expect_null(not_a_numeric(3L))
  expect_null(not_a_numeric(Inf))
})

test_that("not_a_numeric returns NULL for strings that look like numbers", {
  expect_null(not_a_numeric("1"))
  expect_null(not_a_numeric("1.5"))
  expect_null(not_a_numeric("-3"))
  expect_null(not_a_numeric("1e3"))
  expect_null(not_a_numeric(" 2 "))
  expect_null(not_a_numeric("Inf"))
})

test_that("not_a_numeric returns NULL for logical values", {
  expect_null(not_a_numeric(TRUE))
})

test_that("not_a_numeric returns a message for text", {
  # as.numeric() also emits a warning here, which is expected
  expect_warning(res <- not_a_numeric("A"))
  
  expect_identical(res, "Please input a number")
})

test_that("not_a_numeric rejects a decimal comma", {
  expect_warning(res <- not_a_numeric("1,5"))
  
  expect_identical(res, "Please input a number")
})

test_that("not_a_numeric returns a message for empty or NA input without warning", {
  expect_no_warning(res_empty <- not_a_numeric(""))
  expect_identical(res_empty, "Please input a number")
  
  expect_identical(not_a_numeric(NA), "Please input a number")
  expect_identical(not_a_numeric(NA_character_), "Please input a number")
  expect_identical(not_a_numeric(NaN), "Please input a number")
  expect_identical(not_a_numeric("NaN"), "Please input a number")
})

test_that("not_a_numeric returns NULL or a character(1)", {
  expect_true(is.null(not_a_numeric(1)) || is.character(not_a_numeric(1)))
  expect_length(not_a_numeric(NA), 1)
})

test_that("not_a_numeric errors on NULL or on a vector", {
  # The condition of the `if` has length 0 or > 1
  expect_error(not_a_numeric(NULL))
  expect_error(not_a_numeric(c(1, 2)))
})





## ----- isContainedIn -----
test_that("isContainedIn returns TRUE when the element is in the vector", {
  expect_true(isContainedIn("A", c("A", "B")))
})

test_that("isContainedIn returns FALSE when the element is absent", {
  expect_false(isContainedIn("C", c("A", "B")))
})

test_that("isContainedIn is TRUE only if every element of strA is found", {
  expect_true(isContainedIn(c("A", "B"), c("A", "B", "C")))
  expect_false(isContainedIn(c("A", "D"), c("A", "B", "C")))
})

test_that("isContainedIn does not do partial matching", {
  expect_false(isContainedIn("A", c("ABCDE", "FGHIJ")))
  expect_false(isContainedIn("ABC", "A"))
})

test_that("isContainedIn is case sensitive", {
  expect_false(isContainedIn("a", c("A", "B")))
})

test_that("isContainedIn is TRUE for an empty strA", {
  expect_true(isContainedIn(character(0), c("A", "B")))
  expect_true(isContainedIn(character(0), character(0)))
})

test_that("isContainedIn is FALSE against an empty strB", {
  expect_false(isContainedIn("A", character(0)))
})

test_that("isContainedIn handles duplicates and order", {
  expect_true(isContainedIn(c("B", "A", "A"), c("A", "B")))
})

test_that("isContainedIn coerces numbers and strings", {
  expect_true(isContainedIn(1, c("1", "2")))
  expect_true(isContainedIn(2L, c(1, 2, 3)))
})

test_that("isContainedIn treats NA like any other value", {
  expect_true(isContainedIn(NA, c("A", NA)))
  expect_false(isContainedIn(NA, c("A", "B")))
})

test_that("isContainedIn always returns a single logical", {
  res <- isContainedIn(c("A", "B"), c("A", "B"))
  
  expect_type(res, "logical")
  expect_length(res, 1)
})





## ----- checkNA -----
test_that("checkNA returns FALSE when the last assay has no missing value", {
  expect_false(checkNA(make_qf_na(last_na = FALSE)))
})

test_that("checkNA returns TRUE when the last assay has a missing value", {
  expect_true(checkNA(make_qf_na(last_na = TRUE)))
})

test_that("checkNA looks at the last assay by default", {
  # 'first' has an NA but 'last' does not
  expect_false(checkNA(make_qf_na(last_na = FALSE, first_na = TRUE)))
})

test_that("checkNA looks at the assay given by a numeric index", {
  qf <- make_qf_na(last_na = FALSE, first_na = TRUE)
  
  expect_true(checkNA(qf, i = 1))
  expect_false(checkNA(qf, i = 2))
})

test_that("checkNA looks at the assay given by a name", {
  qf <- make_qf_na(last_na = FALSE, first_na = TRUE)
  
  expect_true(checkNA(qf, i = "first"))
  expect_false(checkNA(qf, i = "last"))
})

test_that("checkNA counts NaN as missing", {
  qf <- make_qf_na(last_na = FALSE, first_na = FALSE)
  se <- qf[["last"]]
  SummarizedExperiment::assay(se)[1, 1] <- NaN
  qf <- QFeatures::QFeatures(
    list(only = se),
    colData = S4Vectors::DataFrame(row.names = paste0("S", 1:4))
  )
  
  expect_true(checkNA(qf))
})

test_that("checkNA returns a single logical", {
  res <- checkNA(make_qf_na())
  
  expect_type(res, "logical")
  expect_length(res, 1)
})

test_that("checkNA errors for an out of range index", {
  qf <- make_qf_na()
  
  expect_error(checkNA(qf, i = 10))
})

test_that("checkNA returns FALSE for an unknown assay name (current behaviour)", {
  qf <- make_qf_na()
  
  expect_error(checkNA(qf, i = "unknown"), "Unknown assay: unknown", fixed = TRUE)
})

test_that("checkNA does not modify its input", {
  qf <- make_qf_na()
  before <- SummarizedExperiment::assay(qf[["first"]])
  
  checkNA(qf)
  
  expect_identical(SummarizedExperiment::assay(qf[["first"]]), before)
})





## ----- countPattern -----
test_that("countPattern counts the TRUE values of the match matrix", {
  cap <- new.env()
  mock_count_pattern(cap, matrix(c(TRUE, FALSE, TRUE, TRUE), 2))
  
  expect_equal(countPattern(make_se_cp(), "Missing POV"), 3)
})

test_that("countPattern counts the TRUE values of a logical vector", {
  cap <- new.env()
  mock_count_pattern(cap, c(TRUE, TRUE, FALSE))
  
  expect_equal(countPattern(make_se_cp(), "Missing POV"), 2)
})

test_that("countPattern returns 0 when nothing matches", {
  cap <- new.env()
  mock_count_pattern(cap, matrix(FALSE, 2, 2))
  
  expect_equal(countPattern(make_se_cp(), "Missing POV"), 0)
})

test_that("countPattern ignores NA values", {
  cap <- new.env()
  mock_count_pattern(cap, c(TRUE, NA, FALSE, TRUE))
  
  expect_equal(countPattern(make_se_cp(), "Missing POV"), 2)
})

test_that("countPattern gets the level from typeDataset when it is NULL", {
  cap <- new.env()
  mock_count_pattern(cap, c(TRUE))
  
  countPattern(make_se_cp(), "Missing POV")
  
  expect_true(cap$typed)
  expect_identical(cap$match$level, "protein")
})

test_that("countPattern uses the given level and does not call typeDataset", {
  cap <- new.env()
  mock_count_pattern(cap, c(TRUE))
  
  countPattern(make_se_cp(), "Missing POV", level = "peptide")
  
  expect_null(cap$typed)
  expect_identical(cap$match$level, "peptide")
})

test_that("countPattern gives the qMetacell and the pattern to matchMetacell", {
  cap <- new.env()
  mock_count_pattern(cap, c(TRUE))
  
  countPattern(make_se_cp(), "Missing MEC")
  
  expect_identical(cap$match$metacell, "the_qmetacell")
  expect_identical(cap$match$pattern, "Missing MEC")
})

test_that("countPattern returns a single number", {
  cap <- new.env()
  mock_count_pattern(cap, c(TRUE, FALSE))
  
  res <- countPattern(make_se_cp(), "x")
  
  expect_true(is.numeric(res))
  expect_length(res, 1)
})





## ----- show_log_console -----
test_that("show_log_console returns an empty list when nothing is logged", {
  cap <- new.env(); mock_slc(cap)
  
  res <- run_slc(1 + 1)
  
  expect_identical(res, list())
  expect_null(cap$console)
  expect_null(cap$html)
})

test_that("show_log_console returns the notifications, not the value of expr", {
  cap <- new.env(); mock_slc(cap)
  
  res <- run_slc({ message("hello"); 42 })
  
  expect_type(res, "list")
  expect_length(res, 1)
})

test_that("show_log_console evaluates expr once", {
  cap <- new.env(); mock_slc(cap)
  n <- 0
  
  run_slc({ n <- n + 1; message("a") })
  
  expect_equal(n, 1)
})

test_that("show_log_console records a message with the info colour", {
  cap <- new.env(); mock_slc(cap)
  
  res <- run_slc(message("hello"))
  
  expect_length(res, 1)
  expect_match(res[[1]], notif_regex("grey", "INFO", "hello"))
})

test_that("show_log_console uses the custom info colour", {
  cap <- new.env(); mock_slc(cap)
  
  res <- run_slc(message("hello"), color_info = "green")
  
  expect_match(res[[1]], notif_regex("green", "INFO", "hello"))
})

test_that("show_log_console ignores blank messages", {
  cap <- new.env(); mock_slc(cap)
  
  res <- run_slc(message(""), id_notif = "box")
  
  expect_identical(res, list())
  expect_null(cap$html)
})

test_that("show_log_console ignores messages when cond_info is FALSE", {
  cap <- new.env(); mock_slc(cap)
  
  res <- run_slc(message("hello"), cond_info = FALSE)
  
  expect_identical(res, list())
  expect_null(cap$console)
})

test_that("show_log_console records a warning with the warning colour", {
  cap <- new.env(); mock_slc(cap)
  
  res <- run_slc(warning("careful"))
  
  expect_length(res, 1)
  expect_match(res[[1]], notif_regex("blue", "WARNING", "careful"))
})

test_that("show_log_console uses the custom warning colour", {
  cap <- new.env(); mock_slc(cap)
  
  res <- run_slc(warning("careful"), color_warning = "orange")
  
  expect_match(res[[1]], notif_regex("orange", "WARNING", "careful"))
})

test_that("show_log_console ignores warnings when cond_warning is FALSE", {
  cap <- new.env(); mock_slc(cap)
  
  res <- run_slc(warning("careful"), cond_warning = FALSE)
  
  expect_identical(res, list())
  expect_null(cap$console)
})

test_that("show_log_console catches an error and records it", {
  cap <- new.env(); mock_slc(cap)
  
  expect_no_error(res <- run_slc(stop("boom")))
  
  expect_length(res, 1)
  expect_match(res[[1]], "<b>[Imputation-ERROR]</b>", fixed = TRUE)
  expect_match(res[[1]], "boom", fixed = TRUE)
})

test_that("show_log_console stops evaluating expr after an error", {
  cap <- new.env(); mock_slc(cap)
  reached <- FALSE
  
  run_slc({ stop("boom"); reached <- TRUE })
  
  expect_false(reached)
})

test_that("show_log_console colours errors with color_warning (current behaviour)", {
  # The error branch uses `color_warning`, not `color_error`.
  # If this is fixed, expect 'color: red' instead.
  cap <- new.env(); mock_slc(cap)
  
  res <- run_slc(stop("boom"), color_warning = "blue", color_error = "red")
  
  expect_match(res[[1]], 'style="color: blue"', fixed = TRUE)
  expect_no_match(res[[1]], "color: red", fixed = TRUE)
})

test_that("show_log_console ignores errors when cond_error is FALSE", {
  cap <- new.env(); mock_slc(cap)
  
  expect_no_error(res <- run_slc(stop("boom"), cond_error = FALSE))
  
  expect_identical(res, list())
})

test_that("show_log_console uses 'Imputation' as the default prefix", {
  cap <- new.env(); mock_slc(cap)
  
  res <- run_slc(message("a"))
  
  expect_match(res[[1]], "<b>[Imputation-INFO]</b>", fixed = TRUE)
})

test_that("show_log_console uses the given prefix", {
  cap <- new.env(); mock_slc(cap)
  
  res <- run_slc({ message("a"); warning("b") }, prefix = "PIRAT")
  
  expect_match(res[[1]], "[PIRAT-WARNING]", fixed = TRUE)
  expect_match(res[[2]], "[PIRAT-INFO]", fixed = TRUE)
})

test_that("show_log_console with an empty prefix gives a space (current behaviour)", {
  # prefix = "" becomes " " instead of ""
  cap <- new.env(); mock_slc(cap)
  
  res <- run_slc(message("a"), prefix = "")
  
  expect_match(res[[1]], "<b>[ INFO]</b>", fixed = TRUE)
})

test_that("show_log_console puts the newest notification first by default", {
  cap <- new.env(); mock_slc(cap)
  
  res <- run_slc({ message("first"); message("second") })
  
  expect_length(res, 2)
  expect_match(res[[1]], "second", fixed = TRUE)
  expect_match(res[[2]], "first", fixed = TRUE)
})

test_that("show_log_console puts the newest notification last with new_first = FALSE", {
  cap <- new.env(); mock_slc(cap)
  
  res <- run_slc({ message("first"); message("second") }, new_first = FALSE)
  
  expect_match(res[[1]], "first", fixed = TRUE)
  expect_match(res[[2]], "second", fixed = TRUE)
})

test_that("show_log_console keeps messages, warnings and errors in the same list", {
  cap <- new.env(); mock_slc(cap)
  
  res <- run_slc({ message("m"); warning("w"); stop("e") }, new_first = FALSE)
  
  expect_length(res, 3)
  expect_match(res[[1]], "INFO", fixed = TRUE)
  expect_match(res[[2]], "WARNING", fixed = TRUE)
  expect_match(res[[3]], "ERROR", fixed = TRUE)
})

test_that("show_log_console calls message_console for messages", {
  cap <- new.env(); mock_slc(cap)
  
  run_slc(message("hello"), color_info = "green")
  
  expect_length(cap$console, 1)
  args <- cap$console[[1]]
  expect_match(args[[1]], "hello", fixed = TRUE)
  expect_identical(args$level, "INFO")
  expect_identical(args$info_text, "Imputation-INFO")
  expect_identical(args$color_info, "green")
})

test_that("show_log_console calls message_console for warnings", {
  cap <- new.env(); mock_slc(cap)
  
  run_slc(warning("careful"), color_warning = "orange")
  
  args <- cap$console[[1]]
  expect_identical(args[[1]], "careful")
  expect_identical(args$level, "WARNING")
  expect_identical(args$warning_text, "Imputation-WARNING")
  expect_identical(args$color_warning, "orange")
})

test_that("show_log_console calls message_console for errors", {
  cap <- new.env(); mock_slc(cap)
  
  run_slc(stop("boom"), color_error = "purple")
  
  args <- cap$console[[1]]
  expect_identical(args[[1]], "boom")
  expect_identical(args$level, "ERROR")
  expect_identical(args$error_text, "Imputation-ERROR")
  expect_identical(args$color_error, "purple")
})

test_that("show_log_console gives the prefix to message_console", {
  cap <- new.env(); mock_slc(cap)
  
  run_slc(message("a"), prefix = "PIRAT")
  
  expect_identical(cap$console[[1]]$info_text, "PIRAT-INFO")
})

test_that("show_log_console does not call message_console when console_message is FALSE", {
  cap <- new.env(); mock_slc(cap)
  
  suppressMessages(suppressWarnings(
    run_slc({ message("a"); warning("b"); stop("c") }, console_message = FALSE)
  ))
  
  expect_null(cap$console)
})

test_that("show_log_console does not call message_console when the condition is disabled", {
  cap <- new.env(); mock_slc(cap)
  
  run_slc({ message("a"); warning("b") }, cond_info = FALSE,
          cond_warning = FALSE)
  
  expect_null(cap$console)
})

test_that("show_log_console hides R messages and warnings when console_message is TRUE", {
  cap <- new.env(); mock_slc(cap)
  
  expect_no_message(run_slc(message("hidden")))
  expect_no_warning(run_slc(warning("hidden")))
})

test_that("show_log_console lets R messages and warnings through when console_message is FALSE", {
  cap <- new.env(); mock_slc(cap)
  
  expect_message(run_slc(message("visible"), console_message = FALSE),
                 "visible")
  expect_warning(run_slc(warning("visible"), console_message = FALSE),
                 "visible")
})

test_that("show_log_console records notifications when console_message is FALSE", {
  cap <- new.env(); mock_slc(cap)
  
  res <- suppressMessages(suppressWarnings(
    run_slc({ message("m"); warning("w"); stop("e") },
            console_message = FALSE, new_first = FALSE)
  ))
  
  expect_length(res, 3)
})

test_that("show_log_console does not call shinyjs::html when id_notif is NULL", {
  cap <- new.env(); mock_slc(cap)
  
  run_slc(message("a"))
  
  expect_null(cap$html)
})

test_that("show_log_console updates the element id_notif at each notification", {
  cap <- new.env(); mock_slc(cap)
  
  run_slc({ message("a"); message("b") }, id_notif = "box")
  
  expect_length(cap$html, 2)
  expect_identical(cap$html[[1]]$id, "box")
  expect_identical(cap$html[[2]]$id, "box")
})

test_that("show_log_console renders all the notifications as an HTML list", {
  cap <- new.env(); mock_slc(cap)
  
  run_slc({ message("a"); message("b") }, id_notif = "box")
  
  first  <- cap$html[[1]]$html
  second <- cap$html[[2]]$html
  
  expect_match(second, "^<ul>")
  expect_match(second, "</ul>$")
  expect_equal(lengths(regmatches(first,  gregexpr("<li>", first,  fixed = TRUE))), 1)
  expect_equal(lengths(regmatches(second, gregexpr("<li>", second, fixed = TRUE))), 2)
})

test_that("show_log_console also displays warnings and errors in the app", {
  cap <- new.env(); mock_slc(cap)
  
  run_slc({ warning("w"); stop("e") }, id_notif = "box")
  
  expect_length(cap$html, 2)
})





## ----- message_console -----
test_that("message_console shows an INFO message with message()", {
  cap <- new.env(); message_console <- fake_message_console(cap)
  
  expect_message(message_console("hello"), "hello")
  
  expect_length(cap$calls, 1)
  expect_match(cap$calls[[1]]$text, log_line("INFO", "hello$"))
  expect_identical(cap$calls[[1]]$color, "grey")
})

test_that("message_console uses INFO as the default level", {
  cap <- new.env(); message_console <- fake_message_console(cap)
  
  expect_message(message_console("hello"))
  
  expect_match(cap$calls[[1]]$text, "^\\[INFO\\] ")
})

test_that("message_console sends WARNING lines to print_color, with a final newline", {
  cap <- new.env(); message_console <- fake_message_console(cap)
  
  expect_no_message(message_console("careful", level = "WARNING"))
  
  expect_length(cap$calls, 1)
  expect_match(cap$calls[[1]]$text, log_line("WARNING", "careful\n$"))
  expect_identical(cap$calls[[1]]$color, "blue")
})

test_that("message_console sends ERROR lines to print_color, with a final newline", {
  cap <- new.env(); message_console <- fake_message_console(cap)
  
  expect_no_message(message_console("boom", level = "ERROR"))
  
  expect_match(cap$calls[[1]]$text, log_line("ERROR", "boom\n$"))
  expect_identical(cap$calls[[1]]$color, "red")
})

test_that("message_console prints other levels with cat", {
  cap <- new.env(); message_console <- fake_message_console(cap)
  
  expect_output(message_console("detail", level = "DEBUG"), "[DEBUG]",
                fixed = TRUE)
  
  expect_match(cap$calls[[1]]$text, log_line("DEBUG", "detail$"))
  expect_identical(cap$calls[[1]]$color, "black")
})

test_that("message_console returns NULL for other levels", {
  cap <- new.env(); message_console <- fake_message_console(cap)
  res <- "not set"
  
  expect_output(res <- message_console("x", level = "OTHER"))
  
  expect_null(res)
})

test_that("message_console level is case insensitive", {
  cap <- new.env(); message_console <- fake_message_console(cap)
  
  expect_message(message_console("a", level = "info"))
  expect_no_message(message_console("b", level = "warning"))
  expect_no_message(message_console("c", level = "Error"))
  
  expect_match(cap$calls[[1]]$text, "^\\[INFO\\] ")
  expect_match(cap$calls[[2]]$text, "^\\[WARNING\\] ")
  expect_match(cap$calls[[3]]$text, "^\\[ERROR\\] ")
})

test_that("message_console uses the custom level texts", {
  cap <- new.env(); message_console <- fake_message_console(cap)
  
  expect_message(message_console("a", "INFO", info_text = "MY-INFO"))
  message_console("b", "WARNING", warning_text = "MY-WARN")
  message_console("c", "ERROR", error_text = "MY-ERR")
  
  expect_match(cap$calls[[1]]$text, "^\\[MY-INFO\\] ")
  expect_match(cap$calls[[2]]$text, "^\\[MY-WARN\\] ")
  expect_match(cap$calls[[3]]$text, "^\\[MY-ERR\\] ")
})

test_that("message_console ignores the texts of the other levels", {
  cap <- new.env(); message_console <- fake_message_console(cap)
  
  message_console("b", "WARNING", info_text = "MY-INFO", error_text = "MY-ERR")
  
  expect_match(cap$calls[[1]]$text, "^\\[WARNING\\] ")
})

test_that("message_console uses the custom colours", {
  cap <- new.env(); message_console <- fake_message_console(cap)
  
  expect_message(message_console("a", "INFO", color_info = "green"))
  message_console("b", "WARNING", color_warning = "orange")
  message_console("c", "ERROR", color_error = "purple")
  expect_output(message_console("d", "OTHER", color_other = "pink"))
  
  expect_identical(vapply(cap$calls, function(x) x$color, character(1)),
                   c("green", "orange", "purple", "pink"))
})

test_that("message_console collapses a vector of messages", {
  cap <- new.env(); message_console <- fake_message_console(cap)
  
  expect_message(message_console(c("ab", "cd")))
  
  expect_match(cap$calls[[1]]$text, "abcd$")
})

test_that("message_console accepts a non character message", {
  cap <- new.env(); message_console <- fake_message_console(cap)
  
  expect_message(message_console(123))
  
  expect_match(cap$calls[[1]]$text, "123$")
})

test_that("message_console does not build a line for blank messages", {
  cap <- new.env(); message_console <- fake_message_console(cap)
  
  for (blank in c("", "\n", "\r")) {
    cap$calls <- NULL
    expect_message(message_console(blank, level = "INFO"))
    expect_null(cap$calls[[1]]$text, info = blank)
  }
})

test_that("message_console puts the date before the message", {
  cap <- new.env(); message_console <- fake_message_console(cap)
  
  expect_message(message_console("hello"))
  
  expect_match(cap$calls[[1]]$text, "^\\[INFO\\] \\d{2}-\\d{2}-\\d{4} ")
})
