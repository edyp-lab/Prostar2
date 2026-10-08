library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

# removeAssay() emits a harmless MultiAssayExperiment warning
quiet_dropped <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl("experiments' dropped", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# QFeatures with 2 assays of 5 rows x 4 samples
make_qf_filt <- function() {
  coldata <- S4Vectors::DataFrame(
    Condition = c("A", "A", "B", "B"),
    row.names = paste0("S", 1:4)
  )
  make_se <- function(offset) {
    SummarizedExperiment::SummarizedExperiment(
      assays = list(counts = matrix(
        seq_len(20) + offset, nrow = 5,
        dimnames = list(paste0("f", 1:5), paste0("S", 1:4))
      ))
    )
  }
  QFeatures::QFeatures(
    list(a1 = make_se(0), a2 = make_se(100)),
    colData = coldata
  )
}

# QFeatures with 2 assays whose rowData has a numeric column
# ('Unique_peptides') and a character column ('Name').
# 'first_type' sets the type of 'Unique_peptides' in the FIRST assay.
make_qf_var <- function(first_type = c("numeric", "character")) {
  first_type <- match.arg(first_type)
  coldata <- S4Vectors::DataFrame(
    Condition = c("A", "A", "B", "B"),
    row.names = paste0("S", 1:4)
  )
  make_se <- function(unique_pep) {
    se <- SummarizedExperiment::SummarizedExperiment(
      assays = list(counts = matrix(
        seq_len(12), nrow = 3,
        dimnames = list(paste0("p", 1:3), paste0("S", 1:4))
      ))
    )
    SummarizedExperiment::rowData(se)$Unique_peptides <- unique_pep
    SummarizedExperiment::rowData(se)$Name <- c("a", "b", "c")
    se
  }
  first <- if (first_type == "numeric") c(1, 2, 3) else c("1", "2", "3")
  QFeatures::QFeatures(
    list(v1 = make_se(first), v2 = make_se(c(1, 2, 3))),
    colData = coldata
  )
}

# Mock of DaparToolshed::filterFeaturesOneSE(): records its arguments in 'cap'
# and adds ONE new assay made of the 'keep' rows of assay 'i'
fake_filter <- function(cap, keep = 1:3) {
  function(object, i, name, filters) {
    cap$args <- list(object = object, i = i, name = name, filters = filters)
    QFeatures::addAssay(object, object[[i]][keep, ], name = "filtered_tmp")
  }
}

# Mock of Extract_Value(): records the requested type in 'cap'
fake_extract <- function(cap) {
  function(value, type) {
    cap$type <- type
    if (type == "numeric") as.numeric(value) else as.character(value)
  }
}

widgets_default <- function(...) {
  modifyList(
    list(
      Variablefiltering_value = 2,
      Variablefiltering_operator = "<",
      Variablefiltering_cname = "Unique_peptides",
      Variablefiltering_keep_vs_remove = "delete"
    ),
    list(...)
  )
}

# Mocks everything variableFiltering() depends on
mock_variable_filtering <- function(cap, keep = 1:3) {
  env <- parent.frame()
  
  testthat::local_mocked_bindings(
    filterFeaturesOneSE = fake_filter(cap, keep),
    .package = "DaparToolshed",
    .env = env
  )
  testthat::local_mocked_bindings(
    Timestamp = function() "TS",
    .package = "MagellanNTK",
    .env = env
  )
  testthat::local_mocked_bindings(
    Add2History = function(history, ...) {
      cap$hist <- list(history, ...)
      "new_hist"
    },
    Variablefiltering_BuildVariableFilter = function(...) {
      cap$build <- list(...)
      "the_filter"
    },
    Variablefiltering_WriteQuery = function(...) {
      cap$query <- list(...)
      "the_query"
    },
    .package = "Prostar2",
    .env = env
  )
}

# ---- Tests -----------------------------------------------------------------

## ----- cellmetadataFiltering -----
test_that("cellmetadataFiltering returns data, summary and history", {
  qf <- make_qf_filt()
  cap <- new.env()
  
  testthat::local_mocked_bindings(
    filterFeaturesOneSE = fake_filter(cap),
    .package = "DaparToolshed"
  )
  testthat::local_mocked_bindings(Timestamp = function() "TS", .package = "MagellanNTK")
  testthat::local_mocked_bindings(
    Add2History = function(history, ...) history,
    .package = "Prostar2"
  )
  
  res <- cellmetadataFiltering(qf, filters = list("f"), query = "my query",
                               history = NULL, original_length = 2)
  
  expect_type(res, "list")
  expect_named(res, c("data", "summary", "history"))
  expect_s4_class(res$data, "QFeatures")
  expect_null(res$history)
})

test_that("cellmetadataFiltering forwards its arguments to filterFeaturesOneSE", {
  qf <- make_qf_filt()
  cap <- new.env()
  flt <- list("some_filter")
  
  testthat::local_mocked_bindings(
    filterFeaturesOneSE = fake_filter(cap),
    .package = "DaparToolshed"
  )
  testthat::local_mocked_bindings(Timestamp = function() "TS", .package = "MagellanNTK")
  testthat::local_mocked_bindings(
    Add2History = function(history, ...) history,
    .package = "Prostar2"
  )
  
  cellmetadataFiltering(qf, flt, "q", NULL, original_length = 2)
  
  expect_identical(cap$args$object, qf)
  expect_identical(cap$args$i, 2L)               # last assay
  expect_identical(cap$args$name, "qMetacellFilteredTS")
  expect_identical(cap$args$filters, flt)
})

test_that("cellmetadataFiltering keeps the original assays and renames the new one", {
  qf <- make_qf_filt()
  cap <- new.env()
  
  testthat::local_mocked_bindings(
    filterFeaturesOneSE = fake_filter(cap, keep = 1:3),
    .package = "DaparToolshed"
  )
  testthat::local_mocked_bindings(Timestamp = function() "TS", .package = "MagellanNTK")
  testthat::local_mocked_bindings(
    Add2History = function(history, ...) history,
    .package = "Prostar2"
  )  
  # 1 assay added: len_diff == 1, nothing removed
  res <- cellmetadataFiltering(qf, list(), "q", NULL, original_length = 2)
  
  expect_identical(names(res$data), c("a1", "a2", "Cellmetadatafiltering"))
  expect_equal(nrow(res$data[["Cellmetadatafiltering"]]), 3)
})

test_that("cellmetadataFiltering removes the intermediate assay when len_diff == 2", {
  qf <- make_qf_filt()
  cap <- new.env()
  
  testthat::local_mocked_bindings(
    filterFeaturesOneSE = fake_filter(cap, keep = 1:3),
    .package = "DaparToolshed"
  )
  testthat::local_mocked_bindings(Timestamp = function() "TS", .package = "MagellanNTK")
  testthat::local_mocked_bindings(
    Add2History = function(history, ...) history,
    .package = "Prostar2"
  )  
  # original_length = 1 -> the result has 3 assays, so len_diff == 2:
  # the assay before the last one (a2) is removed
  res <- quiet_dropped(
    cellmetadataFiltering(qf, list(), "q", NULL, original_length = 1)
  )
  
  expect_identical(names(res$data), c("a1", "Cellmetadatafiltering"))
  expect_equal(nrow(res$data[["Cellmetadatafiltering"]]), 3)
})

test_that("cellmetadataFiltering computes the summary", {
  qf <- make_qf_filt()
  cap <- new.env()
  
  testthat::local_mocked_bindings(
    filterFeaturesOneSE = fake_filter(cap, keep = 1:3),   # 5 -> 3 rows
    .package = "DaparToolshed"
  )
  testthat::local_mocked_bindings(Timestamp = function() "TS", .package = "MagellanNTK")
  testthat::local_mocked_bindings(
    Add2History = function(history, ...) history,
    .package = "Prostar2"
  )  
  res <- cellmetadataFiltering(qf, list(), "my query", NULL, original_length = 2)
  
  # c(query, nbDeleted, nbRemaining) is coerced to character
  expect_identical(res$summary, c("my query", "2", "3"))
})

test_that("cellmetadataFiltering reports 0 deleted when nothing is filtered", {
  qf <- make_qf_filt()
  cap <- new.env()
  
  testthat::local_mocked_bindings(
    filterFeaturesOneSE = fake_filter(cap, keep = 1:5),
    .package = "DaparToolshed"
  )
  testthat::local_mocked_bindings(Timestamp = function() "TS", .package = "MagellanNTK")
  testthat::local_mocked_bindings(
    Add2History = function(history, ...) history,
    .package = "Prostar2"
  )  
  res <- cellmetadataFiltering(qf, list(), "q", NULL, original_length = 2)
  
  expect_identical(res$summary, c("q", "0", "5"))
})

test_that("cellmetadataFiltering stops silently when no assay was added", {
  qf <- make_qf_filt()
  cap <- new.env()
  
  testthat::local_mocked_bindings(
    filterFeaturesOneSE = fake_filter(cap),
    .package = "DaparToolshed"
  )
  testthat::local_mocked_bindings(Timestamp = function() "TS", .package = "MagellanNTK")
  testthat::local_mocked_bindings(
    Add2History = function(history, ...) history,
    .package = "Prostar2"
  )
  
  # original_length = 3 -> len_diff == 0 -> req() fails
  expect_error(
    cellmetadataFiltering(qf, list(), "q", NULL, original_length = 3),
    class = "shiny.silent.error"
  )
})

test_that("cellmetadataFiltering records the query in the history", {
  qf <- make_qf_filt()
  cap <- new.env()
  
  testthat::local_mocked_bindings(
    filterFeaturesOneSE = fake_filter(cap),
    .package = "DaparToolshed"
  )
  testthat::local_mocked_bindings(Timestamp = function() "TS", .package = "MagellanNTK")
  testthat::local_mocked_bindings(
    Add2History = function(history, ...) {
      cap$hist <- list(history, ...)
      "new_hist"
    },
    .package = "Prostar2"
  )
  
  init <- list(previous = "step")
  cellmetadataFiltering(qf, list(), "my query", init, original_length = 2)
  
  expect_identical(cap$hist[[1]], init)
  expect_identical(cap$hist[[2]], "Filtering")
  expect_identical(cap$hist[[3]], "Cellmetadatafiltering")
  expect_identical(cap$hist[[4]], "query")
  expect_identical(cap$hist[[5]], "my query")
})





## ----- variableFiltering -----
test_that("variableFiltering returns data, summary and history", {
  qf <- make_qf_filt()
  cap <- new.env()
  mock_variable_filtering(cap)
  
  res <- variableFiltering(qf, widgets_default(), NULL, original_length = 2)
  
  expect_type(res, "list")
  expect_named(res, c("data", "summary", "history"))
  expect_s4_class(res$data, "QFeatures")
  expect_identical(res$history, "new_hist")
})

test_that("variableFiltering passes the widget values to the filter and query builders", {
  qf <- make_qf_filt()
  cap <- new.env()
  mock_variable_filtering(cap)
  
  w <- widgets_default(
    Variablefiltering_value = 5,
    Variablefiltering_operator = ">=",
    Variablefiltering_cname = "Score",
    Variablefiltering_keep_vs_remove = "keep"
  )
  variableFiltering(qf, w, NULL, original_length = 2)
  
  for (nm in c("build", "query")) {
    args <- cap[[nm]]
    expect_identical(args$value, 5)
    expect_identical(args$operator, ">=")
    expect_identical(args$cname, "Score")
    expect_identical(args$keep_vs_remove, "keep")
    expect_identical(args$data, qf)
  }
})

test_that("variableFiltering applies the built filter on the last assay", {
  qf <- make_qf_filt()
  cap <- new.env()
  mock_variable_filtering(cap)
  
  variableFiltering(qf, widgets_default(), NULL, original_length = 2)
  
  expect_identical(cap$args$i, 2L)
  expect_identical(cap$args$name, "variableFilteredTS")
  expect_identical(cap$args$filters, list("the_filter"))
})

test_that("variableFiltering keeps the original assays and renames the new one", {
  qf <- make_qf_filt()
  cap <- new.env()
  mock_variable_filtering(cap, keep = 1:3)
  
  res <- variableFiltering(qf, widgets_default(), NULL, original_length = 2)
  
  expect_identical(names(res$data), c("a1", "a2", "Variablefiltering"))
  expect_equal(nrow(res$data[["Variablefiltering"]]), 3)
})

test_that("variableFiltering removes the intermediate assay when len_diff == 2", {
  qf <- make_qf_filt()
  cap <- new.env()
  mock_variable_filtering(cap, keep = 1:3)
  
  res <- quiet_dropped(
    variableFiltering(qf, widgets_default(), NULL, original_length = 1)
  )
  
  expect_identical(names(res$data), c("a1", "Variablefiltering"))
})

test_that("variableFiltering computes the summary", {
  qf <- make_qf_filt()
  cap <- new.env()
  mock_variable_filtering(cap, keep = 1:3)   # 5 -> 3 rows
  
  res <- variableFiltering(qf, widgets_default(), NULL, original_length = 2)
  
  # c(<list>, numeric, numeric) gives a list: query, nbDeleted, nbRemaining
  expect_type(res$summary, "list")
  expect_length(res$summary, 3)
  expect_identical(res$summary[[1]], "the_query")
  expect_equal(res$summary[[2]], 2)
  expect_equal(res$summary[[3]], 3)
})

test_that("variableFiltering stops silently when no assay was added", {
  qf <- make_qf_filt()
  cap <- new.env()
  mock_variable_filtering(cap)
  
  expect_error(
    variableFiltering(qf, widgets_default(), NULL, original_length = 3),
    class = "shiny.silent.error"
  )
})

test_that("variableFiltering propagates the silent stop of the filter builder", {
  qf <- make_qf_filt()
  cap <- new.env()
  mock_variable_filtering(cap)
  
  testthat::local_mocked_bindings(
    Variablefiltering_BuildVariableFilter = function(...) shiny::req(FALSE),
    .package = "Prostar2"
  )
  
  expect_error(
    variableFiltering(qf, widgets_default(), NULL, original_length = 2),
    class = "shiny.silent.error"
  )
})

test_that("variableFiltering records the query list in the history", {
  qf <- make_qf_filt()
  cap <- new.env()
  mock_variable_filtering(cap)
  
  init <- list(previous = "step")
  variableFiltering(qf, widgets_default(), init, original_length = 2)
  
  expect_identical(cap$hist[[1]], init)
  expect_identical(cap$hist[[2]], "Filtering")
  expect_identical(cap$hist[[3]], "Variablefiltering")
  expect_identical(cap$hist[[4]], "query")
  expect_identical(cap$hist[[5]], list("the_query"))
})





## ----- Variablefiltering_BuildVariableFilter -----
test_that("BuildVariableFilter returns a NumericVariableFilter for a numeric column", {
  cap <- new.env()
  testthat::local_mocked_bindings(
    Extract_Value = fake_extract(cap),
    .package = "Prostar2"
  )  
  
  flt <- Variablefiltering_BuildVariableFilter(
    value = 2, operator = "<", cname = "Unique_peptides",
    keep_vs_remove = "delete", data = make_qf_var()
  )
  
  expect_s4_class(flt, "NumericVariableFilter")
  expect_identical(cap$type, "numeric")
  expect_identical(flt@field, "Unique_peptides")
  expect_equal(flt@value, 2)
  expect_identical(flt@condition, "<")
})

test_that("BuildVariableFilter returns a CharacterVariableFilter for a character column", {
  cap <- new.env()
  testthat::local_mocked_bindings(
    Extract_Value = fake_extract(cap),
    .package = "Prostar2"
  )
  
  flt <- Variablefiltering_BuildVariableFilter(
    value = "b", operator = "==", cname = "Name",
    keep_vs_remove = "keep", data = make_qf_var()
  )
  
  expect_s4_class(flt, "CharacterVariableFilter")
  expect_identical(cap$type, "character")
  expect_identical(flt@field, "Name")
  expect_identical(flt@value, "b")
  expect_identical(flt@condition, "==")
})

test_that("BuildVariableFilter negates the filter only when deleting", {
  testthat::local_mocked_bindings(Extract_Value = fake_extract(new.env()),
                                  .package = "Prostar2")
  qf <- make_qf_var()
  
  del <- Variablefiltering_BuildVariableFilter(
    value = 2, operator = "<", cname = "Unique_peptides",
    keep_vs_remove = "delete", data = qf
  )
  keep <- Variablefiltering_BuildVariableFilter(
    value = 2, operator = "<", cname = "Unique_peptides",
    keep_vs_remove = "keep", data = qf
  )
  
  expect_true(del@not)
  expect_false(keep@not)
})

test_that("BuildVariableFilter uses the last assay by default and 'i' when given", {
  cap <- new.env()
  testthat::local_mocked_bindings(
    Extract_Value = fake_extract(cap),
    .package = "Prostar2"
  )
  
  # v1: 'Unique_peptides' is character, v2 (last): numeric
  qf <- make_qf_var(first_type = "character")
  # "==" is valid for both NumericVariableFilter and CharacterVariableFilter
  args <- list(value = 2, operator = "==", cname = "Unique_peptides",
               keep_vs_remove = "keep", data = qf)
  
  flt_last <- do.call(Variablefiltering_BuildVariableFilter, args)
  expect_identical(cap$type, "numeric")
  expect_s4_class(flt_last, "NumericVariableFilter")
  
  flt_first <- do.call(Variablefiltering_BuildVariableFilter, c(args, i = 1))
  expect_identical(cap$type, "character")
  expect_s4_class(flt_first, "CharacterVariableFilter")
})

test_that("BuildVariableFilter stops silently on placeholder values", {
  qf <- make_qf_var()
  testthat::local_mocked_bindings(Extract_Value = fake_extract(new.env()),
                                  .package = "Prostar2")
  
  expect_error(
    Variablefiltering_BuildVariableFilter(
      "Enter value...", "<", "Unique_peptides", "delete", qf),
    class = "shiny.silent.error"
  )
  expect_error(
    Variablefiltering_BuildVariableFilter(
      2, "None", "Unique_peptides", "delete", qf),
    class = "shiny.silent.error"
  )
  expect_error(
    Variablefiltering_BuildVariableFilter(
      2, "<", "None", "delete", qf),
    class = "shiny.silent.error"
  )
})

test_that("BuildVariableFilter stops silently when keep_vs_remove or data is NULL", {
  qf <- make_qf_var()
  
  expect_error(
    Variablefiltering_BuildVariableFilter(
      2, "<", "Unique_peptides", NULL, qf),
    class = "shiny.silent.error"
  )
  expect_error(
    Variablefiltering_BuildVariableFilter(
      2, "<", "Unique_peptides", "delete", NULL),
    class = "shiny.silent.error"
  )
})

test_that("BuildVariableFilter errors when value, operator or cname is NULL", {
  qf <- make_qf_var()
  
  # With R >= 4.3, `NULL != "..." && ...` raises a regular error
  # (length-zero condition in `&&`); older versions end in req(NA).
  # Either way, nothing is returned.
  expect_error(Variablefiltering_BuildVariableFilter(
    NULL, "<", "Unique_peptides", "delete", qf))
  expect_error(Variablefiltering_BuildVariableFilter(
    2, NULL, "Unique_peptides", "delete", qf))
  expect_error(Variablefiltering_BuildVariableFilter(
    2, "<", NULL, "delete", qf))
  expect_error(Variablefiltering_BuildVariableFilter())
})

test_that("BuildVariableFilter stops silently when the value cannot be extracted", {
  qf <- make_qf_var()
  args <- list(value = "abc", operator = "<", cname = "Unique_peptides",
               keep_vs_remove = "delete", data = qf)
  
  # Extract_Value() signals a warning
  testthat::local_mocked_bindings(
    Extract_Value = function(value, type) warning("coercion"),
    .package = "Prostar2"
  )
  expect_error(do.call(Variablefiltering_BuildVariableFilter, args),
               class = "shiny.silent.error")
  
  # Extract_Value() signals an error
  testthat::local_mocked_bindings(
    Extract_Value = function(value, type) stop("bad value"),
    .package = "Prostar2"
  )
  expect_error(do.call(Variablefiltering_BuildVariableFilter, args),
               class = "shiny.silent.error")
  
  # Extract_Value() returns NULL
  testthat::local_mocked_bindings(
    Extract_Value = function(value, type) NULL,
    .package = "Prostar2"
  )
  expect_error(do.call(Variablefiltering_BuildVariableFilter, args),
               class = "shiny.silent.error")
})





## ----- Variablefiltering_WriteQuery -----
test_that("WriteQuery writes the query for a deletion", {
  testthat::local_mocked_bindings(Extract_Value = fake_extract(new.env()),
                                  .package = "Prostar2")
  
  q <- Variablefiltering_WriteQuery(
    value = 2, operator = "<", cname = "Unique_peptides",
    keep_vs_remove = "delete", data = make_qf_var()
  )
  
  expect_type(q, "character")
  expect_identical(q, "delete values for which Unique_peptides < 2")
})

test_that("WriteQuery writes the query for a keep", {
  testthat::local_mocked_bindings(Extract_Value = fake_extract(new.env()),
                                  .package = "Prostar2")
  
  q <- Variablefiltering_WriteQuery(
    value = "b", operator = "==", cname = "Name",
    keep_vs_remove = "keep", data = make_qf_var()
  )
  
  expect_identical(q, "keep values for which Name == b")
})

test_that("WriteQuery uses the raw value, not the extracted one", {
  # Extract_Value is only a validation step: the query uses `value` as typed
  testthat::local_mocked_bindings(
    Extract_Value = function(value, type) 999,
    .package = "Prostar2"
  )
  
  q <- Variablefiltering_WriteQuery(
    value = "3", operator = ">=", cname = "Unique_peptides",
    keep_vs_remove = "keep", data = make_qf_var()
  )
  
  expect_identical(q, "keep values for which Unique_peptides >= 3")
})

test_that("WriteQuery uses the last assay by default and 'i' when given", {
  cap <- new.env()
  testthat::local_mocked_bindings(
    Extract_Value = fake_extract(cap),
    .package = "Prostar2"
  )
  
  qf <- make_qf_var(first_type = "character")
  args <- list(value = 2, operator = "<", cname = "Unique_peptides",
               keep_vs_remove = "keep", data = qf)
  
  do.call(Variablefiltering_WriteQuery, args)
  expect_identical(cap$type, "numeric")
  
  do.call(Variablefiltering_WriteQuery, c(args, i = 1))
  expect_identical(cap$type, "character")
})

test_that("WriteQuery stops silently on placeholder values", {
  qf <- make_qf_var()
  testthat::local_mocked_bindings(Extract_Value = fake_extract(new.env()),
                                  .package = "Prostar2")
  
  expect_error(
    Variablefiltering_WriteQuery("Enter value...", "<", "Unique_peptides", "delete", qf),
    class = "shiny.silent.error"
  )
  expect_error(
    Variablefiltering_WriteQuery(2, "None", "Unique_peptides", "delete", qf),
    class = "shiny.silent.error"
  )
  expect_error(
    Variablefiltering_WriteQuery(2, "<", "None", "delete", qf),
    class = "shiny.silent.error"
  )
})

test_that("WriteQuery stops silently when keep_vs_remove or data is NULL", {
  qf <- make_qf_var()
  
  expect_error(
    Variablefiltering_WriteQuery(2, "<", "Unique_peptides", NULL, qf),
    class = "shiny.silent.error"
  )
  expect_error(
    Variablefiltering_WriteQuery(2, "<", "Unique_peptides", "delete", NULL),
    class = "shiny.silent.error"
  )
})

test_that("WriteQuery errors when value, operator or cname is NULL", {
  qf <- make_qf_var()
  
  expect_error(Variablefiltering_WriteQuery(
    NULL, "<", "Unique_peptides", "delete", qf))
  expect_error(Variablefiltering_WriteQuery(
    2, NULL, "Unique_peptides", "delete", qf))
  expect_error(Variablefiltering_WriteQuery(
    2, "<", NULL, "delete", qf))
  expect_error(Variablefiltering_WriteQuery())
})

test_that("WriteQuery stops silently when the value cannot be extracted", {
  qf <- make_qf_var()
  args <- list(value = "abc", operator = "<", cname = "Unique_peptides",
               keep_vs_remove = "delete", data = qf)
  
  testthat::local_mocked_bindings(
    Extract_Value = function(value, type) warning("coercion"),
    .package = "Prostar2"
  )
  expect_error(do.call(Variablefiltering_WriteQuery, args),
               class = "shiny.silent.error")
  
  testthat::local_mocked_bindings(
    Extract_Value = function(value, type) stop("bad value"),
    .package = "Prostar2"
  )
  expect_error(do.call(Variablefiltering_WriteQuery, args),
               class = "shiny.silent.error")
})

# Consistency between filter and query 
test_that("BuildVariableFilter and WriteQuery describe the same filter", {
  testthat::local_mocked_bindings(Extract_Value = fake_extract(new.env()),
                                  .package = "Prostar2")
  args <- list(value = 4, operator = ">", cname = "Unique_peptides",
               keep_vs_remove = "keep", data = make_qf_var())
  
  flt <- do.call(Variablefiltering_BuildVariableFilter, args)
  q   <- do.call(Variablefiltering_WriteQuery, args)
  
  expect_match(q, paste0(flt@field, " ", flt@condition, " ", flt@value),
               fixed = TRUE)
})

