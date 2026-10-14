library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

skip_if_no_hd_deps <- function() {
  skip_if_not_installed("QFeatures")
  skip_if_not_installed("MagellanNTK")
  skip_if_not_installed("DaparToolshed")
}

# QFeatures with 2 assays. `offset` changes the values of the last assay,
# so two calls with different offsets give non-identical objects.
make_qf_hd <- function(offset = 0) {
  make_se <- function(offset) {
    SummarizedExperiment::SummarizedExperiment(
      assays = list(counts = matrix(
        seq_len(4) + offset, 2,
        dimnames = list(c("f1", "f2"), c("S1", "S2"))
      ))
    )
  }
  QFeatures::QFeatures(
    list(first = make_se(0), last = make_se(100 + offset)),
    colData = S4Vectors::DataFrame(row.names = c("S1", "S2"))
  )
}

hist_hd <- function() {
  data.frame(Step = "Filtering", Substep = "s", Parameter = "p",
             Value = "v", stringsAsFactors = FALSE)
}

# Mocks the collaborators.
#   cap$ph_obj  : object given to paramshistory()
#   cap$dt_id   : id given to format_DT_server()
#   cap$dt_data : reactive given to format_DT_server()
mock_hd <- function(cap, history = hist_hd(), env = parent.frame()) {
  testthat::local_mocked_bindings(
    paramshistory = function(object, ...) {
      cap$ph_obj <- object
      history
    },
    .package = "DaparToolshed",
    .env = env
  )
  testthat::local_mocked_bindings(
    format_DT_server = function(id, dataIn = NULL, ...) {
      cap$dt_id <- id
      cap$dt_data <- dataIn
      invisible(NULL)
    },
    format_DT_ui = function(id, ...) shiny::div(id = id, "fake DT"),
    .package = "MagellanNTK",
    .env = env
  )
}

with_hd_server <- function(expr, ...) {
  quo <- rlang::enquo(expr)
  shiny::testServer(history_dataset_server, args = list(...), expr = !!quo)
}

# ---- Tests -----------------------------------------------------------------

test_that("history_dataset_ui returns a div with a fixed height", {
  skip_if_no_hd_deps()
  cap <- new.env(); mock_hd(cap)
  
  res <- history_dataset_ui("x")
  
  expect_s3_class(res, "shiny.tag")
  expect_identical(res$name, "div")
  expect_match(res$attribs$style, "height: 600px", fixed = TRUE)
})

test_that("history_dataset_ui contains the namespaced history table", {
  skip_if_no_hd_deps()
  cap <- new.env(); mock_hd(cap)
  
  html <- as.character(history_dataset_ui("x"))
  
  expect_match(html, 'id="x-history"', fixed = TRUE)
})

test_that("history_dataset_ui uses the namespace given", {
  skip_if_no_hd_deps()
  cap <- new.env(); mock_hd(cap)
  
  a <- as.character(history_dataset_ui("a"))
  b <- as.character(history_dataset_ui("b"))
  
  expect_match(a, 'id="a-history"', fixed = TRUE)
  expect_no_match(a, 'id="b-history"', fixed = TRUE)
  expect_match(b, 'id="b-history"', fixed = TRUE)
})

test_that("history_dataset returns a shiny app", {
  skip_if_no_hd_deps()
  cap <- new.env(); mock_hd(cap)
  
  expect_s3_class(history_dataset(make_qf_hd()), "shiny.appobj")
})

test_that("the server starts the table module with the id 'history'", {
  skip_if_no_hd_deps()
  cap <- new.env(); mock_hd(cap)
  
  with_hd_server({
    expect_identical(cap$dt_id, "history")
    expect_true(is.function(cap$dt_data))
  }, dataIn = shiny::reactive(make_qf_hd()))
})

test_that("rv starts empty", {
  skip_if_no_hd_deps()
  cap <- new.env(); mock_hd(cap)
  
  with_hd_server({
    expect_null(rv$dataIn)
  })
})

test_that("the dataset is stored when it is a QFeatures", {
  skip_if_no_hd_deps()
  cap <- new.env(); mock_hd(cap)
  qf <- make_qf_hd()
  
  with_hd_server({
    session$flushReact()
    expect_identical(rv$dataIn, qf)
  }, dataIn = shiny::reactive(qf))
})

test_that("a dataset which is not a QFeatures is ignored", {
  skip_if_no_hd_deps()
  cap <- new.env(); mock_hd(cap)
  
  with_hd_server({
    session$flushReact()
    expect_null(rv$dataIn)
  }, dataIn = shiny::reactive(list(a = 1)))
})

test_that("a NULL dataset is ignored", {
  skip_if_no_hd_deps()
  cap <- new.env(); mock_hd(cap)
  
  with_hd_server({
    session$flushReact()
    expect_null(rv$dataIn)
  }, dataIn = shiny::reactive(NULL))
})

test_that("the history is the one of the last assay", {
  skip_if_no_hd_deps()
  cap <- new.env(); mock_hd(cap)
  qf <- make_qf_hd()
  
  with_hd_server({
    session$flushReact()
    res <- Get_QFeatures_History()
    
    expect_identical(res, hist_hd())
    expect_equal(SummarizedExperiment::assay(cap$ph_obj),
                 SummarizedExperiment::assay(qf[["last"]]))
  }, dataIn = shiny::reactive(qf))
})

test_that("the table module receives the history", {
  skip_if_no_hd_deps()
  cap <- new.env(); mock_hd(cap)
  
  with_hd_server({
    session$flushReact()
    
    expect_identical(cap$dt_data(), hist_hd())
  }, dataIn = shiny::reactive(make_qf_hd()))
})

test_that("the history waits for a dataset", {
  skip_if_no_hd_deps()
  cap <- new.env(); mock_hd(cap)
  
  with_hd_server({
    session$flushReact()
    
    expect_error(Get_QFeatures_History(), class = "shiny.silent.error")
    expect_error(cap$dt_data(), class = "shiny.silent.error")
    expect_null(cap$ph_obj)
  })
})

test_that("the history follows a dataset that arrives later", {
  skip_if_no_hd_deps()
  cap <- new.env(); mock_hd(cap)
  data <- shiny::reactiveVal(NULL)
  
  with_hd_server({
    session$flushReact()
    expect_error(cap$dt_data(), class = "shiny.silent.error")
    
    data(make_qf_hd())
    session$flushReact()
    
    expect_identical(cap$dt_data(), hist_hd())
  }, dataIn = data)
})

test_that("the history is recomputed when the dataset changes", {
  skip_if_no_hd_deps()
  cap <- new.env()
  calls <- 0
  testthat::local_mocked_bindings(
    paramshistory = function(object, ...) {
      calls <<- calls + 1
      data.frame(Step = paste0("call", calls), Substep = "s", Parameter = "p",
                 Value = "v", stringsAsFactors = FALSE)
    },
    .package = "DaparToolshed"
  )
  testthat::local_mocked_bindings(
    format_DT_server = function(id, dataIn = NULL, ...) {
      cap$dt_data <- dataIn
      invisible(NULL)
    },
    .package = "MagellanNTK"
  )
  data <- shiny::reactiveVal(make_qf_hd())
  
  with_hd_server({
    session$flushReact()
    first <- cap$dt_data()$Step
    
    data(make_qf_hd(offset = 1))   # different content, so it invalidates
    session$flushReact()
    second <- cap$dt_data()$Step
    
    expect_identical(first, "call1")
    expect_identical(second, "call2")
  }, dataIn = data)
})

test_that("the server works with its default arguments", {
  skip_if_no_hd_deps()
  cap <- new.env(); mock_hd(cap)
  
  expect_no_error(shiny::testServer(history_dataset_server, expr = {}))
})
