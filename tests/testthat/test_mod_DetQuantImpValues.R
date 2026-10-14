library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

skip_if_no_dq_deps <- function() {
  skip_if_not_installed("DT")
  skip_if_not_installed("DaparToolshed")
  skip_if_not_installed("MagellanNTK")
}

make_se_dq <- function() {
  mat <- matrix(
    c(1, 2, 3, 4,
      5, 6, 7, 8,
      9, 10, 11, 12),
    nrow = 3, byrow = TRUE,
    dimnames = list(paste0("f", 1:3), paste0("S", 1:4))
  )
  SummarizedExperiment::SummarizedExperiment(assays = list(counts = mat))
}

# Mocks the dependencies of the server.
#   cap$quant_args : list(x, quant, factor) given to getQuantile4Imp()
#   cap$pkgs       : packages given to pkgs_require()
#   cap$dt         : list(data, args) given to DT::datatable()
mock_dq <- function(cap, env = parent.frame()) {
  real_datatable <- DT::datatable   # keep the real function
  
  testthat::local_mocked_bindings(
    getQuantile4Imp = function(x, quant, factor, ...) {
      cap$quant_args <- list(x = x, quant = quant, factor = factor)
      list(shiftedImpVal = c(S1 = 0.5, S2 = 1.5, S3 = 2.5, S4 = 3.5))
    },
    .package = "DaparToolshed",
    .env = env
  )
  testthat::local_mocked_bindings(
    initComplete = function(...) DT::JS("function(settings, json) {}"),
    .package = "MagellanNTK",
    .env = env
  )
  testthat::local_mocked_bindings(
    pkgs_require = function(ll.deps) {
      cap$pkgs <- ll.deps
      invisible(NULL)
    },
    .package = "Prostar2",
    .env = env
  )
  testthat::local_mocked_bindings(
    # Spy: record the arguments, then return the real widget
    datatable = function(data, ...) {
      cap$dt <- list(data = data, args = list(...))
      real_datatable(data, ...)
    },
    .package = "DT",
    .env = env
  )
}

# Same mocks, but DT::datatable() is the real one (to check the widget class)
mock_dq_real_dt <- function(cap, env = parent.frame()) {
  testthat::local_mocked_bindings(
    getQuantile4Imp = function(x, quant, factor, ...) {
      cap$quant_args <- list(x = x, quant = quant, factor = factor)
      list(shiftedImpVal = c(S1 = 0.5, S2 = 1.5, S3 = 2.5, S4 = 3.5))
    },
    .package = "DaparToolshed",
    .env = env
  )
  testthat::local_mocked_bindings(
    initComplete = function(...) DT::JS("function(settings, json) {}"),
    .package = "MagellanNTK",
    .env = env
  )
  testthat::local_mocked_bindings(
    pkgs_require = function(ll.deps) {
      cap$pkgs <- ll.deps
      invisible(NULL)
    },
    .package = "Prostar2",
    .env = env
  )
}

# Runs the module server. Arguments are reactives; `expr` is captured lazily.
with_dq_server <- function(expr, dataIn = shiny::reactive(make_se_dq()),
                           quant = shiny::reactive(2.5),
                           factor = shiny::reactive(1.5)) {
  quo <- rlang::enquo(expr)
  shiny::testServer(
    mod_DetQuantImpValues_server,
    args = list(dataIn = dataIn, quant = quant, factor = factor),
    expr = !!quo
  )
}

# ---- Tests -----------------------------------------------------------------

test_that("mod_DetQuantImpValues_ui returns a tagList", {
  skip_if_not_installed("DT")
  
  res <- mod_DetQuantImpValues_ui("x")
  
  expect_s3_class(res, "shiny.tag.list")
})

test_that("mod_DetQuantImpValues_ui contains the explanatory text", {
  skip_if_not_installed("DT")
  
  html <- as.character(mod_DetQuantImpValues_ui("x"))
  
  expect_match(html, "The missing values will be imputed by the following values",
               fixed = TRUE)
})

test_that("mod_DetQuantImpValues_ui has a namespaced DT output", {
  skip_if_not_installed("DT")
  
  html <- as.character(mod_DetQuantImpValues_ui("x"))
  
  expect_match(html, 'id="x-detQuantValues_DT"', fixed = TRUE)
})

test_that("mod_DetQuantImpValues_ui uses the namespace given", {
  skip_if_not_installed("DT")
  
  a <- as.character(mod_DetQuantImpValues_ui("a"))
  b <- as.character(mod_DetQuantImpValues_ui("b"))
  
  expect_match(a, 'id="a-detQuantValues_DT"', fixed = TRUE)
  expect_no_match(a, 'id="b-detQuantValues_DT"', fixed = TRUE)
  expect_match(b, 'id="b-detQuantValues_DT"', fixed = TRUE)
})

test_that("mod_DetQuantImpValues returns a shiny app", {
  skip_if_not_installed("DT")
  
  expect_s3_class(mod_DetQuantImpValues(make_se_dq()), "shiny.appobj")
})

test_that("the server checks that omXplore is available", {
  skip_if_no_dq_deps()
  cap <- new.env(); mock_dq_real_dt(cap)
  
  with_dq_server({})
  
  expect_identical(cap$pkgs, "omXplore")
})

test_that("the server fails when omXplore is missing", {
  skip_if_no_dq_deps()
  cap <- new.env(); mock_dq_real_dt(cap)
  testthat::local_mocked_bindings(
    pkgs_require = function(...) stop("missing package"),
    .package = "Prostar2"
  )
  
  expect_error(with_dq_server({}), "missing package", fixed = TRUE)
})

test_that("the table is rendered as a DT widget", {
  skip_if_no_dq_deps()
  cap <- new.env(); mock_dq_real_dt(cap)
  
  with_dq_server({
    expect_false(is.null(output$detQuantValues_DT))
  })
})

test_that("getQuantile4Imp receives the assay of the dataset", {
  skip_if_no_dq_deps()
  cap <- new.env(); mock_dq_real_dt(cap)
  
  with_dq_server({
    output$detQuantValues_DT
    expect_equal(cap$quant_args$x,
                 SummarizedExperiment::assay(make_se_dq()))
  })
})

test_that("the quantile is given as a proportion (quant / 100)", {
  skip_if_no_dq_deps()
  cap <- new.env(); mock_dq_real_dt(cap)
  
  with_dq_server({
    output$detQuantValues_DT
    expect_equal(cap$quant_args$quant, 0.025)
  })
})

test_that("the factor is given unchanged", {
  skip_if_no_dq_deps()
  cap <- new.env(); mock_dq_real_dt(cap)
  
  with_dq_server({
    output$detQuantValues_DT
    expect_equal(cap$quant_args$factor, 1.5)
  })
})

test_that("the table has one row and one column per sample", {
  skip_if_no_dq_deps()
  cap <- new.env(); mock_dq(cap)
  
  with_dq_server({
    output$detQuantValues_DT
    expect_s3_class(cap$dt$data, "data.frame")
    expect_equal(dim(cap$dt$data), c(1, 4))
    expect_identical(colnames(cap$dt$data), c("S1", "S2", "S3", "S4"))
  })
})

test_that("the table contains the shifted imputation values", {
  skip_if_no_dq_deps()
  cap <- new.env(); mock_dq(cap)
  
  with_dq_server({
    try(output$detQuantValues_DT, silent = TRUE)
    expect_equal(unlist(cap$dt$data, use.names = FALSE),
                 c(0.5, 1.5, 2.5, 3.5))
  })
})

test_that("the table is displayed without row names, as a plain table", {
  skip_if_no_dq_deps()
  cap <- new.env(); mock_dq(cap)
  
  with_dq_server({
    try(output$detQuantValues_DT, silent = TRUE)
    expect_false(cap$dt$args$rownames)
    expect_identical(cap$dt$args$options$dom, "t")
    expect_false(cap$dt$args$options$bLengthChange)
  })
})

test_that("the table follows changes of quant and factor", {
  skip_if_no_dq_deps()
  cap <- new.env(); mock_dq_real_dt(cap)
  quant <- shiny::reactiveVal(2.5)
  factor <- shiny::reactiveVal(1.5)
  
  with_dq_server(
    {
      output$detQuantValues_DT
      expect_equal(cap$quant_args$quant, 0.025)
      
      quant(10)
      factor(2)
      session$flushReact()
      output$detQuantValues_DT
      
      expect_equal(cap$quant_args$quant, 0.1)
      expect_equal(cap$quant_args$factor, 2)
    },
    quant = quant, factor = factor
  )
})

test_that("the table stops silently when there is no dataset", {
  skip_if_no_dq_deps()
  cap <- new.env(); mock_dq_real_dt(cap)
  
  with_dq_server(
    {
      expect_error(output$detQuantValues_DT, class = "shiny.silent.error")
      expect_null(cap$quant_args)
    },
    dataIn = shiny::reactive(NULL)
  )
})

test_that("the server works with its default arguments", {
  skip_if_no_dq_deps()
  cap <- new.env(); mock_dq_real_dt(cap)
  
  shiny::testServer(mod_DetQuantImpValues_server, expr = {
    expect_error(output$detQuantValues_DT, class = "shiny.silent.error")
  })
})
