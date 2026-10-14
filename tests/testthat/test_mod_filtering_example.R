library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

skip_if_no_fe_deps <- function() {
  skip_if_not_installed("DT")
  skip_if_not_installed("omXplore")
  skip_if_not_installed("shinyjqui")
}

make_se_fe <- function(n = 3) {
  SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = matrix(
      seq_len(n * 2), n,
      dimnames = list(paste0("f", seq_len(n)), c("S1", "S2"))
    ))
  )
}

# "Enriched" table: column 1 + 2 data columns + 2 metacell columns.
# Columns 4:5 are the invisible ones (tags).
make_enriched_df <- function(n = 3) {
  data.frame(
    id = paste0("f", seq_len(n)),
    S1 = seq_len(n), S2 = seq_len(n) * 2,
    metacell_S1 = rep("a", n), metacell_S2 = rep("b", n),
    stringsAsFactors = FALSE
  )
}

# Mocks the collaborators and spies on DT.
#   cap$pkgs   : every package given to pkgs_require()
#   cap$build  : object given to Build_enriched_qdata()
#   cap$dt     : list(data, args) given to DT::datatable()
#   cap$fs     : list(columns, valueColumns, backgroundColor)
#                given to DT::formatStyle()
#   cap$levels, cap$values : arguments of DT::styleEqual()
# `enriched` = FALSE makes Build_enriched_qdata() return its input unchanged.
mock_fe <- function(cap, enriched = TRUE, env = parent.frame()) {
  real_datatable   <- DT::datatable
  real_formatStyle <- DT::formatStyle
  real_styleEqual  <- DT::styleEqual
  
  testthat::local_mocked_bindings(
    pkgs_require = function(ll.deps) {
      cap$pkgs <- c(cap$pkgs, ll.deps)
      invisible(NULL)
    },
    .package = "Prostar2",
    .env = env
  )
  testthat::local_mocked_bindings(
    Build_enriched_qdata = function(obj, ...) {
      cap$build <- obj
      if (enriched) make_enriched_df(nrow(obj)) else obj
    },
    custom_metacell_colors = function(...) list(a = "#FF0000", b = "#00FF00"),
    .package = "omXplore",
    .env = env
  )
  testthat::local_mocked_bindings(
    datatable = function(data, ...) {
      cap$dt <- list(data = data, args = list(...))
      real_datatable(data, ...)
    },
    formatStyle = function(table, columns, valueColumns = columns, ...) {
      args <- list(...)
      cap$fs <- list(columns = columns, valueColumns = valueColumns,
                     backgroundColor = args$backgroundColor)
      real_formatStyle(table, columns, valueColumns, ...)
    },
    styleEqual = function(levels, values, ...) {
      cap$levels <- levels
      cap$values <- values
      real_styleEqual(levels, values, ...)
    },
    .package = "DT",
    .env = env
  )
}

with_fe_server <- function(expr, ...) {
  quo <- rlang::enquo(expr)
  shiny::testServer(mod_filtering_example_server, args = list(...),
                    expr = !!quo)
}

# ---- Tests -----------------------------------------------------------------

test_that("mod_filtering_example_ui returns an empty tagList", {
  res <- mod_filtering_example_ui("x")
  
  expect_s3_class(res, "shiny.tag.list")
  expect_length(res, 0)
})

test_that("mod_filtering_example returns a shiny app", {
  skip_if_not_installed("DT")
  
  app <- mod_filtering_example(make_se_fe(), indices = 1:2,
                               operation = "delete")
  
  expect_s3_class(app, "shiny.appobj")
})

test_that("the server checks that magrittr is available", {
  skip_if_no_fe_deps()
  cap <- new.env(); mock_fe(cap)
  
  with_fe_server({}, dataIn = shiny::reactive(make_se_fe()))
  
  expect_true("magrittr" %in% cap$pkgs)
})

test_that("the server fails when magrittr is missing", {
  skip_if_no_fe_deps()
  cap <- new.env(); mock_fe(cap)
  testthat::local_mocked_bindings(
    pkgs_require = function(...) stop("missing package"),
    .package = "Prostar2"
  )
  
  expect_error(with_fe_server({}), "missing package", fixed = TRUE)
})

test_that("the server works with its default arguments", {
  skip_if_no_fe_deps()
  cap <- new.env(); mock_fe(cap)
  
  expect_no_error(
    shiny::testServer(mod_filtering_example_server, expr = {})
  )
})

test_that("the table is rendered from the enriched dataset", {
  skip_if_no_fe_deps()
  cap <- new.env(); mock_fe(cap)
  se <- make_se_fe()
  
  with_fe_server({
    expect_false(is.null(output$example_tab_filtered))
    expect_identical(cap$build, se)
    expect_s3_class(cap$dt$data, "data.frame")
    expect_equal(dim(cap$dt$data), c(3, 5))
  }, dataIn = shiny::reactive(se))
})

test_that("the table options are set", {
  skip_if_no_fe_deps()
  cap <- new.env(); mock_fe(cap)
  
  with_fe_server({
    output$example_tab_filtered
    opts <- cap$dt$args$options
    
    expect_identical(cap$dt$args$extensions, "Scroller")
    expect_identical(opts$dom, "Brtip")
    expect_equal(opts$pageLength, 15)
    expect_false(opts$bLengthChange)
    expect_true(opts$scroller)
  }, dataIn = shiny::reactive(make_se_fe()))
})

test_that("the tag columns are hidden for an enriched dataset", {
  skip_if_no_fe_deps()
  cap <- new.env(); mock_fe(cap)
  
  with_fe_server({
    output$example_tab_filtered
    
    expect_equal(cap$dt$args$options$columnDefs,
                 list(list(targets = 4:5, visible = FALSE)))
  }, dataIn = shiny::reactive(make_se_fe()))
})

test_that("no column is hidden and no colour applied for a non enriched dataset", {
  skip_if_no_fe_deps()
  cap <- new.env(); mock_fe(cap, enriched = FALSE)
  df <- make_enriched_df()[, 1:3]
  
  with_fe_server({
    output$example_tab_filtered
    
    expect_null(cap$dt$args$options$columnDefs)
    expect_null(cap$fs)
  }, dataIn = shiny::reactive(df))
})

test_that("the data columns are coloured by the tag columns", {
  skip_if_no_fe_deps()
  cap <- new.env(); mock_fe(cap)
  
  with_fe_server({
    output$example_tab_filtered
    
    expect_identical(cap$fs$columns, c("S1", "S2"))
    expect_identical(cap$fs$valueColumns, c("metacell_S1", "metacell_S2"))
  }, dataIn = shiny::reactive(make_se_fe()))
})

test_that("each tag has a colour and a darker one", {
  skip_if_no_fe_deps()
  cap <- new.env(); mock_fe(cap)
  
  with_fe_server({
    output$example_tab_filtered
    
    expect_identical(cap$levels, c("a", "b", "darken_a", "darken_b"))
    # Colours are halved: #FF0000 -> #800000, #00FF00 -> #008000
    expect_identical(cap$values,
                     c("#FF0000", "#00FF00", "#800000", "#008000"))
  }, dataIn = shiny::reactive(make_se_fe()))
})

test_that("no line is darkened without indices", {
  skip_if_no_fe_deps()
  cap <- new.env(); mock_fe(cap)
  
  with_fe_server({
    output$example_tab_filtered
    
    expect_identical(cap$dt$data, make_enriched_df())
  }, dataIn = shiny::reactive(make_se_fe()))
})

test_that("operation 'delete' darkens the lines given by indices", {
  skip_if_no_fe_deps()
  cap <- new.env(); mock_fe(cap)
  
  with_fe_server({
    output$example_tab_filtered
    d <- cap$dt$data
    
    expect_identical(d$metacell_S1, c("darken_a", "a", "a"))
    expect_identical(d$metacell_S2, c("darken_b", "b", "b"))
    # Data columns are untouched
    expect_identical(d$S1, 1:3)
  }, dataIn = shiny::reactive(make_se_fe()),
  indices = shiny::reactive(1),
  operation = shiny::reactive("delete"))
})

test_that("operation 'keep' darkens the lines that are not in indices", {
  skip_if_no_fe_deps()
  cap <- new.env(); mock_fe(cap)
  
  with_fe_server({
    output$example_tab_filtered
    
    expect_identical(cap$dt$data$metacell_S1, c("a", "a", "darken_a"))
  }, dataIn = shiny::reactive(make_se_fe()),
  indices = shiny::reactive(1:2),
  operation = shiny::reactive("keep"))
})

test_that("an unknown operation darkens nothing", {
  skip_if_no_fe_deps()
  cap <- new.env(); mock_fe(cap)
  
  with_fe_server({
    output$example_tab_filtered
    
    expect_identical(cap$dt$data, make_enriched_df())
  }, dataIn = shiny::reactive(make_se_fe()),
  indices = shiny::reactive(1:2),
  operation = shiny::reactive("other"))
})

test_that("the table follows changes of indices", {
  skip_if_no_fe_deps()
  cap <- new.env(); mock_fe(cap)
  idx <- shiny::reactiveVal(1)
  
  with_fe_server({
    output$example_tab_filtered
    expect_identical(cap$dt$data$metacell_S1, c("darken_a", "a", "a"))
    
    idx(3)
    session$flushReact()
    output$example_tab_filtered
    
    expect_identical(cap$dt$data$metacell_S1, c("a", "a", "darken_a"))
  }, dataIn = shiny::reactive(make_se_fe()), indices = idx,
  operation = shiny::reactive("delete"))
})

test_that("a plain string operation fails when indices are given (current behaviour)", {
  skip_if_no_fe_deps()
  cap <- new.env(); mock_fe(cap)
  
  # The server calls operation(), but the default is the string "keep"
  with_fe_server({
    expect_error(output$example_tab_filtered)
  }, dataIn = shiny::reactive(make_se_fe()),
  indices = shiny::reactive(1))
})
