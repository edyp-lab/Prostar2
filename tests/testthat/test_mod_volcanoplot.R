library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

skip_if_no_vp_deps <- function() {
  skip_if_not_installed("MagellanNTK")
  skip_if_not_installed("DaparToolshed")
  skip_if_not_installed("omXplore")
  skip_if_not_installed("plotly")
  skip_if_not_installed("DT")
  skip_if_not_installed("shinyjqui")
}

# 3 features x 4 samples, with a 'Protein' annotation column
make_se_vp <- function(offset = 0) {
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = matrix(
      seq_len(12) + offset, 3,
      dimnames = list(paste0("p", 1:3), paste0("S", 1:4))
    ))
  )
  SummarizedExperiment::rowData(se)$Protein <- paste0("Prot", 1:3)
  se
}

grp_vp  <- function() c("A", "A", "B", "B")
comp_vp <- function() c("A", "B")

# Fake hypothesis test table. The 3rd p-value is a "pushed" one (> 1).
ht_vp <- function() {
  data.frame(A_vs_B_logFC = c(2, -1, 0.5),
             A_vs_B_pval  = c(0.001, 0.05, 1.00000000001))
}

# Fake enriched table: 4 peptides, 2 data columns + 2 tag columns
enriched_vp <- function() {
  data.frame(S1 = 1:4, S2 = 5:8, metacell_S1 = "a", metacell_S2 = "b",
             row.names = paste0("pep", 1:4), stringsAsFactors = FALSE)
}

# Adjacency matrix (4 peptides x 2 proteins) and its "specific" version
adj_vp  <- function() matrix(c(1, 1, 1, 0,
                               0, 0, 1, 1), 4, 2,
                             dimnames = list(paste0("pep", 1:4), c("P1", "P2")))
spec_vp <- function() matrix(c(1, 0, 0, 0,
                               0, 0, 0, 1), 4, 2,
                             dimnames = list(paste0("pep", 1:4), c("P1", "P2")))

fake_plot_vp <- function() {
  plotly::plot_ly(x = 1, y = 1, type = "scatter", mode = "markers")
}

# Mocks the collaborators.
#   cap$pkgs   : packages given to pkgs_require()
#   cap$volcano: arguments given to diffAnaVolcanoplot_rCharts()
#   cap$ht_obj : object given to HypothesisTest()
mock_vp <- function(cap, type = "protein", env = parent.frame()) {
  testthat::local_mocked_bindings(
    pkgs_require = function(ll.deps) {
      cap$pkgs <- ll.deps
      invisible(NULL)
    },
    HypothesisTest = function(object, ...) {
      cap$ht_obj <- object
      ht_vp()
    },
    Build_enriched_qdata = function(obj, digits = NULL, ...) enriched_vp(),
    ExtractUniquePeptides = function(x, ...) spec_vp(),
    BuildColorStyles = function(typeDataset) c(a = "#FF0000", b = "#00FF00"),
    .package = "Prostar2",
    .env = env
  )
  testthat::local_mocked_bindings(
    typeDataset = function(object, ...) type,
    diffAnaVolcanoplot_rCharts = function(df, ...) {
      cap$volcano <- list(df = df, args = list(...))
      fake_plot_vp()
    },
    .package = "DaparToolshed",
    .env = env
  )
  testthat::local_mocked_bindings(
    get_type = function(object, ...) type,
    .package = "omXplore",
    .env = env
  )
  testthat::local_mocked_bindings(
    initComplete = function(...) DT::JS("function(settings, json) {}"),
    .package = "MagellanNTK",
    .env = env
  )
  testthat::local_mocked_bindings(
    adjacencyMatrix = function(object, ...) adj_vp(),
    .package = "QFeatures",
    .env = env
  )
}

with_vp_server <- function(expr, ...) {
  quo <- rlang::enquo(expr)
  args <- modifyList(
    list(dataIn = shiny::reactive(make_se_vp()),
         comparison = shiny::reactive(comp_vp()),
         group = shiny::reactive(grp_vp())),
    list(...)
  )
  shiny::testServer(mod_volcanoplot_server, args = args, expr = !!quo)
}

click_protein <- function(rv.custom) {
  rv.custom$dataGetExprsClickedProtein <- data.frame(S1 = 1, row.names = "P1")
}

# ---- Tests -----------------------------------------------------------------

test_that("the UI returns a tagList", {
  skip_if_not_installed("plotly")
  
  expect_s3_class(mod_volcanoplot_ui("x"), "shiny.tag.list")
})

test_that("the UI has the namespaced outputs", {
  skip_if_not_installed("plotly")
  
  html <- as.character(mod_volcanoplot_ui("x"))
  
  expect_match(html, 'id="x-volcanoPlot_UI"', fixed = TRUE)
  expect_match(html, 'id="x-quantiDT"', fixed = TRUE)
})

test_that("the plot is 600px by 600px", {
  skip_if_not_installed("plotly")
  
  html <- as.character(mod_volcanoplot_ui("x"))
  
  expect_match(html, "width:600px", fixed = TRUE)
  expect_match(html, "height:600px", fixed = TRUE)
})

test_that("the UI uses the namespace given", {
  skip_if_not_installed("plotly")
  
  a <- as.character(mod_volcanoplot_ui("a"))
  b <- as.character(mod_volcanoplot_ui("b"))
  
  expect_match(a, 'id="a-volcanoPlot_UI"', fixed = TRUE)
  expect_no_match(a, 'id="b-volcanoPlot_UI"', fixed = TRUE)
  expect_match(b, 'id="b-volcanoPlot_UI"', fixed = TRUE)
})

test_that("the demo app is built", {
  skip_if_not_installed("plotly")
  
  app <- volcanoplot(make_se_vp(), comparison = comp_vp(), group = grp_vp(),
                     thlogfc = 1, thpval = 1)
  
  expect_s3_class(app, "shiny.appobj")
})

test_that("the server checks that magrittr and shinyjqui are available", {
  skip_if_no_vp_deps()
  cap <- new.env(); mock_vp(cap)
  
  with_vp_server({})
  
  expect_identical(cap$pkgs, c("magrittr", "shinyjqui"))
})

test_that("the server works with its default arguments", {
  skip_if_no_vp_deps()
  cap <- new.env(); mock_vp(cap)
  
  expect_no_error(shiny::testServer(mod_volcanoplot_server, expr = {
    session$flushReact()
    expect_error(output$volcanoPlot_UI, class = "shiny.silent.error")
  }))
})

test_that("the dataset and its hypothesis test table are stored", {
  skip_if_no_vp_deps()
  cap <- new.env(); mock_vp(cap)
  se <- make_se_vp()
  
  with_vp_server({
    session$flushReact()
    
    expect_identical(rv$dataIn, se)
    expect_identical(rv.custom$data, ht_vp())
    expect_identical(cap$ht_obj, se)
  }, dataIn = shiny::reactive(se))
})

test_that("nothing is stored without dataset", {
  skip_if_no_vp_deps()
  cap <- new.env(); mock_vp(cap)
  
  with_vp_server({
    session$flushReact()
    
    expect_null(rv$dataIn)
    expect_null(rv.custom$data)
  }, dataIn = shiny::reactive(NULL))
})

test_that("the sorting indices wait for a comparison", {
  skip_if_no_vp_deps()
  cap <- new.env(); mock_vp(cap)
  
  with_vp_server({
    expect_error(GetSortingIndices(), class = "shiny.silent.error")
  }, comparison = shiny::reactive(NULL))
})

test_that("the sorting indices put the first condition then the second one", {
  skip_if_no_vp_deps()
  cap <- new.env(); mock_vp(cap)
  
  with_vp_server({
    expect_identical(GetSortingIndices(), c(3L, 4L, 1L, 2L))
  }, comparison = shiny::reactive(c("B", "A")))
})

test_that("the sorting indices drop the unused conditions", {
  skip_if_no_vp_deps()
  cap <- new.env(); mock_vp(cap)
  
  with_vp_server({
    expect_identical(GetSortingIndices(), c(1L, 2L, 5L, 6L))
  }, group = shiny::reactive(c("A", "A", "B", "B", "C", "C")),
  comparison = shiny::reactive(c("A", "C")))
})

test_that("a 'one vs all' comparison puts the other samples after the first condition", {
  skip_if_no_vp_deps()
  cap <- new.env(); mock_vp(cap)
  
  with_vp_server({
    expect_identical(GetSortingIndices(), c(1L, 2L, 3L, 4L, 5L))
  }, group = shiny::reactive(c("A", "A", "B", "B", "C")),
  comparison = shiny::reactive(c("A", "(all-A)")))
})

test_that("the border indices give the first column of each condition", {
  skip_if_no_vp_deps()
  cap <- new.env(); mock_vp(cap)
  
  with_vp_server({
    expect_identical(GetBorderIndices(), c(1L, 3L))
  })
})

test_that("the volcano plot waits for a dataset and a comparison", {
  skip_if_no_vp_deps()
  cap <- new.env(); mock_vp(cap)
  
  with_vp_server({
    expect_error(output$volcanoPlot_UI, class = "shiny.silent.error")
  }, dataIn = shiny::reactive(NULL))
  
  with_vp_server({
    expect_error(output$volcanoPlot_UI, class = "shiny.silent.error")
  }, comparison = shiny::reactive(NULL))
  
  expect_null(cap$volcano)
})

test_that("the volcano plot is rendered", {
  skip_if_no_vp_deps()
  cap <- new.env(); mock_vp(cap)
  
  with_vp_server({
    expect_false(is.null(output$volcanoPlot_UI))
  })
})

test_that("the volcano plot is built from the logFC and -log10 of the p-values", {
  skip_if_no_vp_deps()
  cap <- new.env(); mock_vp(cap)
  
  with_vp_server({
    output$volcanoPlot_UI
    df <- cap$volcano$df
    
    expect_equal(df$x, c(2, -1, 0.5))
    expect_equal(df$y[1:2], -log10(c(0.001, 0.05)))
    expect_identical(df$index, 1:3)
  })
})

test_that("a pushed p-value gives y = 0, not a negative value", {
  skip_if_no_vp_deps()
  cap <- new.env(); mock_vp(cap)
  
  with_vp_server({
    output$volcanoPlot_UI
    
    expect_equal(cap$volcano$df$y[3], 0)
    expect_true(all(cap$volcano$df$y >= 0))
  })
})

test_that("the thresholds and the comparison are given to the plot function", {
  skip_if_no_vp_deps()
  cap <- new.env(); mock_vp(cap)
  
  with_vp_server({
    output$volcanoPlot_UI
    a <- cap$volcano$args
    
    expect_identical(a$th_logfc, 1.5)
    expect_identical(a$th_pval, 2)
    expect_identical(a$conditions, comp_vp())
  }, thlogfc = shiny::reactive("1.5"), thpval = shiny::reactive(2))
})

test_that("tooltip columns are added with the prefix 'tooltip_'", {
  skip_if_no_vp_deps()
  cap <- new.env(); mock_vp(cap)
  
  with_vp_server({
    output$volcanoPlot_UI
    df <- cap$volcano$df
    
    expect_identical(colnames(df), c("x", "y", "index", "tooltip_Protein"))
    expect_identical(as.character(df$tooltip_Protein), paste0("Prot", 1:3))
  }, tooltip = shiny::reactive("Protein"))
})

test_that("no tooltip column is added without tooltip or with NA", {
  skip_if_no_vp_deps()
  cap <- new.env(); mock_vp(cap)
  
  with_vp_server({
    output$volcanoPlot_UI
    expect_identical(colnames(cap$volcano$df), c("x", "y", "index"))
  })
  
  cap2 <- new.env(); mock_vp(cap2)
  with_vp_server({
    output$volcanoPlot_UI
    expect_identical(colnames(cap2$volcano$df), c("x", "y", "index"))
  }, tooltip = shiny::reactive(NA_character_))
})

test_that("a plot error is reported, not hidden", {
  skip_if_no_vp_deps()
  cap <- new.env(); mock_vp(cap)
  testthat::local_mocked_bindings(
    diffAnaVolcanoplot_rCharts = function(...) stop("plot failed"),
    .package = "DaparToolshed"
  )
  
  with_vp_server({
    expect_error(output$volcanoPlot_UI, "plot failed", fixed = TRUE)
  })
})
