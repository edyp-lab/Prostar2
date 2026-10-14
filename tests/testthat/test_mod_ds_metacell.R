library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

skip_if_no_mh_deps <- function() {
  skip_if_not_installed("plotly")
  skip_if_not_installed("DaparToolshed")
  skip_if_not_installed("shinyjs")
}

make_se_mh <- function() {
  SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = matrix(
      1:8, 2, dimnames = list(c("f1", "f2"), paste0("S", 1:4))
    ))
  )
}

grp4 <- function() c("A", "A", "B", "B")

# Mocks every collaborator of the server.
#   cap$resource : arguments given to addResourcePath()
#   cap$tree     : list(id, dataIn) given to mod_metacell_tree_server()
#   cap$type_obj : object given to typeDataset()
#   cap$histo, cap$lines, cap$conds : arguments given to the plot functions
# `tags` is the reactive returned by the fake tree module.
mock_mh <- function(cap, tags = shiny::reactiveVal(list(values = NULL)),
                    env = parent.frame()) {
  testthat::local_mocked_bindings(
    addResourcePath = function(prefix, directoryPath) {
      cap$resource <- list(prefix = prefix, path = directoryPath)
      invisible(NULL)
    },
    mod_metacell_tree_server = function(id, dataIn, ...) {
      cap$tree <- list(id = id, dataIn = dataIn)
      tags
    },
    mod_metacell_tree_ui = function(id) shiny::div(id = id, "tree"),
    metacellHisto_HC = function(obj, group, pattern, pal, ...) {
      cap$histo <- list(obj = obj, group = group, pattern = pattern, pal = pal)
      fake_plot()
    },
    metacellPerLinesHisto_HC = function(obj, group, pattern, indLegend, ...) {
      cap$lines <- list(obj = obj, group = group, pattern = pattern,
                        indLegend = indLegend)
      fake_plot()
    },
    metacellPerLinesHistoPerCondition_HC = function(obj, group, pattern, pal, ...) {
      cap$conds <- list(obj = obj, group = group, pattern = pattern, pal = pal)
      fake_plot()
    },
    .package = "Prostar2",
    .env = env
  )
  testthat::local_mocked_bindings(
    typeDataset = function(object, ...) {
      cap$type_obj <- object
      "protein"
    },
    .package = "DaparToolshed",
    .env = env
  )
  invisible(tags)
}

with_mh_server <- function(expr, ...) {
  quo <- rlang::enquo(expr)
  shiny::testServer(mod_ds_metacell_Histos_server, args = list(...),
                    expr = !!quo)
}

# A minimal valid plotly widget (no "no trace type" warning)
fake_plot <- function() {
  plotly::plot_ly(x = 1, y = 1, type = "scatter", mode = "markers")
}

# ---- Tests -----------------------------------------------------------------

test_that("mod_ds_metacell_Histos_ui returns a tagList", {
  skip_if_not_installed("plotly")
  
  expect_s3_class(mod_ds_metacell_Histos_ui("x"), "shiny.tag.list")
})

test_that("mod_ds_metacell_Histos_ui has the namespaced outputs", {
  skip_if_not_installed("plotly")
  
  html <- as.character(mod_ds_metacell_Histos_ui("x"))
  
  for (id in c("chooseTagUI", "histo", "histo_per_lines",
               "histo_per_lines_per_conds")) {
    expect_match(html, paste0('id="x-', id, '"'), fixed = TRUE, info = id)
  }
})

test_that("mod_ds_metacell_Histos_ui shows the three plots in three columns", {
  skip_if_not_installed("plotly")
  
  html <- as.character(mod_ds_metacell_Histos_ui("x"))
  
  expect_equal(lengths(regmatches(html, gregexpr("col-sm-4", html, fixed = TRUE))), 3)
})

test_that("mod_ds_metacell_Histos_ui uses the namespace given", {
  skip_if_not_installed("plotly")
  
  a <- as.character(mod_ds_metacell_Histos_ui("a"))
  b <- as.character(mod_ds_metacell_Histos_ui("b"))
  
  expect_match(a, 'id="a-histo"', fixed = TRUE)
  expect_no_match(a, 'id="b-histo"', fixed = TRUE)
  expect_match(b, 'id="b-histo"', fixed = TRUE)
})

test_that("mod_ds_metacell_Histos returns a shiny app", {
  skip_if_not_installed("plotly")
  
  app <- mod_ds_metacell_Histos(make_se_mh(), group = grp4(), pal = NULL)
  
  expect_s3_class(app, "shiny.appobj")
})

test_that("mod_ds_metacell_Histos accepts a predefined pattern", {
  skip_if_not_installed("plotly")
  
  app <- mod_ds_metacell_Histos(make_se_mh(), group = grp4(), pal = NULL,
                                pattern = c("Missing MEC", "Missing POV"))
  
  expect_s3_class(app, "shiny.appobj")
})

test_that("the server registers the DaparToolshed images resource path", {
  skip_if_no_mh_deps()
  cap <- new.env(); mock_mh(cap)
  
  with_mh_server({}, dataIn = shiny::reactive(make_se_mh()))
  
  expect_identical(cap$resource$prefix, "img_ds_metacell")
  expect_identical(cap$resource$path,
                   system.file("images", package = "DaparToolshed"))
})

test_that("the server starts the tree module with the id 'tree' and the dataset", {
  skip_if_no_mh_deps()
  cap <- new.env(); mock_mh(cap)
  se <- make_se_mh()
  
  with_mh_server({
    expect_identical(cap$tree$id, "tree")
    expect_identical(cap$tree$dataIn(), se)
  }, dataIn = shiny::reactive(se))
})

test_that("rv starts without tag, with showSelect TRUE and no type", {
  skip_if_no_mh_deps()
  cap <- new.env(); mock_mh(cap)
  
  with_mh_server({
    expect_null(rv$chooseTag)
    expect_true(rv$showSelect)
    expect_null(rv$type)
  })
})

test_that("rv takes the pattern given as the chosen tag", {
  skip_if_no_mh_deps()
  cap <- new.env(); mock_mh(cap)
  pat <- c("Missing MEC", "Missing POV")
  
  with_mh_server({
    expect_identical(rv$chooseTag, pat)
  }, pattern = shiny::reactive(pat))
})

test_that("showSelect() is used only when a pattern is given", {
  skip_if_no_mh_deps()
  cap <- new.env(); mock_mh(cap)
  
  # No pattern: showSelect is forced to TRUE
  with_mh_server({
    expect_true(rv$showSelect)
  }, showSelect = shiny::reactive(FALSE))
  
  # With a pattern: showSelect is used
  with_mh_server({
    expect_false(rv$showSelect)
  }, pattern = shiny::reactive("Missing MEC"),
  showSelect = shiny::reactive(FALSE))
})

test_that("the dataset type is read when a dataset is given", {
  skip_if_no_mh_deps()
  cap <- new.env(); mock_mh(cap)
  se <- make_se_mh()
  
  with_mh_server({
    session$flushReact()
    expect_identical(rv$type, "protein")
    expect_identical(cap$type_obj, se)
  }, dataIn = shiny::reactive(se))
})

test_that("the type stays NULL without dataset", {
  skip_if_no_mh_deps()
  cap <- new.env(); mock_mh(cap)
  
  with_mh_server({
    session$flushReact()
    expect_null(rv$type)
    expect_null(cap$type_obj)
  })
})

test_that("the chosen tags follow the tree module", {
  skip_if_no_mh_deps()
  cap <- new.env()
  tags <- mock_mh(cap)
  
  with_mh_server({
    session$flushReact()
    
    tags(list(values = c("Missing POV")))
    session$flushReact()
    expect_identical(rv$chooseTag, "Missing POV")
    
    tags(list(values = c("Missing POV", "Quantified")))
    session$flushReact()
    expect_identical(rv$chooseTag, c("Missing POV", "Quantified"))
  }, dataIn = shiny::reactive(make_se_mh()))
})

test_that("an empty selection in the tree resets the chosen tags", {
  skip_if_no_mh_deps()
  cap <- new.env()
  tags <- mock_mh(cap)
  
  with_mh_server({
    session$flushReact()
    tags(list(values = "Missing POV"))
    session$flushReact()
    
    tags(list(values = NULL))
    session$flushReact()
    
    expect_null(rv$chooseTag)
  }, dataIn = shiny::reactive(make_se_mh()))
})

test_that("the tag selector is shown with a dataset and no pattern", {
  skip_if_no_mh_deps()
  cap <- new.env(); mock_mh(cap)
  
  with_mh_server({
    html <- output$chooseTagUI$html
    
    expect_match(html, "Select one or several tag(s) to display statistics about",
                 fixed = TRUE)
    expect_match(html, "-tree", fixed = TRUE)
  }, dataIn = shiny::reactive(make_se_mh()))
})

test_that("the tag selector is hidden without dataset", {
  skip_if_no_mh_deps()
  cap <- new.env(); mock_mh(cap)
  
  with_mh_server({
    expect_error(output$chooseTagUI, class = "shiny.silent.error")
  })
})

test_that("the tag selector is hidden when a pattern is given", {
  skip_if_no_mh_deps()
  cap <- new.env(); mock_mh(cap)
  
  with_mh_server({
    expect_error(output$chooseTagUI, class = "shiny.silent.error")
  }, dataIn = shiny::reactive(make_se_mh()),
  pattern = shiny::reactive("Missing MEC"))
})

test_that("the three plots are rendered with a dataset and a group", {
  skip_if_no_mh_deps()
  cap <- new.env(); mock_mh(cap)
  
  with_mh_server({
    expect_false(is.null(output$histo))
    expect_false(is.null(output$histo_per_lines))
    expect_false(is.null(output$histo_per_lines_per_conds))
  }, dataIn = shiny::reactive(make_se_mh()),
  group = shiny::reactive(grp4()))
})

test_that("the plots wait for a dataset", {
  skip_if_no_mh_deps()
  cap <- new.env(); mock_mh(cap)
  
  with_mh_server({
    expect_error(output$histo, class = "shiny.silent.error")
    expect_error(output$histo_per_lines, class = "shiny.silent.error")
    expect_error(output$histo_per_lines_per_conds, class = "shiny.silent.error")
    expect_null(cap$histo)
  }, group = shiny::reactive(grp4()))
})

test_that("the plots wait for a group", {
  skip_if_no_mh_deps()
  cap <- new.env(); mock_mh(cap)
  
  with_mh_server({
    expect_error(output$histo, class = "shiny.silent.error")
    expect_error(output$histo_per_lines, class = "shiny.silent.error")
    expect_error(output$histo_per_lines_per_conds, class = "shiny.silent.error")
    expect_null(cap$lines)
  }, dataIn = shiny::reactive(make_se_mh()))
})

test_that("the histogram receives the dataset, the group, the tags and the palette", {
  skip_if_no_mh_deps()
  cap <- new.env(); mock_mh(cap)
  se <- make_se_mh()
  pat <- c("Missing MEC", "Missing POV")
  
  with_mh_server({
    output$histo
    
    expect_identical(cap$histo$obj, se)
    expect_identical(cap$histo$group, grp4())
    expect_identical(cap$histo$pattern, pat)
    expect_identical(cap$histo$pal, c("red", "blue"))
  }, dataIn = shiny::reactive(se), group = shiny::reactive(grp4()),
  pattern = shiny::reactive(pat), pal = c("red", "blue"))
})

test_that("the per-line histogram receives a legend index from 2 to the group length", {
  skip_if_no_mh_deps()
  cap <- new.env(); mock_mh(cap)
  
  with_mh_server({
    output$histo_per_lines
    
    expect_identical(cap$lines$group, grp4())
    expect_identical(cap$lines$indLegend, 2:4)
  }, dataIn = shiny::reactive(make_se_mh()),
  group = shiny::reactive(grp4()))
})

test_that("the per-condition histogram receives the dataset, group, tags and palette", {
  skip_if_no_mh_deps()
  cap <- new.env(); mock_mh(cap)
  se <- make_se_mh()
  
  with_mh_server({
    output$histo_per_lines_per_conds
    
    expect_identical(cap$conds$obj, se)
    expect_identical(cap$conds$group, grp4())
    expect_null(cap$conds$pal)
  }, dataIn = shiny::reactive(se), group = shiny::reactive(grp4()))
})

test_that("the plots use the tags chosen in the tree", {
  skip_if_no_mh_deps()
  cap <- new.env()
  tags <- mock_mh(cap)
  
  with_mh_server({
    session$flushReact()
    tags(list(values = "Missing POV"))
    session$flushReact()
    
    output$histo
    output$histo_per_lines
    output$histo_per_lines_per_conds
    
    expect_identical(cap$histo$pattern, "Missing POV")
    expect_identical(cap$lines$pattern, "Missing POV")
    expect_identical(cap$conds$pattern, "Missing POV")
  }, dataIn = shiny::reactive(make_se_mh()),
  group = shiny::reactive(grp4()))
})

test_that("the plots are redrawn when the group changes", {
  skip_if_no_mh_deps()
  cap <- new.env(); mock_mh(cap)
  grp <- shiny::reactiveVal(grp4())
  
  with_mh_server({
    output$histo
    expect_identical(cap$histo$group, grp4())
    
    grp(c("X", "X", "Y", "Y"))
    session$flushReact()
    output$histo
    
    expect_identical(cap$histo$group, c("X", "X", "Y", "Y"))
  }, dataIn = shiny::reactive(make_se_mh()), group = grp)
})

test_that("a plot error is reported, not hidden", {
  skip_if_no_mh_deps()
  cap <- new.env(); mock_mh(cap)
  testthat::local_mocked_bindings(
    metacellHisto_HC = function(...) stop("plot failed"),
    .package = "Prostar2"
  )
  
  with_mh_server({
    expect_error(output$histo, "plot failed", fixed = TRUE)
  }, dataIn = shiny::reactive(make_se_mh()),
  group = shiny::reactive(grp4()))
})

test_that("the server works with its default arguments", {
  skip_if_no_mh_deps()
  cap <- new.env(); mock_mh(cap)
  
  shiny::testServer(mod_ds_metacell_Histos_server, expr = {
    expect_error(output$histo, class = "shiny.silent.error")
    expect_error(output$chooseTagUI, class = "shiny.silent.error")
  })
})
