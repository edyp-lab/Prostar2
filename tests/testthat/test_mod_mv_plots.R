library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

skip_if_no_mv_deps <- function() {
  skip_if_not_installed("plotly")
  skip_if_not_installed("DaparToolshed")
}

make_se_mv <- function(offset = 0) {
  SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = matrix(
      seq_len(8) + offset, 2,
      dimnames = list(c("f1", "f2"), paste0("S", 1:4))
    ))
  )
}

grp_mv <- function() c("A", "A", "B", "B")

fake_plot_mv <- function() {
  plotly::plot_ly(x = 1, y = 1, type = "scatter", mode = "markers")
}

# Mocks the collaborators.
#   cap$hc      : arguments given to hc_mvTypePlot2()
#   cap$image   : arguments given to wrapperMVImage()
#   cap$colors  : value given to GetColorsForConditions()
#   cap$pkgs    : packages given to pkgs_require()
# `meta` is the matrix returned by qMetacell().
mock_mv <- function(cap, meta = matrix("Quantified", 2, 4),
                    env = parent.frame()) {
  testthat::local_mocked_bindings(
    hc_mvTypePlot2 = function(obj, group, pattern, title, pal, ...) {
      cap$hc <- list(obj = obj, group = group, pattern = pattern,
                     title = title, pal = pal)
      fake_plot_mv()
    },
    wrapperMVImage = function(obj, group, ...) {
      cap$image <- list(obj = obj, group = group)
      graphics::plot(1)
      invisible(NULL)
    },
    GetColorsForConditions = function(x, ...) {
      cap$colors <- x
      c("#111111", "#222222")
    },
    pkgs_require = function(ll.deps) {
      cap$pkgs <- c(cap$pkgs, ll.deps)
      invisible(NULL)
    },
    .package = "Prostar2",
    .env = env
  )
  testthat::local_mocked_bindings(
    qMetacell = function(object, ...) meta,
    .package = "DaparToolshed",
    .env = env
  )
}

with_mv_server <- function(expr, ...) {
  quo <- rlang::enquo(expr)
  shiny::testServer(mod_mv_plots_server, args = list(...), expr = !!quo)
}

# ---- Tests -----------------------------------------------------------------

test_that("mod_mv_plots_ui returns a tagList", {
  skip_if_not_installed("plotly")
  
  expect_s3_class(mod_mv_plots_ui("x"), "shiny.tag.list")
})

test_that("mod_mv_plots_ui has the namespaced outputs", {
  skip_if_not_installed("plotly")
  
  html <- as.character(mod_mv_plots_ui("x"))
  
  for (id in c("plot_viewNAbyMean", "WarnForImageNA", "plot_showImageNA")) {
    expect_match(html, paste0('id="x-', id, '"'), fixed = TRUE, info = id)
  }
})

test_that("mod_mv_plots_ui shows the two plots side by side with a width of 600px", {
  skip_if_not_installed("plotly")
  
  html <- as.character(mod_mv_plots_ui("x"))
  
  expect_equal(
    lengths(regmatches(html, gregexpr("display:inline-block", html, fixed = TRUE))),
    2
  )
  expect_equal(
    lengths(regmatches(html, gregexpr("width:600px", html, fixed = TRUE))),
    2
  )
})

test_that("mod_mv_plots_ui uses the namespace given", {
  skip_if_not_installed("plotly")
  
  a <- as.character(mod_mv_plots_ui("a"))
  b <- as.character(mod_mv_plots_ui("b"))
  
  expect_match(a, 'id="a-plot_viewNAbyMean"', fixed = TRUE)
  expect_no_match(a, 'id="b-plot_viewNAbyMean"', fixed = TRUE)
  expect_match(b, 'id="b-plot_viewNAbyMean"', fixed = TRUE)
})

test_that("mod_mv_plots returns a shiny app", {
  skip_if_not_installed("plotly")
  
  expect_s3_class(mod_mv_plots(make_se_mv(), grp = grp_mv(),
                               pattern = "Missing POV"),
                  "shiny.appobj")
})

test_that("the server works with its default arguments", {
  skip_if_no_mv_deps()
  cap <- new.env(); mock_mv(cap)
  
  expect_no_error(shiny::testServer(mod_mv_plots_server, expr = {
    session$flushReact()
    expect_error(output$plot_viewNAbyMean, class = "shiny.silent.error")
  }))
})

test_that("rv starts empty", {
  skip_if_no_mv_deps()
  cap <- new.env(); mock_mv(cap)
  
  with_mv_server({
    expect_null(rv$grp)
    expect_null(rv$mytitle)
    expect_null(rv$palette)
    expect_null(rv$pattern)
  })
})

test_that("the given group, title, palette and pattern are stored", {
  skip_if_no_mv_deps()
  cap <- new.env(); mock_mv(cap)
  
  with_mv_server({
    session$flushReact()
    
    expect_identical(rv$grp, grp_mv())
    expect_identical(rv$mytitle, "My plot")
    expect_identical(rv$palette, c("red", "blue"))
    expect_identical(rv$pattern, "Missing POV")
    # GetColorsForConditions() is not needed when a palette is given
    expect_null(cap$colors)
  }, data = shiny::reactive(make_se_mv()),
  grp = shiny::reactive(grp_mv()),
  mytitle = shiny::reactive("My plot"),
  pal = c("red", "blue"),
  pattern = shiny::reactive("Missing POV"))
})

test_that("default group names are built from the number of samples", {
  skip_if_no_mv_deps()
  cap <- new.env(); mock_mv(cap)
  
  with_mv_server({
    session$flushReact()
    
    expect_identical(rv$grp, paste0("grp_", 1:4))
  }, data = shiny::reactive(make_se_mv()))
})

test_that("the default title is 'my title'", {
  skip_if_no_mv_deps()
  cap <- new.env(); mock_mv(cap)
  
  with_mv_server({
    session$flushReact()
    
    expect_identical(rv$mytitle, "my title")
  }, data = shiny::reactive(make_se_mv()))
})

test_that("the palette is built from the group when none is given", {
  skip_if_no_mv_deps()
  cap <- new.env(); mock_mv(cap)
  
  with_mv_server({
    session$flushReact()
    
    expect_identical(rv$palette, c("#111111", "#222222"))
    expect_identical(cap$colors, grp_mv())
  }, data = shiny::reactive(make_se_mv()),
  grp = shiny::reactive(grp_mv()))
})

test_that("the palette uses a NULL group when no group is given (current behaviour)", {
  skip_if_no_mv_deps()
  cap <- new.env(); mock_mv(cap)
  
  # The server calls GetColorsForConditions(grp()) and not rv$grp, so the
  # default group names are not used to build the palette
  with_mv_server({
    session$flushReact()
    
    expect_null(cap$colors)
  }, data = shiny::reactive(make_se_mv()))
})

test_that("the pattern is NULL when none is given", {
  skip_if_no_mv_deps()
  cap <- new.env(); mock_mv(cap)
  
  with_mv_server({
    session$flushReact()
    
    expect_null(rv$pattern)
  }, data = shiny::reactive(make_se_mv()))
})

test_that("a dataset which is not a SummarizedExperiment is ignored", {
  skip_if_no_mv_deps()
  cap <- new.env(); mock_mv(cap)
  
  with_mv_server({
    session$flushReact()
    
    expect_null(rv$grp)
    expect_null(rv$mytitle)
  }, data = shiny::reactive(list(a = 1)))
})

test_that("the values follow a dataset that arrives later", {
  skip_if_no_mv_deps()
  cap <- new.env(); mock_mv(cap)
  data <- shiny::reactiveVal(NULL)
  
  with_mv_server({
    session$flushReact()
    expect_null(rv$grp)
    
    data(make_se_mv())
    session$flushReact()
    
    expect_identical(rv$grp, paste0("grp_", 1:4))
  }, data = data)
})

test_that("the NA by mean plot is rendered with the stored values", {
  skip_if_no_mv_deps()
  cap <- new.env(); mock_mv(cap)
  se <- make_se_mv()
  
  with_mv_server({
    expect_false(is.null(output$plot_viewNAbyMean))
    
    expect_identical(cap$hc$obj, se)
    expect_identical(cap$hc$group, grp_mv())
    expect_identical(cap$hc$pattern, "Missing POV")
    expect_identical(cap$hc$title, "My plot")
    expect_identical(cap$hc$pal, c("red", "blue"))
  }, data = shiny::reactive(se),
  grp = shiny::reactive(grp_mv()),
  mytitle = shiny::reactive("My plot"),
  pal = c("red", "blue"),
  pattern = shiny::reactive("Missing POV"))
})

test_that("the NA by mean plot waits for a dataset", {
  skip_if_no_mv_deps()
  cap <- new.env(); mock_mv(cap)
  
  with_mv_server({
    expect_error(output$plot_viewNAbyMean, class = "shiny.silent.error")
    expect_null(cap$hc)
  }, pattern = shiny::reactive("Missing POV"))
})

test_that("the NA by mean plot waits for a pattern", {
  skip_if_no_mv_deps()
  cap <- new.env(); mock_mv(cap)
  
  with_mv_server({
    expect_error(output$plot_viewNAbyMean, class = "shiny.silent.error")
    expect_null(cap$hc)
  }, data = shiny::reactive(make_se_mv()))
})

test_that("the NA by mean plot reports an error of the plot function", {
  skip_if_no_mv_deps()
  cap <- new.env(); mock_mv(cap)
  testthat::local_mocked_bindings(
    hc_mvTypePlot2 = function(...) stop("plot failed"),
    .package = "Prostar2"
  )
  
  with_mv_server({
    expect_error(output$plot_viewNAbyMean, "plot failed", fixed = TRUE)
  }, data = shiny::reactive(make_se_mv()),
  pattern = shiny::reactive("Missing POV"))
})

test_that("the MEC image is not drawn without 'Missing MEC' tags", {
  skip_if_no_mv_deps()
  cap <- new.env(); mock_mv(cap, meta = matrix("Quantified", 2, 4))
  
  with_mv_server({
    session$flushReact()
    
    expect_error(output$plot_showImageNA, class = "shiny.silent.error")
    expect_null(cap$image)
  }, data = shiny::reactive(make_se_mv()),
  grp = shiny::reactive(grp_mv()))
})

test_that("the MEC image waits for a dataset", {
  skip_if_no_mv_deps()
  cap <- new.env(); mock_mv(cap)
  
  with_mv_server({
    expect_error(output$plot_showImageNA, class = "shiny.silent.error")
  })
})

test_that("the MEC image is drawn when there are 'Missing MEC' tags", {
  skip_if_no_mv_deps()
  meta <- matrix("Quantified", 2, 4)
  meta[1, 2] <- "Missing MEC"
  cap <- new.env(); mock_mv(cap, meta = meta)
  se <- make_se_mv()
  
  with_mv_server({
    session$flushReact()
    res <- output$plot_showImageNA
    
    expect_false(is.null(res))
    expect_identical(res$alt, "This is alternate text")
    expect_identical(cap$image$obj, se)
    expect_identical(cap$image$group, grp_mv())
    expect_true("grDevices" %in% cap$pkgs)
  }, data = shiny::reactive(se),
  grp = shiny::reactive(grp_mv()))
})
