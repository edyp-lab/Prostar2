library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

skip_if_no_example_deps <- function() {
  skip_if_not_installed("rhandsontable")
  skip_if_not_installed("magrittr")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("DaparToolshed")
  skip_if_not(
    "ExtendPalette" %in% getNamespaceExports("DaparToolshed"),
    "ExtendPalette() is not exported by DaparToolshed"
  )
}

# Renders the example for a given n and returns the widget as a list
render_example <- function(n) {
  out <- NULL
  shiny::testServer(
    mod_designExample_server,
    args = list(n = n),
    expr = {
      out <<- output$examples
    }
  )
  if (is.character(out)) out <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  out
}

# Returns the table as a character matrix (rows x columns) and its headers,
# whatever the orientation used by rhandsontable (rows or columns)
example_table <- function(widget) {
  x <- widget$x
  headers <- unlist(x$colHeaders)
  m <- do.call(rbind, lapply(x$data, function(el) as.character(unlist(el))))
  if (ncol(m) != length(headers)) m <- t(m)
  colnames(m) <- headers
  m
}

# ---- Tests -----------------------------------------------------------------

test_that("mod_designExample_ui returns a namespaced rhandsontable output", {
  skip_if_not_installed("rhandsontable")
  
  res <- mod_designExample_ui("x")
  html <- as.character(res)
  
  expect_s3_class(res, "shiny.tag.list")
  expect_match(html, 'id="x-examples"', fixed = TRUE)
  expect_match(html, "rhandsontable", fixed = TRUE)
})

test_that("mod_designExample_ui uses the namespace given", {
  skip_if_not_installed("rhandsontable")
  
  a <- as.character(mod_designExample_ui("a"))
  b <- as.character(mod_designExample_ui("b"))
  
  expect_match(a, 'id="a-examples"', fixed = TRUE)
  expect_no_match(a, 'id="b-examples"', fixed = TRUE)
  expect_match(b, 'id="b-examples"', fixed = TRUE)
})

test_that("mod_designExample returns a shiny app", {
  skip_if_not_installed("rhandsontable")
  
  expect_s3_class(mod_designExample(2), "shiny.appobj")
  expect_s3_class(mod_designExample(3), "shiny.appobj")
})

test_that("mod_designExample can be built with its default n", {
  skip_if_not_installed("rhandsontable")
  
  # Only builds the app: the default n = 1 is not rendered here
  expect_s3_class(mod_designExample(), "shiny.appobj")
})

test_that("the server renders a widget for n = 2 and n = 3", {
  skip_if_no_example_deps()
  
  expect_false(is.null(render_example(2)))
  expect_false(is.null(render_example(3)))
})

test_that("the 2 levels example has 14 rows and the expected columns", {
  skip_if_no_example_deps()
  
  m <- example_table(render_example(2))
  
  expect_equal(dim(m), c(14, 4))
  expect_identical(colnames(m),
                   c("quantCols", "Condition", "Bio.Rep", "Tech.Rep"))
})

test_that("the 3 levels example has 16 rows and the expected columns", {
  skip_if_no_example_deps()
  
  m <- example_table(render_example(3))
  
  expect_equal(dim(m), c(16, 5))
  expect_identical(colnames(m),
                   c("quantCols", "Condition", "Bio.Rep", "Tech.Rep",
                     "Analyt.Rep"))
})

test_that("the sample names are 'Sample 1', 'Sample 2', ...", {
  skip_if_no_example_deps()
  
  expect_identical(example_table(render_example(2))[, "quantCols"],
                   paste0("Sample ", 1:14))
  expect_identical(example_table(render_example(3))[, "quantCols"],
                   paste0("Sample ", 1:16))
})

test_that("the conditions of the examples are as documented", {
  skip_if_no_example_deps()
  
  expect_identical(example_table(render_example(2))[, "Condition"],
                   c(rep("A", 4), rep("B", 4), rep("C", 6)))
  expect_identical(example_table(render_example(3))[, "Condition"],
                   c(rep("A", 8), rep("B", 8)))
})

test_that("the replicate columns of the 2 levels example are consistent", {
  skip_if_no_example_deps()
  
  m <- example_table(render_example(2))
  
  expect_equal(as.numeric(m[, "Bio.Rep"]),
               c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7))
  expect_equal(as.numeric(m[, "Tech.Rep"]), 1:14)
})

test_that("the replicate columns of the 3 levels example are consistent", {
  skip_if_no_example_deps()
  
  m <- example_table(render_example(3))
  
  expect_equal(as.numeric(m[, "Bio.Rep"]), rep(1:4, each = 4))
  expect_equal(as.numeric(m[, "Tech.Rep"]), rep(1:8, each = 2))
  expect_equal(as.numeric(m[, "Analyt.Rep"]), 1:16)
})

test_that("each Bio.Rep belongs to a single condition", {
  skip_if_no_example_deps()
  
  for (n in 2:3) {
    m <- example_table(render_example(n))
    per_rep <- tapply(m[, "Condition"], m[, "Bio.Rep"],
                      function(x) length(unique(x)))
    expect_true(all(per_rep == 1), info = paste("n =", n))
  }
})

test_that("the example tables are read-only", {
  skip_if_no_example_deps()
  
  x <- render_example(2)$x
  ro <- vapply(x$columns, function(cl) isTRUE(cl$readOnly), logical(1))
  
  expect_true(all(ro))
})

test_that("the example tables use a colour renderer", {
  skip_if_no_example_deps()
  
  txt <- jsonlite::toJSON(render_example(3), auto_unbox = TRUE, null = "null")
  
  expect_match(txt, "TextRenderer", fixed = TRUE)
  expect_match(txt, "lightgrey", fixed = TRUE)
})

test_that("the 2 levels and 3 levels examples are different", {
  skip_if_no_example_deps()
  
  expect_false(identical(render_example(2), render_example(3)))
})

test_that("the server fails for n other than 2 or 3", {
  skip_if_no_example_deps()
  
  # `data` is never defined in that case
  expect_error(
    shiny::testServer(
      mod_designExample_server,
      args = list(n = 1),
      expr = { output$examples }
    )
  )
})

test_that("the server fails when a required package is missing", {
  skip_if_no_example_deps()
  
  testthat::local_mocked_bindings(
    pkgs_require = function(...) stop("missing package"),
    .package = "Prostar2"
  )
  
  expect_error(
    shiny::testServer(mod_designExample_server, args = list(n = 2),
                      expr = {}),
    "missing package",
    fixed = TRUE
  )
})

test_that("the server checks for magrittr and rhandsontable", {
  skip_if_no_example_deps()
  received <- NULL
  testthat::local_mocked_bindings(
    pkgs_require = function(ll.deps) received <<- ll.deps,
    .package = "Prostar2"
  )
  
  shiny::testServer(mod_designExample_server, args = list(n = 2), expr = {})
  
  expect_identical(received, c("magrittr", "rhandsontable"))
})
