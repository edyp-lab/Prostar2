library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

skip_if_no_mt_deps <- function() {
  skip_if_not_installed("DaparToolshed")
  skip_if_not_installed("shinyjs")
}

# The UI registers the "images" resource path of DaparToolshed
skip_if_no_images <- function() {
  skip_if_not_installed("DaparToolshed")
  skip_if(system.file("images", package = "DaparToolshed") == "",
          "DaparToolshed has no 'images' folder")
}

make_se_mt <- function(offset = 0) {
  SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = matrix(
      seq_len(4) + offset, 2,
      dimnames = list(c("f1", "f2"), c("S1", "S2"))
    ))
  )
}

# Fake metacell definition (protein tree). "Any" is the root.
nodes_mt <- c("Any", "Quantified", "Quant. by direct id",
              "Quant. by recovery", "Missing", "Missing POV", "Missing MEC",
              "Imputed", "Imputed POV", "Imputed MEC", "Combined tags")

make_meta_mt <- function() {
  data.frame(
    node   = nodes_mt,
    parent = c(NA, "Any", "Quantified", "Quantified", "Any", "Missing",
               "Missing", "Any", "Imputed", "Imputed", "Any"),
    color  = sprintf("#%02X%02X%02X", seq_along(nodes_mt) * 10, 0, 0),
    stringsAsFactors = FALSE
  )
}

# Fake number of cells per tag (sum = 100)
nb_mt <- function() {
  c("Quantified" = 30, "Quant. by direct id" = 10, "Quant. by recovery" = 10,
    "Missing" = 20, "Missing POV" = 5, "Missing MEC" = 5,
    "Imputed" = 10, "Imputed POV" = 3, "Imputed MEC" = 2,
    "Combined tags" = 5)
}

# Mocks the DaparToolshed functions used by the server.
#   cap$type_obj : object given to typeDataset()
#   cap$level    : level given to metacellDef()
mock_mt <- function(cap, type = "protein", env = parent.frame()) {
  testthat::local_mocked_bindings(
    typeDataset = function(object, ...) {
      cap$type_obj <- object
      type
    },
    metacellDef = function(level, ...) {
      cap$level <- level
      make_meta_mt()
    },
    GetNbTags = function(object, ...) nb_mt(),
    .package = "DaparToolshed",
    .env = env
  )
}

with_mt_server <- function(expr, ...) {
  quo <- rlang::enquo(expr)
  shiny::testServer(mod_metacell_tree_server, args = list(...), expr = !!quo)
}

# ---- Tests -----------------------------------------------------------------

test_that("mod_metacell_tree_ui returns a shiny page", {
  skip_if_no_images()
  
  res <- mod_metacell_tree_ui("x")
  
  expect_true(inherits(res, c("shiny.tag", "shiny.tag.list")))
})

test_that("mod_metacell_tree_ui has the namespaced elements", {
  skip_if_no_images()
  
  html <- as.character(mod_metacell_tree_ui("x"))
  
  for (id in c("openModalBtn", "selectedNodes", "modaltree")) {
    expect_match(html, paste0('id="x-', id, '"'), fixed = TRUE, info = id)
  }
})

test_that("mod_metacell_tree_ui shows the metacell image on the button", {
  skip_if_no_images()
  
  html <- as.character(mod_metacell_tree_ui("x"))
  
  expect_match(html, "images/ds_metacell.png", fixed = TRUE)
})

test_that("mod_metacell_tree_ui registers the 'images' resource path", {
  skip_if_no_images()
  
  mod_metacell_tree_ui("x")
  
  expect_true("images" %in% names(shiny::resourcePaths()))
})

test_that("mod_metacell_tree_ui uses the namespace given", {
  skip_if_no_images()
  
  a <- as.character(mod_metacell_tree_ui("a"))
  b <- as.character(mod_metacell_tree_ui("b"))
  
  expect_match(a, 'id="a-modaltree"', fixed = TRUE)
  expect_no_match(a, 'id="b-modaltree"', fixed = TRUE)
  expect_match(b, 'id="b-modaltree"', fixed = TRUE)
})

test_that("mod_metacell_tree returns a shiny app", {
  expect_s3_class(mod_metacell_tree(make_se_mt()), "shiny.appobj")
})

test_that("the server works with its default arguments", {
  skip_if_no_mt_deps()
  cap <- new.env(); mock_mt(cap)
  
  expect_no_error(shiny::testServer(mod_metacell_tree_server, expr = {
    session$flushReact()
  }))
})

test_that("without dataset the returned selection is empty", {
  skip_if_no_mt_deps()
  cap <- new.env(); mock_mt(cap)
  
  with_mt_server({
    session$flushReact()
    res <- session$returned()
    
    expect_null(res$trigger)
    expect_null(res$values)
    expect_null(rv$mapping)
    expect_null(rv$tags)
  })
})

test_that("without dataset the tree shows a message", {
  skip_if_no_mt_deps()
  cap <- new.env(); mock_mt(cap)
  
  with_mt_server({
    expect_match(output$tree$html, "dataIn() is NULL", fixed = TRUE)
  })
})

test_that("without dataset the selected tags are hidden", {
  skip_if_no_mt_deps()
  cap <- new.env(); mock_mt(cap)
  
  with_mt_server({
    expect_error(output$selectedNodes, class = "shiny.silent.error")
  })
})

test_that("without dataset the protein tree waits", {
  skip_if_no_mt_deps()
  cap <- new.env(); mock_mt(cap)
  
  with_mt_server({
    expect_error(output$metacell_tree_protein, class = "shiny.silent.error")
  })
})

test_that("a dataset initialises the tree and the selection", {
  skip_if_no_mt_deps()
  cap <- new.env(); mock_mt(cap)
  se <- make_se_mt()
  
  with_mt_server({
    session$flushReact()
    res <- session$returned()
    
    expect_true(is.numeric(res$trigger))
    expect_null(res$values)
    expect_identical(cap$type_obj, se)
    expect_identical(cap$level, "protein")
  }, dataIn = shiny::reactive(se))
})

test_that("the widget names are built from the tags, without the root 'Any'", {
  skip_if_no_mt_deps()
  cap <- new.env(); mock_mt(cap)
  
  with_mt_server({
    session$flushReact()
    
    expect_identical(
      names(rv$mapping),
      c("quantified_cb", "quantbydirectid_cb", "quantbyrecovery_cb",
        "missing_cb", "missingpov_cb", "missingmec_cb", "imputed_cb",
        "imputedpov_cb", "imputedmec_cb", "combinedtags_cb")
    )
    expect_false("Any" %in% rv$mapping)
    expect_identical(unname(rv$mapping), setdiff(nodes_mt, "Any"))
  }, dataIn = shiny::reactive(make_se_mt()))
})

test_that("the colours are named by widget and skip the root", {
  skip_if_no_mt_deps()
  cap <- new.env(); mock_mt(cap)
  
  with_mt_server({
    session$flushReact()
    meta <- make_meta_mt()
    
    expect_identical(names(rv$bg_colors), names(rv$mapping))
    expect_identical(unname(rv$bg_colors), meta$color[-1])
  }, dataIn = shiny::reactive(make_se_mt()))
})

test_that("no tag is selected at start", {
  skip_if_no_mt_deps()
  cap <- new.env(); mock_mt(cap)
  
  with_mt_server({
    session$flushReact()
    
    expect_identical(names(rv$tags), setdiff(nodes_mt, "Any"))
    expect_true(all(rv$tags == FALSE))
    expect_true(rv$autoChanged)
  }, dataIn = shiny::reactive(make_se_mt()))
})

test_that("the selection is still reset when the dataset has no type", {
  skip_if_no_mt_deps()
  cap <- new.env(); mock_mt(cap, type = NULL)
  
  with_mt_server({
    session$flushReact()
    res <- session$returned()
    
    # init_tree() is skipped, the trigger is still set
    expect_null(rv$mapping)
    expect_true(is.numeric(res$trigger))
    expect_null(res$values)
  }, dataIn = shiny::reactive(make_se_mt()))
})

test_that("a new dataset rebuilds the tree and clears the selection", {
  skip_if_no_mt_deps()
  cap <- new.env(); mock_mt(cap)
  data <- shiny::reactiveVal(make_se_mt())
  
  with_mt_server({
    session$flushReact()
    first <- session$returned()$trigger
    dataOut$values <- "Missing POV"
    expect_identical(session$returned()$values, "Missing POV")
    
    data(make_se_mt(offset = 10))   # must differ, or nothing is invalidated
    session$flushReact()
    
    expect_null(session$returned()$values)
    expect_gte(session$returned()$trigger, first)
    expect_identical(cap$type_obj, make_se_mt(offset = 10))
  }, dataIn = data)
})

test_that("a remote reset clears the selection", {
  skip_if_no_mt_deps()
  cap <- new.env(); mock_mt(cap)
  remote <- shiny::reactiveVal(0)
  
  with_mt_server({
    session$flushReact()
    dataOut$values <- "Missing POV"
    
    remote(1)
    session$flushReact()
    
    expect_null(session$returned()$values)
  }, dataIn = shiny::reactive(make_se_mt()), remoteReset = remote)
})

test_that("the selected tags are displayed after validation", {
  skip_if_no_mt_deps()
  cap <- new.env(); mock_mt(cap)
  
  with_mt_server({
    session$flushReact()
    rv$tags[c("Missing POV", "Imputed")] <- TRUE
    session$setInputs(validatetree = 1)
    session$flushReact()
    
    html <- output$selectedNodes$html
    
    expect_match(html, "Selected tags: ", fixed = TRUE)
    expect_match(html, "Missing POV, Imputed", fixed = TRUE)
  }, dataIn = shiny::reactive(make_se_mt()))
})

test_that("the selected tags are hidden when the selection is empty", {
  skip_if_no_mt_deps()
  cap <- new.env(); mock_mt(cap)
  
  with_mt_server({
    session$flushReact()
    
    expect_error(output$selectedNodes, class = "shiny.silent.error")
  }, dataIn = shiny::reactive(make_se_mt()))
})

test_that("the tree container points to the output of the dataset type", {
  skip_if_no_mt_deps()
  cap <- new.env(); mock_mt(cap, type = "protein")
  
  with_mt_server({
    session$flushReact()
    
    expect_match(output$tree$html, "metacell_tree_protein", fixed = TRUE)
  }, dataIn = shiny::reactive(make_se_mt()))
})

test_that("the protein tree has one checkbox per tag", {
  skip_if_no_mt_deps()
  cap <- new.env(); mock_mt(cap)
  
  with_mt_server({
    session$flushReact()
    html <- output$metacell_tree_protein$html
    
    for (id in names(rv$mapping)) {
      expect_match(html, paste0("-", id, '"'), fixed = TRUE, info = id)
    }
    expect_match(html, "wtree", fixed = TRUE)
  }, dataIn = shiny::reactive(make_se_mt()))
})

test_that("the protein tree shows the count and percentage of each tag", {
  skip_if_no_mt_deps()
  cap <- new.env(); mock_mt(cap)
  
  with_mt_server({
    session$flushReact()
    html <- output$metacell_tree_protein$html
    
    expect_match(html, "Quantified (30 - 30%)", fixed = TRUE)
    expect_match(html, "Quant. by direct id (10 - 10%)", fixed = TRUE)
    expect_match(html, "Missing POV (5 - 5%)", fixed = TRUE)
    expect_match(html, "Imputed MEC (2 - 2%)", fixed = TRUE)
    expect_match(html, "Combined tags (5 - 5%)", fixed = TRUE)
  }, dataIn = shiny::reactive(make_se_mt()))
})

test_that("the protein tree colours each tag with its background colour", {
  skip_if_no_mt_deps()
  cap <- new.env(); mock_mt(cap)
  
  with_mt_server({
    session$flushReact()
    html <- output$metacell_tree_protein$html
    
    # Quantified is the 2nd node of the fake definition
    expect_match(html, paste0("background: ", make_meta_mt()$color[2]),
                 fixed = TRUE)
    expect_no_match(html, "#bg-color#", fixed = TRUE)
  }, dataIn = shiny::reactive(make_se_mt()))
})

test_that("validating returns the selected tags", {
  skip_if_no_mt_deps()
  cap <- new.env(); mock_mt(cap)
  
  with_mt_server({
    session$flushReact()
    rv$tags[c("Missing POV", "Imputed")] <- TRUE
    
    # Note: this calls removeModal() on the mock session
    session$setInputs(validatetree = 1)
    
    expect_identical(session$returned()$values, c("Missing POV", "Imputed"))
  }, dataIn = shiny::reactive(make_se_mt()))
})
