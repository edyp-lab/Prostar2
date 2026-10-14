library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

skip_if_no_is_deps <- function() {
  skip_if_not_installed("QFeatures")
  skip_if_not_installed("MagellanNTK")
  skip_if_not_installed("DaparToolshed")
  skip_if_not_installed("tibble")
  skip_if_not_installed("RColorBrewer")
}

# 3 features x 4 samples with 5 NA out of 12 cells (41.67 %).
# Row f1 is completely empty.
make_se_is <- function(peptide_extras = FALSE) {
  mat <- matrix(
    c(NA, NA, NA, NA,
      1, NA, 3, 4,
      5, 6, 7, 8),
    nrow = 3, byrow = TRUE,
    dimnames = list(paste0("f", 1:3), paste0("S", 1:4))
  )
  SummarizedExperiment::SummarizedExperiment(assays = list(counts = mat))
}

make_qf_is <- function(analysis = NULL, pipeline = NULL, file = NULL,
                       last = make_se_is()) {
  coldata <- S4Vectors::DataFrame(
    Condition = c("A", "A", "B", "B"),
    Replicate = 1:4,
    row.names = paste0("S", 1:4)
  )
  qf <- QFeatures::QFeatures(list(first = make_se_is(), last = last),
                             colData = coldata)
  if (!is.null(analysis)) {
    S4Vectors::metadata(qf)$analysis <- list(analysis = analysis)
  }
  if (!is.null(pipeline)) S4Vectors::metadata(qf)$name.pipeline <- pipeline
  if (!is.null(file)) S4Vectors::metadata(qf)$file <- file
  qf
}

# Mocks the collaborators.
#   cap$pkgs : packages given to pkgs_require()
#   cap$dt   : named list (by id) of list(dataIn, args) given to
#              format_DT_server()
#   cap$type_obj : object given to typeDataset()
mock_is <- function(cap, type = "protein", env = parent.frame()) {
  testthat::local_mocked_bindings(
    pkgs_require = function(ll.deps) {
      cap$pkgs <- c(cap$pkgs, ll.deps)
      invisible(NULL)
    },
    .package = "Prostar2",
    .env = env
  )
  testthat::local_mocked_bindings(
    format_DT_server = function(id, dataIn = NULL, ...) {
      cap$dt <- c(cap$dt, stats::setNames(list(list(dataIn = dataIn,
                                                    args = list(...))), id))
      invisible(NULL)
    },
    format_DT_ui = function(id, ...) shiny::div(id = id, "fake DT"),
    .package = "MagellanNTK",
    .env = env
  )
  testthat::local_mocked_bindings(
    typeDataset = function(object, ...) {
      cap$type_obj <- object
      type
    },
    design_qf = function(object, ...) {
      data.frame(Condition = c("A", "A", "B", "B"))
    },
    .package = "DaparToolshed",
    .env = env
  )
}

with_is_server <- function(expr, ...) {
  quo <- rlang::enquo(expr)
  shiny::testServer(infos_dataset_server, args = list(...), expr = !!quo)
}

# ---- Tests -----------------------------------------------------------------

test_that("infos_dataset_ui returns a tagList", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  expect_s3_class(infos_dataset_ui("x"), "shiny.tag.list")
})

test_that("infos_dataset_ui has the namespaced outputs and table", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  html <- as.character(infos_dataset_ui("x"))
  
  for (id in c("title", "samples_tab_ui", "choose_SE_ui", "show_SE_ui", "dt")) {
    expect_match(html, paste0('id="x-', id, '"'), fixed = TRUE, info = id)
  }
})

test_that("infos_dataset_ui has two columns of width 6", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  html <- as.character(infos_dataset_ui("x"))
  
  expect_equal(lengths(regmatches(html, gregexpr("col-sm-6", html, fixed = TRUE))), 2)
})

test_that("infos_dataset_ui uses the namespace given", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  a <- as.character(infos_dataset_ui("a"))
  b <- as.character(infos_dataset_ui("b"))
  
  expect_match(a, 'id="a-title"', fixed = TRUE)
  expect_no_match(a, 'id="b-title"', fixed = TRUE)
  expect_match(b, 'id="b-title"', fixed = TRUE)
})

test_that("infos_dataset returns a shiny app", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  expect_s3_class(infos_dataset(make_qf_is()), "shiny.appobj")
})

test_that("the server checks that RColorBrewer is available", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({}, dataIn = shiny::reactive(make_qf_is()))
  
  expect_identical(cap$pkgs, "RColorBrewer")
})

test_that("the server fails when RColorBrewer is missing", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  testthat::local_mocked_bindings(
    pkgs_require = function(...) stop("missing package"),
    .package = "Prostar2"
  )
  
  expect_error(with_is_server({}), "missing package", fixed = TRUE)
})

test_that("the server starts the 'dt' and 'dt2' table modules", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({
    expect_true(all(c("dt", "dt2") %in% names(cap$dt)))
  })
})

test_that("the server works with its default arguments", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  expect_no_error(shiny::testServer(infos_dataset_server, expr = {}))
})

test_that("rv starts empty", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({
    expect_null(rv$dataIn)
  })
})

test_that("the dataset is stored when it is a QFeatures", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  qf <- make_qf_is()
  
  with_is_server({
    session$flushReact()
    expect_identical(rv$dataIn, qf)
  }, dataIn = shiny::reactive(qf))
})

test_that("a dataset which is not a QFeatures is ignored", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({
    session$flushReact()
    expect_null(rv$dataIn)
  }, dataIn = shiny::reactive(list(a = 1)))
})

test_that("the title shows the analysis name and the original file", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  qf <- make_qf_is(analysis = "myAnalysis", file = "data.xlsx")
  
  with_is_server({
    session$flushReact()
    html <- output$title$html
    
    expect_match(html, "Dataset summary", fixed = TRUE)
    expect_match(html, "Name of analysis:myAnalysis", fixed = TRUE)
    expect_match(html, "Original file:data.xlsx", fixed = TRUE)
  }, dataIn = shiny::reactive(qf))
})

test_that("the title is hidden without dataset", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({
    session$flushReact()
    expect_error(output$title, class = "shiny.silent.error")
  })
})

test_that("the dataset summary has one row per property", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({
    session$flushReact()
    res <- Get_QFeatures_summary()
    
    expect_s3_class(res, "data.frame")
    expect_identical(res$Definition,
                     c("Pipeline Type", "Number of assay(s)", "List of assay(s)"))
  }, dataIn = shiny::reactive(make_qf_is(pipeline = "PipelineProtein")))
})

test_that("the dataset summary gives the pipeline, the number and the list of assays", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({
    session$flushReact()
    res <- Get_QFeatures_summary()
    
    expect_identical(as.character(res$Value[1]), "PipelineProtein")
    expect_identical(as.character(res$Value[2]), "2")
    expect_match(as.character(res$Value[3]), "<li>first</li>", fixed = TRUE)
    expect_match(as.character(res$Value[3]), "<li>last</li>", fixed = TRUE)
  }, dataIn = shiny::reactive(make_qf_is(pipeline = "PipelineProtein")))
})

test_that("the pipeline is '-' when it is not defined", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({
    session$flushReact()
    
    expect_identical(as.character(Get_QFeatures_summary()$Value[1]), "-")
  }, dataIn = shiny::reactive(make_qf_is()))
})

test_that("the dataset summary waits for a dataset", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({
    expect_error(Get_QFeatures_summary(), class = "shiny.silent.error")
  })
})

test_that("the 'dt' table receives the summary as a tibble", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({
    session$flushReact()
    res <- cap$dt$dt$dataIn()
    
    expect_s3_class(res, "tbl_df")
    expect_equal(nrow(res), 3)
  }, dataIn = shiny::reactive(make_qf_is()))
})

test_that("the 'dt' table waits for a dataset", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({
    expect_error(cap$dt$dt$dataIn(), class = "shiny.silent.error")
  })
})

test_that("the samples table is hidden without dataset", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({
    expect_error(output$samples_tab_ui, class = "shiny.silent.error")
    expect_null(cap$dt$samples_tab)
  })
})

test_that("the samples table is shown with a title", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({
    session$flushReact()
    html <- output$samples_tab_ui$html
    
    expect_match(html, "Samples", fixed = TRUE)
    expect_match(html, "-samples_tab", fixed = TRUE)
  }, dataIn = shiny::reactive(make_qf_is()))
})

test_that("the samples table receives the colData and the number of rows", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({
    session$flushReact()
    output$samples_tab_ui
    dt <- cap$dt$samples_tab
    
    expect_s3_class(dt$dataIn(), "data.frame")
    expect_identical(colnames(dt$dataIn()), c("Condition", "Replicate"))
    expect_equal(nrow(dt$dataIn()), 4)
    expect_equal(dt$args$max.rows, 4)
  }, dataIn = shiny::reactive(make_qf_is()))
})

test_that("the samples table is coloured by condition", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({
    session$flushReact()
    output$samples_tab_ui
    style <- cap$dt$samples_tab$args$hc_style()
    
    expect_identical(style$cols, c("Condition", "Replicate"))
    # The second column of colData is used as value column
    expect_identical(style$vals, "Replicate")
    expect_identical(style$unique, c("A", "B"))
    expect_identical(style$pal, RColorBrewer::brewer.pal(3, "Dark2")[1:2])
  }, dataIn = shiny::reactive(make_qf_is()))
})

test_that("the assay selector lists the assays and selects the last one", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({
    session$flushReact()
    html <- output$choose_SE_ui$html
    
    expect_match(html, "Select a dataset for further information", fixed = TRUE)
    expect_match(html, 'value="first"', fixed = TRUE)
    expect_match(html, 'value="last"', fixed = TRUE)
    expect_match(html, 'value="last"[^>]*checked')
    expect_no_match(html, 'value="first"[^>]*checked')
  }, dataIn = shiny::reactive(make_qf_is()))
})

test_that("the assay selector is hidden without dataset", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({
    expect_error(output$choose_SE_ui, class = "shiny.silent.error")
  })
})

test_that("the assay details are hidden until an assay is chosen", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({
    session$flushReact()
    
    expect_error(output$show_SE_ui, class = "shiny.silent.error")
  }, dataIn = shiny::reactive(make_qf_is()))
})

test_that("the assay details are shown once an assay is chosen", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({
    session$flushReact()
    session$setInputs(selectInputSE = "last")
    
    expect_match(output$show_SE_ui$html, "-dt2", fixed = TRUE)
  }, dataIn = shiny::reactive(make_qf_is()))
})

test_that("the assay details are hidden for the choice 'None'", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({
    session$flushReact()
    session$setInputs(selectInputSE = "None")
    
    expect_error(output$show_SE_ui, class = "shiny.silent.error")
  }, dataIn = shiny::reactive(make_qf_is()))
})

test_that("the assay summary waits for a dataset and a choice", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({
    expect_error(Get_SE_Summary(), class = "shiny.silent.error")
  })
  
  with_is_server({
    session$flushReact()
    expect_error(Get_SE_Summary(), class = "shiny.silent.error")
  }, dataIn = shiny::reactive(make_qf_is()))
})

test_that("the assay summary is hidden for the choice 'None'", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({
    session$flushReact()
    session$setInputs(selectInputSE = "None")
    
    expect_error(Get_SE_Summary(), class = "shiny.silent.error")
  }, dataIn = shiny::reactive(make_qf_is()))
})

test_that("the assay summary stops silently for an unknown assay", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({
    session$flushReact()
    session$setInputs(selectInputSE = "unknown")
    
    expect_error(Get_SE_Summary(), class = "shiny.silent.error")
  }, dataIn = shiny::reactive(make_qf_is()))
})

test_that("the assay summary of a protein dataset has 6 rows", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap, type = "protein")
  
  with_is_server({
    session$flushReact()
    session$setInputs(selectInputSE = "last")
    res <- Get_SE_Summary()
    
    expect_s3_class(res, "data.frame")
    expect_identical(colnames(res), c("Definition", "Value"))
    expect_identical(res$Definition,
                     c("Type of data", "Number of lines",
                       "% of missing values", "Number of empty lines",
                       "Number of samples", "Number of conditions"))
    expect_identical(rownames(res), res$Definition)
  }, dataIn = shiny::reactive(make_qf_is()))
})

test_that("the assay summary computes the figures of the chosen assay", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap, type = "protein")
  
  with_is_server({
    session$flushReact()
    session$setInputs(selectInputSE = "last")
    res <- Get_SE_Summary()
    
    # type, lines, % NA (5 / 12), empty lines, samples, conditions
    expect_identical(as.character(res$Value),
                     c("protein", "3", "41.67", "1", "4", "2"))
  }, dataIn = shiny::reactive(make_qf_is()))
})

test_that("the assay summary uses the chosen assay", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  qf <- make_qf_is()
  
  with_is_server({
    session$flushReact()
    session$setInputs(selectInputSE = "first")
    Get_SE_Summary()
    
    expect_equal(SummarizedExperiment::assay(cap$type_obj),
                 SummarizedExperiment::assay(qf[["first"]]))
  }, dataIn = shiny::reactive(qf))
})

test_that("the assay summary shows 0 % missing values and 0 empty lines without NA", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = matrix(
      as.numeric(1:12), 3,
      dimnames = list(paste0("f", 1:3), paste0("S", 1:4))
    ))
  )
  
  with_is_server({
    session$flushReact()
    session$setInputs(selectInputSE = "last")
    res <- Get_SE_Summary()
    
    expect_identical(as.character(res$Value[3]), "0")
    expect_identical(as.character(res$Value[4]), "0")
  }, dataIn = shiny::reactive(make_qf_is(last = se)))
})

test_that("a peptide dataset adds the adjacency matrix and connected components rows", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap, type = "peptide")
  se <- make_se_is()
  SummarizedExperiment::rowData(se)$adjacencyMatrix <- matrix(
    c(1, 0, 0, 1, 1, 0), 3, 2,
    dimnames = list(paste0("f", 1:3), c("P1", "P2"))
  )
  S4Vectors::metadata(se)$list.cc <- list(1, 2)
  
  with_is_server({
    session$flushReact()
    session$setInputs(selectInputSE = "last")
    res <- Get_SE_Summary()
    
    expect_equal(nrow(res), 8)
    expect_identical(tail(res$Definition, 2),
                     c("Adjacency matrices", "Connex components"))
    expect_match(res["Adjacency matrices", "Value"], "green", fixed = TRUE)
    expect_match(res["Adjacency matrices", "Value"], "OK", fixed = TRUE)
    expect_match(res["Connex components", "Value"], "green", fixed = TRUE)
  }, dataIn = shiny::reactive(make_qf_is(last = se)))
})

test_that("a peptide dataset flags missing adjacency matrix and components", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap, type = "peptide")
  
  with_is_server({
    session$flushReact()
    session$setInputs(selectInputSE = "last")
    res <- Get_SE_Summary()
    
    expect_match(res["Adjacency matrices", "Value"], "red", fixed = TRUE)
    expect_match(res["Adjacency matrices", "Value"], "Missing", fixed = TRUE)
    expect_match(res["Connex components", "Value"], "red", fixed = TRUE)
    expect_match(res["Connex components", "Value"], "Missing", fixed = TRUE)
  }, dataIn = shiny::reactive(make_qf_is()))
})

test_that("the peptide type is detected whatever its case", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap, type = "Peptide")
  
  with_is_server({
    session$flushReact()
    session$setInputs(selectInputSE = "last")
    
    expect_equal(nrow(Get_SE_Summary()), 8)
  }, dataIn = shiny::reactive(make_qf_is()))
})

test_that("the 'dt2' table receives the assay summary", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  
  with_is_server({
    session$flushReact()
    session$setInputs(selectInputSE = "last")
    
    expect_identical(cap$dt$dt2$dataIn(), Get_SE_Summary())
  }, dataIn = shiny::reactive(make_qf_is()))
})

test_that("the assay summary follows the chosen assay", {
  skip_if_no_is_deps()
  cap <- new.env(); mock_is(cap)
  full <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = matrix(
      as.numeric(1:8), 2,
      dimnames = list(paste0("f", 1:2), paste0("S", 1:4))
    ))
  )
  
  with_is_server({
    session$flushReact()
    session$setInputs(selectInputSE = "last")
    expect_identical(as.character(Get_SE_Summary()$Value[2]), "2")
    
    session$setInputs(selectInputSE = "first")
    expect_identical(as.character(Get_SE_Summary()$Value[2]), "3")
  }, dataIn = shiny::reactive(make_qf_is(last = full)))
})
