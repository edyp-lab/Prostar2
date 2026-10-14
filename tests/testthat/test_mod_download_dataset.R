library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

skip_if_no_dl_deps <- function() {
  skip_if_not_installed("QFeatures")
  skip_if_not_installed("DaparToolshed")
  ns <- topenv(environment(download_dataset_server))
  skip_if_not(
    exists("actionBtnClass", envir = ns, inherits = TRUE),
    "actionBtnClass is not available in the package"
  )
}

# QFeatures with one assay. `analysis` sets the analysis name used in the
# file names (NULL = none)
make_qf_dl <- function(analysis = NULL) {
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = matrix(
      1:4, 2, dimnames = list(c("f1", "f2"), c("S1", "S2"))
    ))
  )
  qf <- QFeatures::QFeatures(
    list(a1 = se),
    colData = S4Vectors::DataFrame(row.names = c("S1", "S2"))
  )
  if (!is.null(analysis)) qf@metadata$analysis <- list(analysis = analysis)
  qf
}

# Runs the module server. `expr` is captured lazily so that the module's
# internal objects (rv, output, session) are visible inside it.
with_dl_server <- function(expr, ...) {
  quo <- rlang::enquo(expr)
  shiny::testServer(download_dataset_server, args = list(...), expr = !!quo)
}

# Mocks writeExcel(): records its arguments in 'cap' and writes a small file
mock_write_excel <- function(cap, env = parent.frame()) {
  testthat::local_mocked_bindings(
    writeExcel = function(obj, filename, ...) {
      cap$obj <- obj
      cap$filename <- filename
      writeLines("fake xlsx", filename)
      invisible(filename)
    },
    .package = "DaparToolshed",
    .env = env
  )
}

# ---- Tests -----------------------------------------------------------------

test_that("download_dataset_ui returns a tagList", {
  expect_s3_class(download_dataset_ui("x"), "shiny.tag.list")
})

test_that("download_dataset_ui has a title and namespaced outputs", {
  html <- as.character(download_dataset_ui("x"))
  
  expect_match(html, "Download dataset", fixed = TRUE)
  expect_match(html, 'id="x-nodataset_ui"', fixed = TRUE)
  expect_match(html, 'id="x-buttons_ui"', fixed = TRUE)
})

test_that("download_dataset_ui uses the namespace given", {
  a <- as.character(download_dataset_ui("a"))
  b <- as.character(download_dataset_ui("b"))
  
  expect_match(a, 'id="a-buttons_ui"', fixed = TRUE)
  expect_no_match(a, 'id="b-buttons_ui"', fixed = TRUE)
  expect_match(b, 'id="b-buttons_ui"', fixed = TRUE)
})

test_that("download_dataset returns a shiny app", {
  expect_s3_class(download_dataset(), "shiny.appobj")
  expect_s3_class(download_dataset(make_qf_dl()), "shiny.appobj")
})

test_that("without dataset the module shows 'No dataset available'", {
  skip_if_no_dl_deps()
  
  with_dl_server({
    session$flushReact()
    
    expect_null(rv$data_save)
    expect_match(output$nodataset_ui$html, "No dataset available", fixed = TRUE)
  })
})

test_that("without dataset the buttons are not shown", {
  skip_if_no_dl_deps()
  
  with_dl_server({
    session$flushReact()
    
    expect_error(output$buttons_ui, class = "shiny.silent.error")
  })
})

test_that("a NULL dataset behaves like no dataset", {
  skip_if_no_dl_deps()
  
  with_dl_server(
    {
      session$flushReact()
      expect_null(rv$data_save)
    },
    dataIn = shiny::reactive(NULL)
  )
})

test_that("the dataset given is stored in the module", {
  skip_if_no_dl_deps()
  qf <- make_qf_dl()
  
  with_dl_server(
    {
      session$flushReact()
      expect_identical(rv$data_save, qf)
    },
    dataIn = shiny::reactive(qf)
  )
})

test_that("with a dataset the 'No dataset' message disappears", {
  skip_if_no_dl_deps()
  
  with_dl_server(
    {
      session$flushReact()
      expect_error(output$nodataset_ui, class = "shiny.silent.error")
    },
    dataIn = shiny::reactive(make_qf_dl())
  )
})

test_that("with a dataset the buttons and the waiting message are shown", {
  skip_if_no_dl_deps()
  
  with_dl_server(
    {
      session$flushReact()
      html <- output$buttons_ui$html
      
      expect_match(html, "Please be patient", fixed = TRUE)
      expect_match(html, "dl_xl", fixed = TRUE)
      expect_match(html, "dl_raw", fixed = TRUE)
    },
    dataIn = shiny::reactive(make_qf_dl())
  )
})

test_that("the module follows a dataset that arrives later", {
  skip_if_no_dl_deps()
  data <- shiny::reactiveVal(NULL)
  qf <- make_qf_dl()
  
  with_dl_server(
    {
      session$flushReact()
      expect_null(rv$data_save)
      
      data(qf)
      session$flushReact()
      
      expect_identical(rv$data_save, qf)
      expect_error(output$nodataset_ui, class = "shiny.silent.error")
    },
    dataIn = data
  )
})

test_that("both download buttons are shown by default", {
  skip_if_no_dl_deps()
  
  with_dl_server({
    expect_match(output$dl_xl$html, "downloadDataExcel", fixed = TRUE)
    expect_match(output$dl_raw$html, "downloadDataQf", fixed = TRUE)
  })
})

test_that("the download buttons are namespaced and labelled", {
  skip_if_no_dl_deps()
  
  with_dl_server({
    html_xl <- output$dl_xl$html
    html_qf <- output$dl_raw$html
    
    expect_match(html_xl, "-downloadDataExcel", fixed = TRUE)
    expect_match(html_xl, "xlsx", fixed = TRUE)
    expect_match(html_qf, "-downloadDataQf", fixed = TRUE)
    expect_match(html_qf, "qf", fixed = TRUE)
  })
})

test_that("only the xlsx button is shown with extension = 'xlsx'", {
  skip_if_no_dl_deps()
  
  with_dl_server(
    {
      expect_match(output$dl_xl$html, "downloadDataExcel", fixed = TRUE)
      expect_error(output$dl_raw, class = "shiny.silent.error")
    },
    extension = "xlsx"
  )
})

test_that("only the qf button is shown with extension = 'qf'", {
  skip_if_no_dl_deps()
  
  with_dl_server(
    {
      expect_error(output$dl_xl, class = "shiny.silent.error")
      expect_match(output$dl_raw$html, "downloadDataQf", fixed = TRUE)
    },
    extension = "qf"
  )
})

test_that("no button is shown with an unsupported extension", {
  skip_if_no_dl_deps()
  
  with_dl_server(
    {
      expect_error(output$dl_xl, class = "shiny.silent.error")
      expect_error(output$dl_raw, class = "shiny.silent.error")
    },
    extension = "csv"
  )
})

test_that("the qf download is an RDS file holding the dataset", {
  skip_if_no_dl_deps()
  qf <- make_qf_dl()
  
  with_dl_server(
    {
      session$flushReact()
      path <- output$downloadDataQf
      
      expect_true(file.exists(path))
      expect_equal(
        SummarizedExperiment::assay(readRDS(path)[["a1"]]),
        SummarizedExperiment::assay(qf[["a1"]])
      )
    },
    dataIn = shiny::reactive(qf)
  )
})

test_that("the xlsx download gives the dataset to writeExcel and returns its file", {
  skip_if_no_dl_deps()
  cap <- new.env(); mock_write_excel(cap)
  qf <- make_qf_dl()
  
  with_dl_server(
    {
      session$flushReact()
      path <- output$downloadDataExcel
      
      expect_true(file.exists(path))
      expect_identical(readLines(path), "fake xlsx")
      expect_identical(cap$obj, qf)
      expect_match(cap$filename, "\\.xlsx$")
    },
    dataIn = shiny::reactive(qf)
  )
})

test_that("the xlsx export is built in a temporary file, not in the output file", {
  skip_if_no_dl_deps()
  cap <- new.env(); mock_write_excel(cap)
  
  with_dl_server(
    {
      session$flushReact()
      path <- output$downloadDataExcel
      
      expect_false(identical(cap$filename, path))
      expect_identical(rv$export_file_xlsx, cap$filename)
    },
    dataIn = shiny::reactive(make_qf_dl())
  )
})

test_that("the qf export keeps the path of the temporary file", {
  skip_if_no_dl_deps()
  
  with_dl_server(
    {
      session$flushReact()
      output$downloadDataQf
      
      expect_match(rv$export_file_qf, "\\.qf$")
    },
    dataIn = shiny::reactive(make_qf_dl())
  )
})

test_that("the xlsx download fails when writeExcel fails", {
  skip_if_no_dl_deps()
  testthat::local_mocked_bindings(
    writeExcel = function(...) stop("cannot write"),
    .package = "DaparToolshed"
  )
  
  with_dl_server(
    {
      session$flushReact()
      # tryCatch() stores the error in rv$export_file_xlsx, then file.copy()
      # fails on it: the error is not reported to the user
      expect_error(output$downloadDataExcel)
      expect_s3_class(rv$export_file_xlsx, "error")
    },
    dataIn = shiny::reactive(make_qf_dl())
  )
})
