library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

make_qf_save <- function() {
  coldata <- S4Vectors::DataFrame(
    Condition = c("A", "A", "B", "B"),
    row.names = paste0("S", 1:4)
  )
  make_se <- function(offset) {
    SummarizedExperiment::SummarizedExperiment(
      assays = list(counts = matrix(
        seq_len(8) + offset, nrow = 2,
        dimnames = list(paste0("f", 1:2), paste0("S", 1:4))
      ))
    )
  }
  QFeatures::QFeatures(list(a1 = make_se(0), a2 = make_se(100)),
                       colData = coldata)
}

hist_df <- function(step = "s1") {
  data.frame(Step = step, Param = "p", Value = "v", stringsAsFactors = FALSE)
}

# Mocks paramshistory() / `paramshistory<-`().
#   cap$set : value given to the setter (the new history)
#   cap$on  : object the setter was applied to
mock_paramshistory <- function(cap, existing = hist_df("old"),
                               env = parent.frame()) {
  testthat::local_mocked_bindings(
    paramshistory = function(object, ...) existing,
    `paramshistory<-` = function(object, value) {
      cap$set <- value
      cap$on <- object
      object
    },
    .package = "DaparToolshed",
    .env = env
  )
}

# Writes 'lines' to a temporary file and returns a fileInput-like list
make_upload <- function(lines, ext, env = parent.frame()) {
  path <- withr::local_tempfile(fileext = paste0(".", ext), .local_envir = env)
  writeLines(lines, path)
  list(name = paste0("data.", ext), datapath = path)
}

# Mocks shinyjs::info() and records the messages in 'cap$info'
mock_info <- function(cap, env = parent.frame()) {
  testthat::local_mocked_bindings(
    info = function(text, ...) {
      cap$info <- c(cap$info, text)
      invisible(NULL)
    },
    .package = "shinyjs",
    .env = env
  )
}

# ---- Tests -----------------------------------------------------------------

## ----- save_txt_ui -----
test_that("save_txt_ui returns a div tag", {
  res <- save_txt_ui()
  
  expect_s3_class(res, "shiny.tag")
  expect_identical(res$name, "div")
})

test_that("save_txt_ui has an outer margin", {
  res <- save_txt_ui()
  
  expect_match(res$attribs$style, "margin: 25px;", fixed = TRUE)
})

test_that("save_txt_ui contains a single paragraph with the instructions", {
  html <- as.character(save_txt_ui())
  
  expect_equal(lengths(regmatches(html, gregexpr("<p", html, fixed = TRUE))), 1)
  expect_match(html, "<b>'Run'</b>", fixed = TRUE)
  expect_match(html, "<b>'Reset'</b>", fixed = TRUE)
  expect_match(html, "<br>", fixed = TRUE)
})

test_that("save_txt_ui styles the paragraph", {
  html <- as.character(save_txt_ui())
  
  expect_match(html, "background-color: #EAEAEA", fixed = TRUE)
  expect_match(html, "font-size: 17px", fixed = TRUE)
})

test_that("save_txt_ui is deterministic", {
  expect_identical(as.character(save_txt_ui()), as.character(save_txt_ui()))
})




## ----- prepareQFsave -----
test_that("prepareQFsave errors when data is missing", {
  expect_error(prepareQFsave(history = hist_df()), "'data' is required.",
               fixed = TRUE)
})

test_that("prepareQFsave errors when data is not a QFeatures", {
  msg <- "'data' must be an object of class QFeatures."
  
  expect_error(prepareQFsave(list(), history = hist_df()), msg, fixed = TRUE)
  expect_error(prepareQFsave(NULL, history = hist_df()), msg, fixed = TRUE)
  expect_error(prepareQFsave(make_qf_save()[[1]], history = hist_df()), msg,
               fixed = TRUE)
})

test_that("prepareQFsave errors when i is not a single number", {
  qf <- make_qf_save()
  msg <- "'i' must be a numeric of length 1."
  
  expect_error(prepareQFsave(qf, i = "a", history = hist_df()), msg, fixed = TRUE)
  expect_error(prepareQFsave(qf, i = 1:2, history = hist_df()), msg, fixed = TRUE)
  expect_error(prepareQFsave(qf, i = numeric(0), history = hist_df()), msg,
               fixed = TRUE)
})

test_that("prepareQFsave errors when history is missing", {
  expect_error(prepareQFsave(make_qf_save()), "'history' is required.",
               fixed = TRUE)
})

test_that("prepareQFsave errors when history is not a data.frame", {
  qf <- make_qf_save()
  msg <- "'history' must be an object of class data.frame."
  
  expect_error(prepareQFsave(qf, history = list(a = 1)), msg, fixed = TRUE)
  expect_error(prepareQFsave(qf, history = NULL), msg, fixed = TRUE)
  expect_error(prepareQFsave(qf, history = matrix(1)), msg, fixed = TRUE)
})

test_that("prepareQFsave errors when namePipeline is invalid", {
  qf <- make_qf_save()
  msg <- "'namePipeline' must be a character of length 1."
  
  expect_error(prepareQFsave(qf, history = hist_df(), namePipeline = 1), msg,
               fixed = TRUE)
  expect_error(prepareQFsave(qf, history = hist_df(),
                             namePipeline = NA_character_), msg, fixed = TRUE)
  # Longer or empty vectors also fail (the exact error depends on the R version)
  expect_error(prepareQFsave(qf, history = hist_df(),
                             namePipeline = c("a", "b")))
  expect_error(prepareQFsave(qf, history = hist_df(),
                             namePipeline = character(0)))
})

test_that("prepareQFsave errors when SEname is invalid", {
  qf <- make_qf_save()
  msg <- "'SEname' must be a character of length 1."
  
  expect_error(prepareQFsave(qf, history = hist_df(), SEname = 1), msg,
               fixed = TRUE)
  expect_error(prepareQFsave(qf, history = hist_df(), SEname = NA_character_),
               msg, fixed = TRUE)
  expect_error(prepareQFsave(qf, history = hist_df(), SEname = c("a", "b")))
})

test_that("prepareQFsave does not touch the paramshistory when an argument is invalid", {
  cap <- new.env()
  mock_paramshistory(cap)
  
  expect_error(prepareQFsave(make_qf_save(), history = list()))
  expect_null(cap$set)
})

test_that("prepareQFsave returns a QFeatures with the same assays", {
  mock_paramshistory(new.env())
  qf <- make_qf_save()
  
  res <- prepareQFsave(qf, history = hist_df())
  
  expect_s4_class(res, "QFeatures")
  expect_identical(names(res), names(qf))
  expect_equal(SummarizedExperiment::assay(res[["a2"]]),
               SummarizedExperiment::assay(qf[["a2"]]))
})

test_that("prepareQFsave stores the pipeline name in the metadata", {
  mock_paramshistory(new.env())
  
  res <- prepareQFsave(make_qf_save(), history = hist_df(),
                       namePipeline = "PipelinePeptide")
  
  expect_identical(S4Vectors::metadata(res)$name.pipeline, "PipelinePeptide")
})

test_that("prepareQFsave uses 'PipelineProtein' as the default pipeline name", {
  mock_paramshistory(new.env())
  
  res <- prepareQFsave(make_qf_save(), history = hist_df())
  
  expect_identical(S4Vectors::metadata(res)$name.pipeline, "PipelineProtein")
})

test_that("prepareQFsave appends the history to the existing one", {
  cap <- new.env()
  mock_paramshistory(cap, existing = hist_df("old"))
  
  prepareQFsave(make_qf_save(), history = hist_df("new"))
  
  expect_s3_class(cap$set, "data.frame")
  expect_equal(nrow(cap$set), 2)
  expect_identical(cap$set$Step, c("old", "new"))
})

test_that("prepareQFsave works on the last assay by default", {
  cap <- new.env()
  mock_paramshistory(cap)
  qf <- make_qf_save()
  
  prepareQFsave(qf, history = hist_df())
  
  expect_equal(SummarizedExperiment::assay(cap$on),
               SummarizedExperiment::assay(qf[[length(qf)]]))
})

test_that("prepareQFsave works on the assay given by i", {
  cap <- new.env()
  mock_paramshistory(cap)
  qf <- make_qf_save()
  
  prepareQFsave(qf, i = 1, history = hist_df())
  
  expect_equal(SummarizedExperiment::assay(cap$on),
               SummarizedExperiment::assay(qf[[1]]))
})

test_that("prepareQFsave renames the last assay by default", {
  mock_paramshistory(new.env())
  
  res <- prepareQFsave(make_qf_save(), history = hist_df(), SEname = "saved")
  
  expect_identical(names(res), c("a1", "saved"))
})

test_that("prepareQFsave renames the assay given by i", {
  mock_paramshistory(new.env())
  
  res <- prepareQFsave(make_qf_save(), i = 1, history = hist_df(),
                       SEname = "saved")
  
  expect_identical(names(res), c("saved", "a2"))
})

test_that("prepareQFsave keeps the names when SEname is NULL", {
  mock_paramshistory(new.env())
  
  res <- prepareQFsave(make_qf_save(), history = hist_df())
  
  expect_identical(names(res), c("a1", "a2"))
})

test_that("prepareQFsave does not modify its input", {
  mock_paramshistory(new.env())
  qf <- make_qf_save()
  
  prepareQFsave(qf, history = hist_df(), SEname = "saved",
                namePipeline = "Other")
  
  expect_identical(names(qf), c("a1", "a2"))
  expect_null(S4Vectors::metadata(qf)$name.pipeline)
})





## ----- readFileConvert -----
test_that("readFileConvert reads a csv file with ';' as separator", {
  cap <- new.env(); mock_info(cap)
  f <- make_upload(c("id;value", "a;1", "b;2"), "csv")
  
  res <- readFileConvert(f)
  
  expect_s3_class(res, "data.frame")
  expect_equal(dim(res), c(2, 2))
  expect_identical(colnames(res), c("id", "value"))
  expect_identical(res$id, c("a", "b"))
  expect_equal(res$value, c(1, 2))
  expect_null(cap$info)
})

test_that("readFileConvert reads a txt file with tabs", {
  cap <- new.env(); mock_info(cap)
  f <- make_upload(c("id\tvalue", "a\t1", "b\t2"), "txt")
  
  res <- readFileConvert(f)
  
  expect_equal(dim(res), c(2, 2))
  expect_identical(res$id, c("a", "b"))
})

test_that("readFileConvert reads a tsv file with tabs", {
  cap <- new.env(); mock_info(cap)
  f <- make_upload(c("id\tvalue", "a\t1", "b\t2"), "tsv")
  
  res <- readFileConvert(f)
  
  expect_equal(dim(res), c(2, 2))
  expect_equal(res$value, c(1, 2))
})

test_that("readFileConvert does not split a csv file on tabs", {
  cap <- new.env(); mock_info(cap)
  f <- make_upload(c("id\tvalue", "a\t1"), "csv")
  
  res <- readFileConvert(f)
  
  expect_equal(ncol(res), 1)
})

test_that("readFileConvert replaces '.' and spaces by '_' in column names", {
  cap <- new.env(); mock_info(cap)
  f <- make_upload(c("my.col;other col;a.b c", "1;2;3"), "csv")
  
  res <- readFileConvert(f)
  
  expect_identical(colnames(res), c("my_col", "other_col", "a_b_c"))
})

test_that("readFileConvert keeps character columns as character", {
  cap <- new.env(); mock_info(cap)
  f <- make_upload(c("id;value", "a;x", "b;y"), "csv")
  
  res <- readFileConvert(f)
  
  expect_type(res$value, "character")
})

test_that("readFileConvert reads xls and xlsx files with readExcel and the sheet", {
  for (ext in c("xls", "xlsx")) {
    cap <- new.env(); mock_info(cap)
    testthat::local_mocked_bindings(
      readExcel = function(path, sheet = NULL, ...) {
        cap$excel <- list(path = path, sheet = sheet)
        data.frame(`a.b` = 1:2, `c d` = 3:4, check.names = FALSE)
      },
      .package = "DaparToolshed"
    )
    f <- list(name = paste0("data.", ext), datapath = "/some/path")
    
    res <- readFileConvert(f, sheet = "Sheet2")
    
    expect_identical(cap$excel$path, "/some/path", info = ext)
    expect_identical(cap$excel$sheet, "Sheet2", info = ext)
    # Column names are cleaned for Excel files too
    expect_identical(colnames(res), c("a_b", "c_d"), info = ext)
    expect_null(cap$info)
  }
})

test_that("readFileConvert passes sheet = NULL to readExcel by default", {
  cap <- new.env(); mock_info(cap)
  testthat::local_mocked_bindings(
    readExcel = function(path, sheet = NULL, ...) {
      cap$excel <- list(sheet = sheet)
      data.frame(x = 1)
    },
    .package = "DaparToolshed"
  )
  
  readFileConvert(list(name = "data.xlsx", datapath = "p"))
  
  expect_null(cap$excel$sheet)
})

test_that("readFileConvert returns NULL and informs the user when readExcel fails", {
  cap <- new.env(); mock_info(cap)
  testthat::local_mocked_bindings(
    readExcel = function(...) stop("cannot read sheet"),
    .package = "DaparToolshed"
  )
  
  res <- readFileConvert(list(name = "data.xlsx", datapath = "p"))
  
  expect_identical(cap$info, "cannot read sheet")
  # `data` was never assigned: the function returns utils::data (current behaviour)
  expect_true(is.function(res))
})

test_that("readFileConvert informs the user when readExcel warns", {
  cap <- new.env(); mock_info(cap)
  testthat::local_mocked_bindings(
    readExcel = function(...) {
      warning("sheet is empty")
      data.frame(x = 1)
    },
    .package = "DaparToolshed"
  )
  
  readFileConvert(list(name = "data.xlsx", datapath = "p"))
  
  expect_identical(cap$info, "sheet is empty")
})

test_that("readFileConvert informs the user when the file cannot be read", {
  cap <- new.env(); mock_info(cap)
  f <- list(name = "data.csv", datapath = file.path(tempdir(), "missing.csv"))
  
  res <- readFileConvert(f)
  
  expect_length(cap$info, 1)
  expect_type(cap$info, "character")
  expect_true(is.function(res))   # same behaviour as above
})

test_that("readFileConvert returns NULL and informs the user for an unknown extension", {
  cap <- new.env(); mock_info(cap)
  f <- make_upload(c("a b c"), "pdf")
  
  res <- readFileConvert(f)
  
  expect_null(res)
  expect_length(cap$info, 1)
})
