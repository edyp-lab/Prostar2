library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

skip_if_no_ig_deps <- function() {
  skip_if_not_installed("MagellanNTK")
}

make_df_ig <- function() {
  data.frame(A = 1:2, B = 1:2, C = 1:2, D = 1:2)
}

quant_ig <- function() paste0("S", 1:3)

with_ig_server <- function(expr, ...) {
  quo <- rlang::enquo(expr)
  shiny::testServer(mod_inputGroup_server, args = list(...), expr = !!quo)
}

# Default arguments: 3 samples and a table with 4 columns
with_ig_default <- function(expr) {
  quo <- rlang::enquo(expr)
  with_ig_server(
    !!quo,
    df = shiny::reactive(make_df_ig()),
    quantCols = shiny::reactive(quant_ig())
  )
}

# ---- Tests -----------------------------------------------------------------

test_that("mod_inputGroup_ui returns a tagList", {
  expect_s3_class(mod_inputGroup_ui("x"), "shiny.tag.list")
})

test_that("mod_inputGroup_ui has the namespaced outputs", {
  html <- as.character(mod_inputGroup_ui("x"))
  
  expect_match(html, 'id="x-checkIdentificationTab"', fixed = TRUE)
  expect_match(html, 'id="x-inputGroup"', fixed = TRUE)
})

test_that("mod_inputGroup_ui uses the namespace given", {
  a <- as.character(mod_inputGroup_ui("a"))
  b <- as.character(mod_inputGroup_ui("b"))
  
  expect_match(a, 'id="a-inputGroup"', fixed = TRUE)
  expect_no_match(a, 'id="b-inputGroup"', fixed = TRUE)
  expect_match(b, 'id="b-inputGroup"', fixed = TRUE)
})

test_that("mod_inputGroup returns a shiny app", {
  # The data file is only read when the server runs, not when the app is built
  expect_s3_class(mod_inputGroup(), "shiny.appobj")
})

test_that("the server works with its default arguments", {
  skip_if_no_ig_deps()
  
  expect_no_error(shiny::testServer(mod_inputGroup_server, expr = {
    expect_error(output$inputGroup, class = "shiny.silent.error")
    expect_error(output$checkIdentificationTab, class = "shiny.silent.error")
    expect_null(session$returned())
  }))
})

test_that("the selectors wait for the sample names", {
  skip_if_no_ig_deps()
  
  with_ig_server({
    expect_error(output$inputGroup, class = "shiny.silent.error")
  }, df = shiny::reactive(make_df_ig()))
})

test_that("the selectors wait for the table", {
  skip_if_no_ig_deps()
  
  with_ig_server({
    expect_error(output$inputGroup, class = "shiny.silent.error")
  }, quantCols = shiny::reactive(quant_ig()))
})

test_that("the check waits for the sample names and the table", {
  skip_if_no_ig_deps()
  
  with_ig_server({
    expect_error(isOk(), class = "shiny.silent.error")
    expect_error(output$checkIdentificationTab, class = "shiny.silent.error")
  })
})

test_that("there is one selector per sample", {
  skip_if_no_ig_deps()
  
  with_ig_default({
    html <- output$inputGroup$html
    
    expect_equal(lengths(regmatches(html, gregexpr("<select", html, fixed = TRUE))), 3)
    for (i in 1:3) {
      expect_match(html, paste0("input_", i), fixed = TRUE, info = i)
    }
  })
})

test_that("each selector is labelled with its sample name", {
  skip_if_no_ig_deps()
  
  with_ig_default({
    html <- output$inputGroup$html
    
    for (s in quant_ig()) {
      expect_match(html, paste0("Identification col. for ", s), fixed = TRUE,
                   info = s)
    }
  })
})

test_that("the selectors offer 'None' and the columns of the table", {
  skip_if_no_ig_deps()
  
  with_ig_default({
    html <- output$inputGroup$html
    
    expect_match(html, 'value="None"', fixed = TRUE)
    for (col in colnames(make_df_ig())) {
      expect_match(html, paste0('value="', col, '"'), fixed = TRUE, info = col)
    }
  })
})

test_that("the number of selectors follows the sample names", {
  skip_if_no_ig_deps()
  cols <- shiny::reactiveVal(quant_ig())
  
  with_ig_server({
    n_select <- function(h) lengths(regmatches(h, gregexpr("<select", h, fixed = TRUE)))
    expect_equal(n_select(output$inputGroup$html), 3)
    
    cols(paste0("S", 1:2))
    session$flushReact()
    
    expect_equal(n_select(output$inputGroup$html), 2)
  }, df = shiny::reactive(make_df_ig()), quantCols = cols)
})

test_that("isOk reports a missing choice when a selector is on 'None'", {
  skip_if_no_ig_deps()
  
  with_ig_default({
    session$setInputs(input_1 = "A", input_2 = "None", input_3 = "C")
    res <- isOk()
    
    expect_false(res$ok)
    expect_identical(
      res$txt,
      "The identification method is not appropriately defined for each sample."
    )
    expect_identical(res$temp, c("A", "None", "C"))
  })
})

test_that("isOk reports duplicates in the identification columns", {
  skip_if_no_ig_deps()
  
  with_ig_default({
    session$setInputs(input_1 = "A", input_2 = "B", input_3 = "A")
    res <- isOk()
    
    expect_false(res$ok)
    expect_identical(res$txt, "There are duplicates in identification columns.")
  })
})

test_that("isOk accepts distinct columns", {
  skip_if_no_ig_deps()
  
  with_ig_default({
    session$setInputs(input_1 = "A", input_2 = "B", input_3 = "C")
    res <- isOk()
    
    expect_true(res$ok)
    expect_identical(res$txt, "Correct")
    expect_identical(res$temp, c("A", "B", "C"))
  })
})

test_that("isOk gives a trigger value", {
  skip_if_no_ig_deps()
  
  with_ig_default({
    session$setInputs(input_1 = "A", input_2 = "B", input_3 = "C")
    
    expect_false(is.null(isOk()$trigger))
  })
})

test_that("'None' is reported before duplicates", {
  skip_if_no_ig_deps()
  
  with_ig_default({
    session$setInputs(input_1 = "None", input_2 = "None", input_3 = "A")
    
    expect_identical(
      isOk()$txt,
      "The identification method is not appropriately defined for each sample."
    )
  })
})

test_that("selectors that are not set yet count as duplicated NA values", {
  skip_if_no_ig_deps()
  
  # Current behaviour: before any choice, shinyValue() gives NA for each
  # sample, and several NA are seen as duplicates
  with_ig_default({
    res <- isOk()
    
    expect_false(res$ok)
    expect_identical(res$txt, "There are duplicates in identification columns.")
    expect_true(all(is.na(res$temp)))
  })
})

test_that("a single sample with no choice yet is considered correct (current behaviour)", {
  skip_if_no_ig_deps()
  
  with_ig_server({
    res <- isOk()
    
    expect_true(res$ok)
    expect_true(is.na(res$temp))
  }, df = shiny::reactive(make_df_ig()),
  quantCols = shiny::reactive("S1"))
})

test_that("isOk follows the inputs", {
  skip_if_no_ig_deps()
  
  with_ig_default({
    session$setInputs(input_1 = "A", input_2 = "A", input_3 = "C")
    expect_false(isOk()$ok)
    
    session$setInputs(input_2 = "B")
    expect_true(isOk()$ok)
  })
})

test_that("the module returns NULL until the choice is valid", {
  skip_if_no_ig_deps()
  
  with_ig_default({
    isOk()
    expect_null(session$returned())
    
    session$setInputs(input_1 = "A", input_2 = "None", input_3 = "C")
    isOk()
    expect_null(session$returned())
  })
})

test_that("the module returns the chosen columns when the choice is valid", {
  skip_if_no_ig_deps()
  
  with_ig_default({
    session$setInputs(input_1 = "A", input_2 = "B", input_3 = "C")
    isOk()
    
    expect_identical(session$returned(), c("A", "B", "C"))
  })
})

test_that("the returned value is reset to NULL when the choice becomes invalid", {
  skip_if_no_ig_deps()
  
  with_ig_default({
    session$setInputs(input_1 = "A", input_2 = "B", input_3 = "C")
    isOk()
    expect_identical(session$returned(), c("A", "B", "C"))
    
    session$setInputs(input_2 = "A")
    isOk()
    expect_null(session$returned())
    
    session$setInputs(input_2 = "D")
    isOk()
    expect_identical(session$returned(), c("A", "D", "C"))
  })
})

test_that("the returned value is updated through the status output, without calling isOk", {
  skip_if_no_ig_deps()
  
  # dataOut is set as a side effect of isOk(), which the output
  # checkIdentificationTab reads: setInputs() flushes it automatically
  with_ig_default({
    session$setInputs(input_1 = "A", input_2 = "B", input_3 = "C")
    
    expect_identical(session$returned(), c("A", "B", "C"))
    
    session$setInputs(input_2 = "A")
    
    expect_null(session$returned())
  })
})

test_that("the status shows a green check when the choice is valid", {
  skip_if_no_ig_deps()
  
  with_ig_default({
    session$setInputs(input_1 = "A", input_2 = "B", input_3 = "C")
    html <- output$checkIdentificationTab$html
    
    expect_match(html, "images/Ok.png", fixed = TRUE)
    expect_match(html, "Correct", fixed = TRUE)
    expect_no_match(html, "Problem.png", fixed = TRUE)
  })
})

test_that("the status shows a red message for 'None'", {
  skip_if_no_ig_deps()
  
  with_ig_default({
    session$setInputs(input_1 = "A", input_2 = "None", input_3 = "C")
    html <- output$checkIdentificationTab$html
    
    expect_match(html, "images/Problem.png", fixed = TRUE)
    expect_match(html, "not appropriately defined", fixed = TRUE)
    expect_match(html, "color: red", fixed = TRUE)
    expect_no_match(html, "Ok.png", fixed = TRUE)
  })
})

test_that("the status shows a red message for duplicates", {
  skip_if_no_ig_deps()
  
  with_ig_default({
    session$setInputs(input_1 = "A", input_2 = "A", input_3 = "C")
    html <- output$checkIdentificationTab$html
    
    expect_match(html, "images/Problem.png", fixed = TRUE)
    expect_match(html, "duplicates in identification columns", fixed = TRUE)
  })
})

test_that("the status follows the inputs", {
  skip_if_no_ig_deps()
  
  with_ig_default({
    session$setInputs(input_1 = "A", input_2 = "A", input_3 = "C")
    expect_match(output$checkIdentificationTab$html, "Problem.png", fixed = TRUE)
    
    session$setInputs(input_2 = "B")
    expect_match(output$checkIdentificationTab$html, "Ok.png", fixed = TRUE)
  })
})
