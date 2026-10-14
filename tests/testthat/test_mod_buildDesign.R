library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

quant_cols <- function() paste0("S", 1:4)

# Runs a testServer() on the module with 4 quantitative columns.
# `remote` is the remoteReset reactive (a reactiveVal can be given).
with_design_server <- function(expr, quantCols = quant_cols(),
                               remote = shiny::reactive(0)) {
  quo <- rlang::enquo(expr)
  
  shiny::testServer(
    mod_buildDesign_server,
    args = list(quantCols = shiny::reactive(quantCols),
                remoteReset = remote),
    expr = !!quo
  )
}

skip_if_no_helpers <- function() {
  skip_if_not_installed("rhandsontable")
  skip_if_not_installed("DaparToolshed")
  
  ns <- asNamespace("DaparToolshed")
  missing <- Filter(
    function(f) !exists(f, envir = ns, inherits = FALSE),
    c("checkConditions", "checkDesign", "ExtendPalette")
  )
  skip_if(
    length(missing) > 0,
    paste("Not available in DaparToolshed:", paste(missing, collapse = ", "))
  )
}

# Fills the conditions and validates them
check_conditions <- function(session, rv.custom, conds) {
  rv.custom$hot$Condition <- conds
  session$setInputs(btn_checkConds = 1)
  session$flushReact()
}

# ---- Tests -----------------------------------------------------------------

test_that("mod_buildDesign_ui returns a tagList", {
  skip_if_not_installed("rhandsontable")
  
  res <- mod_buildDesign_ui("x")
  
  expect_s3_class(res, "shiny.tag.list")
})

test_that("mod_buildDesign_ui namespaces its outputs", {
  skip_if_not_installed("rhandsontable")
  
  html <- as.character(mod_buildDesign_ui("x"))
  
  for (id in c("hot", "reset_btn", "UI_reorder", "UI_checkConditions",
               "UI_hierarchicalExp", "checkDesign")) {
    expect_match(html, paste0('id="x-', id, '"'), fixed = TRUE, info = id)
  }
})

test_that("mod_buildDesign_ui contains the instructions and the title", {
  skip_if_not_installed("rhandsontable")
  
  html <- as.character(mod_buildDesign_ui("x"))
  
  expect_match(html, "<h4>Design</h4>", fixed = TRUE)
  expect_match(html, "1 - Fill the", fixed = TRUE)
})

test_that("mod_buildDesign_ui uses different ids for different namespaces", {
  skip_if_not_installed("rhandsontable")
  
  a <- as.character(mod_buildDesign_ui("a"))
  b <- as.character(mod_buildDesign_ui("b"))
  
  expect_match(a, 'id="a-hot"', fixed = TRUE)
  expect_no_match(a, 'id="b-hot"', fixed = TRUE)
  expect_match(b, 'id="b-hot"', fixed = TRUE)
})

test_that("mod_buildDesign returns a shiny app", {
  skip_if_not_installed("rhandsontable")
  
  app <- mod_buildDesign(quant_cols())
  
  expect_s3_class(app, "shiny.appobj")
})

test_that("the helpers used by mod_buildDesign are exported by DaparToolshed", {
  skip_if_not_installed("DaparToolshed")
  
  exported <- getNamespaceExports("DaparToolshed")
  
  expect_true("checkConditions" %in% exported)
  expect_true("checkDesign" %in% exported)
  expect_true("ExtendPalette" %in% exported)
})

test_that("the design table starts with one row per quantitative column", {
  skip_if_no_helpers()
  
  with_design_server({
    expect_s3_class(rv.custom$hot, "data.frame")
    expect_identical(colnames(rv.custom$hot), c("quantCols", "Condition"))
    expect_identical(rv.custom$hot$quantCols, quant_cols())
    expect_identical(rv.custom$hot$Condition, rep("", 4))
  })
})

test_that("the table is empty when there are no quantitative columns", {
  skip_if_no_helpers()
  
  with_design_server(quantCols = character(0), {
    expect_equal(nrow(rv.custom$hot), 0)
  })
})

test_that("the module returns an empty design at start", {
  skip_if_no_helpers()
  
  with_design_server({
    res <- session$returned()
    
    expect_null(res$trigger)
    expect_null(res$design)
    expect_null(res$order)
  })
})

test_that("the widgets and the checks start with their default values", {
  skip_if_no_helpers()
  
  with_design_server({
    expect_identical(rv.widgets$convert_reorder, "No")
    expect_identical(rv.widgets$chooseExpDesign, "FlatDesign")
    expect_null(rv.custom$conditionsChecked)
    expect_null(rv.custom$designChecked)
  })
})

test_that("the table output is rendered", {
  skip_if_no_helpers()
  
  with_design_server({
    # Conditions are empty at start: color_renderer must still work
    expect_no_error(output$hot)
    expect_false(is.null(output$hot))
  })
})

test_that("the reset button and the reorder widget are rendered", {
  skip_if_no_helpers()
  
  with_design_server({
    expect_match(output$reset_btn$html, "Reset design", fixed = TRUE)
    expect_match(output$UI_reorder$html, "Order by conditions", fixed = TRUE)
  })
})

test_that("the 'Check conditions' button is hidden while a condition is empty", {
  skip_if_no_helpers()
  
  with_design_server({
    expect_error(output$UI_checkConditions, class = "shiny.silent.error")
  })
})

test_that("the 'Check conditions' button appears when all conditions are filled", {
  skip_if_no_helpers()
  
  with_design_server({
    rv.custom$hot$Condition <- c("A", "A", "B", "B")
    session$flushReact()
    
    expect_match(output$UI_checkConditions$html, "Check conditions",
                 fixed = TRUE)
  })
})

test_that("the design type and 'Check design' outputs wait for valid conditions", {
  skip_if_no_helpers()
  
  with_design_server({
    expect_error(output$UI_hierarchicalExp, class = "shiny.silent.error")
    expect_error(output$checkDesign, class = "shiny.silent.error")
  })
})

test_that("color_renderer builds a JS renderer coloring the filled conditions", {
  skip_if_no_helpers()
  
  with_design_server({
    rv.custom$hot$Condition <- c("A", "A", "B", "")
    js <- color_renderer()
    
    expect_type(js, "character")
    expect_match(js, "^function \\(instance, td, row, col, prop, value, cellProperties\\)")
    expect_match(js, "TextRenderer", fixed = TRUE)
    # Condition column is column index 1; rows 0, 1, 2 are colored, row 3 not
    for (r in 0:2) {
      expect_match(js, paste0("row==", r, " && col==1"), fixed = TRUE)
    }
    expect_no_match(js, "row==3", fixed = TRUE)
  })
})

test_that("color_renderer gives the same colour to rows of the same condition", {
  skip_if_no_helpers()
  
  with_design_server({
    rv.custom$hot$Condition <- c("A", "A", "B", "B")
    js <- color_renderer()
    
    cols <- regmatches(js, gregexpr("background = '[^']+'", js))[[1]]
    
    expect_length(cols, 4)
    expect_identical(cols[1], cols[2])
    expect_identical(cols[3], cols[4])
    expect_false(cols[1] == cols[3])
  })
})

test_that("color_renderer has no coloring rule when no condition is filled", {
  skip_if_no_helpers()
  
  with_design_server({
    js <- color_renderer()
    
    expect_no_match(js, "td.style.background", fixed = TRUE)
  })
})

test_that("valid conditions are checked and the table gets a Bio.Rep column", {
  skip_if_no_helpers()
  
  with_design_server({
    check_conditions(session, rv.custom, c("A", "A", "B", "B"))
    
    expect_true(rv.custom$conditionsChecked$valid)
    # Flat design (default): automatic Bio.Rep column
    expect_identical(colnames(rv.custom$hot),
                     c("quantCols", "Condition", "Bio.Rep"))
    expect_equal(rv.custom$hot$Bio.Rep, 1:4)
  })
})

test_that("the table is not reordered by default", {
  skip_if_no_helpers()
  
  with_design_server({
    check_conditions(session, rv.custom, c("B", "A", "B", "A"))
    
    expect_identical(rv.custom$hot$quantCols, quant_cols())
  })
})

test_that("the table is reordered by condition when asked", {
  skip_if_no_helpers()
  
  with_design_server({
    session$setInputs(convert_reorder = "Yes")
    check_conditions(session, rv.custom, c("B", "A", "B", "A"))
    
    expect_equal(rv.custom$newOrder, c(2, 4, 1, 3))
    expect_identical(rv.custom$hot$quantCols, c("S2", "S4", "S1", "S3"))
  })
})

test_that("checking conditions does nothing when Bio.Rep already exists", {
  skip_if_no_helpers()
  
  with_design_server({
    check_conditions(session, rv.custom, c("A", "A", "B", "B"))
    before <- rv.custom$hot
    
    session$setInputs(btn_checkConds = 2)
    session$flushReact()
    
    expect_identical(rv.custom$hot, before)
  })
})

test_that("the module returns the design after a valid design check", {
  skip_if_no_helpers()
  
  with_design_server({
    check_conditions(session, rv.custom, c("A", "A", "B", "B"))
    session$setInputs(btn_checkDesign = 1)
    session$flushReact()
    res <- session$returned()
    
    expect_true(rv.custom$designChecked$valid)
    expect_false(is.null(res$trigger))
    expect_s3_class(res$design, "data.frame")
    expect_identical(res$design$quantCols, quant_cols())
    expect_identical(res$design$Condition, c("A", "A", "B", "B"))
    expect_equal(res$design$Bio.Rep, 1:4)
  })
})

test_that("the returned order is the identity when conditions are not reordered", {
  skip_if_no_helpers()
  
  with_design_server({
    check_conditions(session, rv.custom, c("A", "A", "B", "B"))
    session$setInputs(btn_checkDesign = 1)
    session$flushReact()
    
    expect_equal(session$returned()$order, 1:4)
  })
})

test_that("the returned order is the reordering permutation when requested", {
  skip_if_no_helpers()
  
  with_design_server({
    session$setInputs(convert_reorder = "Yes")
    check_conditions(session, rv.custom, c("B", "A", "B", "A"))
    session$setInputs(btn_checkDesign = 1)
    session$flushReact()
    
    expect_equal(session$returned()$order, c(2, 4, 1, 3))
  })
})

test_that("choosing a two levels design adds Bio.Rep and Tech.Rep columns", {
  skip_if_no_helpers()
  
  with_design_server({
    check_conditions(session, rv.custom, c("A", "A", "B", "B"))
    session$setInputs(chooseExpDesign = "twoLevelsDesign")
    session$flushReact()
    
    expect_identical(colnames(rv.custom$hot),
                     c("quantCols", "Condition", "Bio.Rep", "Tech.Rep"))
    expect_identical(rv.custom$hot$Bio.Rep, rep("", 4))
    expect_equal(rv.custom$hot$Tech.Rep, 1:4)
  })
})

test_that("choosing a three levels design adds three replicate columns", {
  skip_if_no_helpers()
  
  with_design_server({
    check_conditions(session, rv.custom, c("A", "A", "B", "B"))
    session$setInputs(chooseExpDesign = "threeLevelsDesign")
    session$flushReact()
    
    expect_identical(
      colnames(rv.custom$hot),
      c("quantCols", "Condition", "Bio.Rep", "Tech.Rep", "Analyt.Rep")
    )
    expect_identical(rv.custom$hot$Tech.Rep, rep("", 4))
    expect_equal(rv.custom$hot$Analyt.Rep, 1:4)
  })
})

test_that("an incomplete two levels design does not return a design", {
  skip_if_no_helpers()
  
  with_design_server({
    check_conditions(session, rv.custom, c("A", "A", "B", "B"))
    session$setInputs(chooseExpDesign = "twoLevelsDesign")
    session$flushReact()
    session$setInputs(btn_checkDesign = 1)
    session$flushReact()
    
    # Bio.Rep is still empty: the design cannot be valid
    expect_false(isTRUE(rv.custom$designChecked$valid))
    expect_null(session$returned()$design)
  })
})

test_that("the reset button restores the initial state", {
  skip_if_no_helpers()
  
  with_design_server({
    session$setInputs(convert_reorder = "Yes")
    check_conditions(session, rv.custom, c("A", "A", "B", "B"))
    session$setInputs(btn_checkDesign = 1)
    session$flushReact()
    expect_false(is.null(session$returned()$design))
    
    session$setInputs(reset = 1)
    session$flushReact()
    res <- session$returned()
    
    expect_identical(colnames(rv.custom$hot), c("quantCols", "Condition"))
    expect_identical(rv.custom$hot$Condition, rep("", 4))
    expect_null(rv.custom$conditionsChecked)
    expect_null(rv.custom$designChecked)
    expect_identical(rv.widgets$convert_reorder, "No")
    expect_identical(rv.widgets$chooseExpDesign, "FlatDesign")
    expect_null(res$trigger)
    expect_null(res$design)
    expect_null(res$order)
  })
})

test_that("the remote reset restores the initial state", {
  skip_if_no_helpers()
  remote <- shiny::reactiveVal(0)
  
  with_design_server(remote = remote, {
    check_conditions(session, rv.custom, c("A", "A", "B", "B"))
    expect_true(rv.custom$conditionsChecked$valid)
    
    remote(1)
    session$flushReact()
    
    expect_null(rv.custom$conditionsChecked)
    expect_identical(rv.custom$hot$Condition, rep("", 4))
    expect_identical(colnames(rv.custom$hot), c("quantCols", "Condition"))
  })
})

test_that("the reset does not fire at initialisation", {
  skip_if_no_helpers()
  
  with_design_server({
    rv.custom$hot$Condition <- c("A", "A", "B", "B")
    session$flushReact()
    
    # No reset happened yet: the edit is kept
    expect_identical(rv.custom$hot$Condition, c("A", "A", "B", "B"))
  })
})

test_that("a reset gives the table again for the current quantitative columns", {
  skip_if_no_helpers()
  cols <- shiny::reactiveVal(quant_cols())
  
  shiny::testServer(
    mod_buildDesign_server,
    args = list(quantCols = cols),
    expr = {
      # Let the observers run their (ignored) initial execution first
      session$flushReact()
      
      cols(paste0("X", 1:3))
      session$setInputs(reset = 1)
      session$flushReact()
      
      expect_identical(rv.custom$hot$quantCols, paste0("X", 1:3))
      expect_identical(rv.custom$hot$Condition, rep("", 3))
    }
  )
})
