library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

skip_if_no_ff_deps <- function() {
  skip_if_not_installed("MagellanNTK")
  skip_if_not_installed("DaparToolshed")
  skip_if_not_installed("shinyjs")
}

# 4 features x 4 samples
make_se_ff <- function(offset = 0) {
  SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = matrix(
      seq_len(16) + offset, 4,
      dimnames = list(paste0("f", 1:4), paste0("S", 1:4))
    ))
  )
}

conds_ff <- function() c("A", "A", "B", "B")

# Mocks the collaborators.
#   cap$tree    : id given to mod_metacell_tree_server()
#   cap$filter  : arguments given to filterFeaturesOneSE()
#   cap$nbvals  : arguments given to getListNbValuesInLines()
# `tags` is the reactive returned by the fake tree module.
# `kept` = rownames kept by the fake filter.
mock_ff <- function(cap,
                    tags = shiny::reactiveVal(list(trigger = NULL, values = NULL)),
                    kept = c("f1", "f2"),
                    env = parent.frame()) {
  testthat::local_mocked_bindings(
    mod_metacell_tree_server = function(id, dataIn, ...) {
      cap$tree <- list(id = id, dataIn = dataIn)
      tags
    },
    mod_metacell_tree_ui = function(id) shiny::div(id = id, "fake tree"),
    mod_filtering_example_ui = function(id) shiny::div(id = id, "fake example"),
    mod_filtering_example_server = function(...) invisible(NULL),
    getListNbValuesInLines = function(object, conds, type, ...) {
      cap$nbvals <- list(conds = conds, type = type)
      c("0", "1", "2")
    },
    .package = "Prostar2",
    .env = env
  )
  testthat::local_mocked_bindings(
    QFeaturesFromSE = function(obj.se, colData, name, ...) list(),
    filterFeaturesOneSE = function(object, i, name, filters, ...) {
      cap$filter <- list(name = name, filters = filters)
      list(make_se_ff(), make_se_ff()[kept, ])
    },
    .package = "DaparToolshed",
    .env = env
  )
}

with_ff_server <- function(expr, ...) {
  quo <- rlang::enquo(expr)
  args <- modifyList(
    list(dataIn = shiny::reactive(make_se_ff()),
         conds = shiny::reactive(conds_ff())),
    list(...)
  )
  shiny::testServer(mod_qMetacell_FunctionFilter_Generator_server,
                    args = args, expr = !!quo)
}

  set_widgets <- function(rv.widgets, session, scope, valPercent) {
    session$flushReact()
    rv.widgets$scope <- scope
    session$flushReact()          # the observer fires here
    rv.widgets$valPercent <- valPercent
    session$flushReact()
  }

# ---- Tests -----------------------------------------------------------------

test_that("the UI returns a tagList", {
  expect_s3_class(mod_qMetacell_FunctionFilter_Generator_ui("x"),
                  "shiny.tag.list")
})

test_that("the UI has the namespaced outputs", {
  html <- as.character(mod_qMetacell_FunctionFilter_Generator_ui("x"))
  
  for (id in c("tree_UI", "chooseKeepRemove_ui", "chooseScope_ui",
               "qMetacellScope_widgets_set2_ui", "qMetacellScope_request_ui",
               "Preview_UI", "Add_btn_UI")) {
    expect_match(html, paste0('id="x-', id, '"'), fixed = TRUE, info = id)
  }
})

test_that("the UI uses the namespace given", {
  a <- as.character(mod_qMetacell_FunctionFilter_Generator_ui("a"))
  b <- as.character(mod_qMetacell_FunctionFilter_Generator_ui("b"))
  
  expect_match(a, 'id="a-tree_UI"', fixed = TRUE)
  expect_no_match(a, 'id="b-tree_UI"', fixed = TRUE)
  expect_match(b, 'id="b-tree_UI"', fixed = TRUE)
})

test_that("the demo app is built", {
  skip_if_not_installed("DaparToolshed")
  
  app <- mod_qMetacell_FunctionFilter_Generator(
    list(make_se_ff()), conds_ff()
  )
  
  expect_s3_class(app, "shiny.appobj")
})

test_that("the server works with its default arguments", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  expect_no_error(shiny::testServer(
    mod_qMetacell_FunctionFilter_Generator_server,
    args = list(conds = shiny::reactive(conds_ff())),
    expr = { session$flushReact() }
  ))
})

test_that("the widgets start with their default values", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  with_ff_server({
    session$flushReact()
    
    expect_identical(rv.widgets$tag, "None")
    expect_identical(rv.widgets$scope, "None")
    expect_identical(rv.widgets$keep_vs_remove, "delete")
    expect_identical(rv.widgets$valPercent, "Percentage")
    expect_identical(rv.widgets$operator, "None")
  })
})

test_that("the dataset is stored and the tree module is started", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  se <- make_se_ff()
  
  with_ff_server({
    session$flushReact()
    
    expect_identical(rv$dataIn, se)
    expect_identical(cap$tree$id, "tree")
    expect_identical(cap$tree$dataIn(), se)
  }, dataIn = shiny::reactive(se))
})

test_that("a dataset which is not a SummarizedExperiment is ignored", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  with_ff_server({
    session$flushReact()
    
    expect_null(rv$dataIn)
    expect_null(cap$tree)
  }, dataIn = shiny::reactive(list(a = 1)))
})

test_that("choosing tags in the tree updates the widgets and the output", {
  skip_if_no_ff_deps()
  cap <- new.env()
  tags <- shiny::reactiveVal(list(trigger = NULL, values = NULL))
  mock_ff(cap, tags = tags)
  
  with_ff_server({
    session$flushReact()
    
    tags(list(trigger = 1, values = c("Missing POV", "Missing MEC")))
    session$flushReact()
    res <- session$returned()
    
    expect_identical(rv.widgets$tag, c("Missing POV", "Missing MEC"))
    expect_identical(res$value$ll.pattern, c("Missing POV", "Missing MEC"))
    expect_null(res$value$ll.fun)
    expect_null(res$value$ll.indices)
    expect_false(is.null(res$trigger))
  })
})

test_that("choosing tags resets the other widgets", {
  skip_if_no_ff_deps()
  cap <- new.env()
  tags <- shiny::reactiveVal(list(trigger = NULL, values = NULL))
  mock_ff(cap, tags = tags)
  
  with_ff_server({
    session$flushReact()
    rv.widgets$scope <- "WholeMatrix"
    rv.widgets$keep_vs_remove <- "keep"
    rv.widgets$operator <- "<"
    
    tags(list(trigger = 2, values = "Missing POV"))
    session$flushReact()
    
    expect_identical(rv.widgets$scope, "None")
    expect_identical(rv.widgets$keep_vs_remove, "delete")
    expect_identical(rv.widgets$operator, "None")
  })
})

test_that("the tree and the buttons are rendered", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  with_ff_server({
    expect_match(output$tree_UI$html, "fake tree", fixed = TRUE)
    expect_match(output$Add_btn_UI$html, "Add filter", fixed = TRUE)
    expect_match(output$Preview_UI$html, "Preview", fixed = TRUE)
    expect_match(output$Preview_UI$html, "fake example", fixed = TRUE)
  })
})

test_that("the keep/remove and scope selectors wait for a tag", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  with_ff_server({
    expect_error(output$chooseKeepRemove_ui, class = "shiny.silent.error")
    expect_error(output$chooseScope_ui, class = "shiny.silent.error")
  })
})

test_that("the keep/remove selector offers delete and keep", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  with_ff_server({
    rv.widgets$tag <- "Missing POV"
    html <- output$chooseKeepRemove_ui$html
    
    expect_match(html, "Type of filter operation", fixed = TRUE)
    expect_match(html, 'value="delete"', fixed = TRUE)
    expect_match(html, 'value="keep"', fixed = TRUE)
  })
})

test_that("the scope selector offers the four scopes", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  with_ff_server({
    rv.widgets$tag <- "Missing POV"
    html <- output$chooseScope_ui$html
    
    for (v in c("None", "WholeLine", "WholeMatrix", "AllCond",
                "AtLeastOneCond")) {
      expect_match(html, paste0('value="', v, '"'), fixed = TRUE, info = v)
    }
  })
})

test_that("the threshold widgets wait for a scope other than None or WholeLine", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  with_ff_server({
    rv.widgets$tag <- "Missing POV"
    
    rv.widgets$scope <- "None"
    expect_error(output$qMetacellScope_widgets_set2_ui,
                 class = "shiny.silent.error")
    
    rv.widgets$scope <- "WholeLine"
    expect_error(output$qMetacellScope_widgets_set2_ui,
                 class = "shiny.silent.error")
  })
})

test_that("the threshold widgets are shown for a matrix scope", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  with_ff_server({
    rv.widgets$tag <- "Missing POV"
    rv.widgets$scope <- "WholeMatrix"
    html <- output$qMetacellScope_widgets_set2_ui$html
    
    expect_match(html, "Threshold", fixed = TRUE)
    expect_match(html, "Operator", fixed = TRUE)
    expect_match(html, 'value="Count"', fixed = TRUE)
    expect_match(html, 'value="Percentage"', fixed = TRUE)
  })
})

test_that("the count selector uses the numbers of values in the lines", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  with_ff_server({
    set_widgets(rv.widgets, session, scope = "AllCond", valPercent = "Count")
    html <- output$value_ui$html
    
    expect_identical(cap$nbvals$type, "AllCond")
    expect_identical(cap$nbvals$conds, conds_ff())
    expect_match(html, 'value="2"', fixed = TRUE)
  })
})

test_that("the count selector waits for the 'Count' threshold type", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  with_ff_server({
    set_widgets(rv.widgets, session, scope = "AllCond", valPercent = "Percentage")
    
    expect_error(output$value_ui, class = "shiny.silent.error")
  })
})

test_that("the percentage slider is shown for the 'Percentage' threshold type", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  with_ff_server({
    set_widgets(rv.widgets, session, scope = "AllCond", valPercent = "Percentage")
    html <- output$percentage_ui$html
    
    expect_match(html, "percentTh", fixed = TRUE)
    expect_match(html, 'data-max="100"', fixed = TRUE)
  })
})

test_that("the percentage slider waits for the 'Percentage' threshold type", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  with_ff_server({
    set_widgets(rv.widgets, session, scope = "AllCond", valPercent = "Count")
    
    expect_error(output$percentage_ui, class = "shiny.silent.error")
  })
})

test_that("the query says that nothing is filtered without scope", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  with_ff_server({
    expect_identical(WriteQuery(), "No filtering is processed.")
  })
})

test_that("the query describes a whole line filter", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  with_ff_server({
    rv.widgets$scope <- "WholeLine"
    rv.widgets$tag <- "Missing POV"
    rv.widgets$keep_vs_remove <- "keep"
    
    expect_identical(WriteQuery(),
                     "keep lines that contain only ( Missing POV )")
  })
})

test_that("the query lists several tags separated by commas", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  with_ff_server({
    rv.widgets$scope <- "WholeLine"
    rv.widgets$tag <- c("Missing POV", "Missing MEC")
    
    expect_match(WriteQuery(), "Missing POV, Missing MEC", fixed = TRUE)
  })
})

test_that("the query describes a count threshold on the whole matrix", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  with_ff_server({
    rv.widgets$scope <- "WholeMatrix"
    rv.widgets$tag <- "Missing POV"
    rv.widgets$valPercent <- "Count"
    rv.widgets$valueTh <- 3
    rv.widgets$operator <- "<"
    q <- WriteQuery()
    
    expect_match(q, "^delete  lines where number of \\(")
    expect_match(q, "Missing POV", fixed = TRUE)
    expect_match(q, "delete  lines where number of ( Missing POV ) data  <   3  in  the whole matrix.", fixed = TRUE)
  })
})

test_that("the query describes a percentage threshold on each condition", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  with_ff_server({
    rv.widgets$scope <- "AllCond"
    rv.widgets$tag <- "Missing POV"
    rv.widgets$valPercent <- "Percentage"
    rv.widgets$percentTh <- 40
    rv.widgets$operator <- ">="
    q <- WriteQuery()
    
    expect_match(q, "40 %", fixed = TRUE)
    expect_match(q, "each condition.", fixed = TRUE)
  })
})

test_that("the query describes a threshold on at least one condition", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  with_ff_server({
    rv.widgets$scope <- "AtLeastOneCond"
    rv.widgets$tag <- "Missing POV"
    rv.widgets$valPercent <- "Count"
    rv.widgets$valueTh <- 1
    
    expect_match(WriteQuery(), "at least one condition.", fixed = TRUE)
  })
})

test_that("the request text is built by an output not shown in the UI (current behaviour)", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  # The server defines `qMetacellFilter_request_ui` while the UI shows
  # `qMetacellScope_request_ui`: the text is never displayed
  with_ff_server({
    expect_match(output$qMetacellFilter_request_ui$html, "You are about to",
                 fixed = TRUE)
    expect_error(output$qMetacellScope_request_ui)
  })
})

test_that("the indices computation waits for a dataset, a tag and a scope", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  with_ff_server({
    session$flushReact()
    
    expect_error(GetIndicesAndFunction(), class = "shiny.silent.error")
    
    rv.widgets$tag <- "Missing POV"
    expect_error(GetIndicesAndFunction(), class = "shiny.silent.error")
  })
})

test_that("a 'delete' whole line filter gives the indices of the removed lines", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap, kept = c("f1", "f2"))
  
  with_ff_server({
    session$flushReact()
    rv.widgets$tag <- "Missing POV"
    rv.widgets$scope <- "WholeLine"
    rv.widgets$keep_vs_remove <- "delete"
    
    GetIndicesAndFunction()
    
    expect_equal(rv.custom$indices, c(3, 4))
    expect_identical(rv.custom$ll.fun@params$cmd, "delete")
    expect_identical(rv.custom$ll.fun@params$pattern, "Missing POV")
  })
})

test_that("a 'keep' whole line filter gives the indices of the kept lines", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap, kept = c("f1", "f2"))
  
  with_ff_server({
    session$flushReact()
    rv.widgets$tag <- "Missing POV"
    rv.widgets$scope <- "WholeLine"
    rv.widgets$keep_vs_remove <- "keep"
    
    GetIndicesAndFunction()
    
    expect_equal(rv.custom$indices, c(1, 2))
  })
})

test_that("the filter is applied with the function filter on the last assay", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  with_ff_server({
    session$flushReact()
    rv.widgets$tag <- "Missing POV"
    rv.widgets$scope <- "WholeLine"
    
    GetIndicesAndFunction()
    
    expect_length(cap$filter$filters, 1)
    expect_identical(cap$filter$filters[[1]], rv.custom$ll.fun)
    expect_match(cap$filter$name, "^qMetacellFiltered")
  })
})

test_that("a matrix scope waits for an operator and a threshold", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  with_ff_server({
    session$flushReact()
    rv.widgets$tag <- "Missing POV"
    rv.widgets$scope <- "WholeMatrix"
    rv.widgets$valPercent <- "Count"
    rv.widgets$valueTh <- NULL       
    rv.widgets$operator <- "<"
    
    expect_error(GetIndicesAndFunction(), class = "shiny.silent.error")
    
    rv.widgets$valueTh <- 2
    rv.widgets$operator <- "None"
    expect_error(GetIndicesAndFunction(), class = "shiny.silent.error")
  })
})

test_that("a matrix scope builds a filter with its threshold and operator", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  with_ff_server({
    session$flushReact()
    rv.widgets$tag <- "Missing POV"
    rv.widgets$scope <- "WholeMatrix"
    rv.widgets$valPercent <- "Count"
    rv.widgets$valueTh <- 2
    rv.widgets$operator <- "<"
    
    GetIndicesAndFunction()
    p <- rv.custom$ll.fun@params
    
    expect_identical(p$th, 2L)
    expect_identical(p$operator, "<")
    expect_false(p$percent)
  })
})

test_that("a percentage threshold is given as a proportion", {
  skip_if_no_ff_deps()
  cap <- new.env(); mock_ff(cap)
  
  with_ff_server({
    session$flushReact()
    rv.widgets$tag <- "Missing POV"
    rv.widgets$scope <- "AllCond"
    rv.widgets$valPercent <- "Percentage"
    rv.widgets$percentTh <- 40
    rv.widgets$operator <- ">="
    
    GetIndicesAndFunction()
    p <- rv.custom$ll.fun@params
    
    expect_equal(p$th, 0.4)
    expect_true(p$percent)
    expect_identical(p$conds, conds_ff())
  })
})
