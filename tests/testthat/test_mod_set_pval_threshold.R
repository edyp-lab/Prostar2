library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

skip_if_no_pv_deps <- function() {
  skip_if_not_installed("MagellanNTK")
  skip_if_not_installed("shinyWidgets")
  skip_if_not_installed("shinyjs")
}

# Mocks shinyWidgets::updateAutonumericInput().
#   cap$updates : list of list(id, value), in call order
mock_pv <- function(cap, env = parent.frame()) {
  testthat::local_mocked_bindings(
    updateAutonumericInput = function(session, inputId, value = NULL, ...) {
      cap$updates <- c(cap$updates, list(list(id = inputId, value = value)))
      invisible(NULL)
    },
    .package = "shinyWidgets",
    .env = env
  )
}

# Last value sent to a given input
last_update <- function(cap, id) {
  u <- Filter(function(x) identical(x$id, id), cap$updates)
  if (length(u) == 0) return(NULL)
  u[[length(u)]]$value
}

with_pv_server <- function(expr, ...) {
  quo <- rlang::enquo(expr)
  shiny::testServer(mod_set_pval_threshold_server, args = list(...),
                    expr = !!quo)
}

# ---- Tests -----------------------------------------------------------------

test_that("the UI returns a tagList", {
  expect_s3_class(mod_set_pval_threshold_ui("x"), "shiny.tag.list")
})

test_that("the UI has the namespaced outputs", {
  html <- as.character(mod_set_pval_threshold_ui("x"))
  
  for (id in c("thresholdType_UI", "text_pval_UI", "text_log_pval_UI",
               "ApplyThreshold_UI")) {
    expect_match(html, paste0('id="x-', id, '"'), fixed = TRUE, info = id)
  }
})

test_that("the UI is wrapped in a well panel", {
  html <- as.character(mod_set_pval_threshold_ui("x"))
  
  expect_match(html, "well", fixed = TRUE)
  expect_match(html, 'id="pvalPanel"', fixed = TRUE)
})

test_that("the UI uses the namespace given", {
  a <- as.character(mod_set_pval_threshold_ui("a"))
  b <- as.character(mod_set_pval_threshold_ui("b"))
  
  expect_match(a, 'id="a-text_pval_UI"', fixed = TRUE)
  expect_no_match(a, 'id="b-text_pval_UI"', fixed = TRUE)
  expect_match(b, 'id="b-text_pval_UI"', fixed = TRUE)
})

test_that("the demo app is built", {
  expect_s3_class(mod_set_pval_threshold(), "shiny.appobj")
  expect_s3_class(mod_set_pval_threshold(pval_init = 0.05), "shiny.appobj")
})

test_that("the server works with its default arguments", {
  skip_if_no_pv_deps()
  cap <- new.env(); mock_pv(cap)
  
  expect_no_error(shiny::testServer(mod_set_pval_threshold_server, expr = {
    session$flushReact()
  }))
})

test_that("the threshold type starts on 'pval'", {
  skip_if_no_pv_deps()
  cap <- new.env(); mock_pv(cap)
  
  with_pv_server({
    session$flushReact()
    
    expect_identical(rv.widgets$thresholdType, "pval")
  })
})

test_that("the text fields start from pval_init", {
  skip_if_no_pv_deps()
  cap <- new.env(); mock_pv(cap)
  
  with_pv_server({
    session$flushReact()
    
    expect_equal(rv.custom$text_pval, 0.05)
    expect_equal(rv.custom$text_log_pval, -log10(0.05))
  }, pval_init = shiny::reactive(0.05))
})

test_that("the text fields start at 1 and 0 with the default pval_init", {
  skip_if_no_pv_deps()
  cap <- new.env(); mock_pv(cap)
  
  with_pv_server({
    session$flushReact()
    
    expect_equal(rv.custom$text_pval, 1)
    expect_equal(rv.custom$text_log_pval, 0)
  })
})

test_that("the text fields follow a change of pval_init", {
  skip_if_no_pv_deps()
  cap <- new.env(); mock_pv(cap)
  init <- shiny::reactiveVal(0.05)
  
  with_pv_server({
    session$flushReact()
    init(0.01)
    session$flushReact()
    
    expect_equal(rv.custom$text_pval, 0.01)
    expect_equal(rv.custom$text_log_pval, 2)
  }, pval_init = init)
})

test_that("the threshold type selector offers p-value and -log10(p-value)", {
  skip_if_no_pv_deps()
  cap <- new.env(); mock_pv(cap)
  
  with_pv_server({
    html <- output$thresholdType_UI$html
    
    expect_match(html, 'value="pval"', fixed = TRUE)
    expect_match(html, 'value="logpval"', fixed = TRUE)
    expect_match(html, "-log10(p-value)", fixed = TRUE)
    expect_match(html, 'value="pval"[^>]*checked')
  })
})

test_that("the selector follows the threshold type", {
  skip_if_no_pv_deps()
  cap <- new.env(); mock_pv(cap)
  
  with_pv_server({
    rv.widgets$thresholdType <- "logpval"
    html <- output$thresholdType_UI$html
    
    expect_match(html, 'value="logpval"[^>]*checked')
  })
})

test_that("the two numeric fields and the apply button are rendered", {
  skip_if_no_pv_deps()
  cap <- new.env(); mock_pv(cap)
  
  with_pv_server({
    expect_match(output$text_pval_UI$html, "text_pval", fixed = TRUE)
    expect_match(output$text_log_pval_UI$html, "text_log_pval", fixed = TRUE)
    expect_match(output$ApplyThreshold_UI$html, "Apply threshold",
                 fixed = TRUE)
  }, pval_init = shiny::reactive(0.05))
})

test_that("the numeric fields show the initial values", {
  skip_if_no_pv_deps()
  cap <- new.env(); mock_pv(cap)
  
  with_pv_server({
    session$flushReact()
    
    expect_match(output$text_pval_UI$html, "0.05", fixed = TRUE)
  }, pval_init = shiny::reactive(0.05))
})

test_that("editing the p-value updates the -log10 field", {
  skip_if_no_pv_deps()
  cap <- new.env(); mock_pv(cap)
  
  with_pv_server({
    session$flushReact()      # initial (ignored) run
    rv.widgets$thresholdType <- "pval"
    rv.widgets$text_pval <- 0.01
    session$flushReact()
    
    expect_equal(last_update(cap, "text_log_pval"), 2)
  })
})

test_that("editing the -log10 value updates the p-value field", {
  skip_if_no_pv_deps()
  cap <- new.env(); mock_pv(cap)
  
  with_pv_server({
    session$flushReact()
    rv.widgets$thresholdType <- "logpval"
    rv.widgets$text_log_pval <- 3
    session$flushReact()
    
    expect_equal(last_update(cap, "text_pval"), 0.001)
  })
})

test_that("editing the p-value does nothing when the type is 'logpval'", {
  skip_if_no_pv_deps()
  cap <- new.env(); mock_pv(cap)
  
  with_pv_server({
    session$flushReact()
    rv.widgets$thresholdType <- "logpval"
    rv.widgets$text_pval <- 0.01
    session$flushReact()
    
    expect_null(last_update(cap, "text_log_pval"))
  })
})

test_that("editing the -log10 value does nothing when the type is 'pval'", {
  skip_if_no_pv_deps()
  cap <- new.env(); mock_pv(cap)
  
  with_pv_server({
    session$flushReact()
    rv.widgets$thresholdType <- "pval"
    rv.widgets$text_log_pval <- 3
    session$flushReact()
    
    expect_null(last_update(cap, "text_pval"))
  })
})

test_that("the module returns the -log10 threshold when 'Apply' is clicked", {
  skip_if_no_pv_deps()
  cap <- new.env(); mock_pv(cap)
  
  with_pv_server({
    session$flushReact()
    rv.widgets$text_log_pval <- 2
    
    session$setInputs(ApplyThreshold = 1)
    session$flushReact()
    
    expect_equal(session$returned(), 2)
    expect_true(is.numeric(session$returned()))
  })
})

test_that("the returned value is converted to numeric", {
  skip_if_no_pv_deps()
  cap <- new.env(); mock_pv(cap)
  
  with_pv_server({
    session$flushReact()
    rv.widgets$text_log_pval <- "1.5"
    
    session$setInputs(ApplyThreshold = 1)
    session$flushReact()
    
    expect_identical(session$returned(), 1.5)
  })
})

test_that("the returned value always holds the -log10 value, whatever the type", {
  skip_if_no_pv_deps()
  cap <- new.env(); mock_pv(cap)
  
  with_pv_server({
    session$flushReact()
    rv.widgets$thresholdType <- "pval"
    rv.widgets$text_log_pval <- 1.3
    
    session$setInputs(ApplyThreshold = 1)
    session$flushReact()
    
    expect_equal(session$returned(), 1.3)
  })
})

test_that("the returned value follows each click", {
  skip_if_no_pv_deps()
  cap <- new.env(); mock_pv(cap)
  
  with_pv_server({
    session$flushReact()
    rv.widgets$text_log_pval <- 1
    session$setInputs(ApplyThreshold = 1)
    session$flushReact()
    expect_equal(session$returned(), 1)
    
    rv.widgets$text_log_pval <- 4
    session$setInputs(ApplyThreshold = 2)
    session$flushReact()
    expect_equal(session$returned(), 4)
  })
})

test_that("the returned value is empty before any value is set (current behaviour)", {
  skip_if_no_pv_deps()
  cap <- new.env(); mock_pv(cap)
  
  # The apply observer also runs at start (ignoreInit = FALSE, ignoreNULL =
  # FALSE): rv.widgets$text_log_pval is NULL, so as.numeric(NULL) is numeric(0)
  with_pv_server({
    session$flushReact()
    
    expect_length(session$returned(), 0)
  })
})

test_that("a remote reset sends pval_init to both numeric fields", {
  skip_if_no_pv_deps()
  cap <- new.env(); mock_pv(cap)
  remote <- shiny::reactiveVal(0)
  
  with_pv_server({
    session$flushReact()
    remote(1)
    session$flushReact()
    
    expect_equal(last_update(cap, "text_pval"), 0.05)
    expect_equal(last_update(cap, "text_log_pval"), -log10(0.05))
  }, pval_init = shiny::reactive(0.05), remoteReset = remote)
})

test_that("the remote reset does not fire at start", {
  skip_if_no_pv_deps()
  cap <- new.env(); mock_pv(cap)
  
  with_pv_server({
    session$flushReact()
    
    expect_null(cap$updates)
  }, remoteReset = shiny::reactiveVal(0))
})

test_that("a NULL remote reset does nothing", {
  skip_if_no_pv_deps()
  cap <- new.env(); mock_pv(cap)
  remote <- shiny::reactiveVal(0)
  
  with_pv_server({
    session$flushReact()
    remote(NULL)
    session$flushReact()
    
    expect_null(cap$updates)
  }, remoteReset = remote)
})
