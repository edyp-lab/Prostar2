library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

# Mocks the dependencies of Prostar2() and records what happens in 'cap':
#   cap$pkgs   : packages given to pkgs_require()
#   cap$mag    : arguments given to MagellanNTK::MagellanNTK()
#   cap$order  : order in which the dependencies were called
mock_prostar2 <- function(cap, magellan_value = "app_result",
                          env = parent.frame()) {
  testthat::local_mocked_bindings(
    pkgs_require = function(pkgs, ...) {
      cap$pkgs <- pkgs
      cap$order <- c(cap$order, "pkgs_require")
      invisible(NULL)
    },
    .package = "Prostar2",
    .env = env
  )
  testthat::local_mocked_bindings(
    MagellanNTK = function(...) {
      cap$mag <- list(...)
      cap$order <- c(cap$order, "MagellanNTK")
      magellan_value
    },
    .package = "MagellanNTK",
    .env = env
  )
}

wf_path <- function(name) {
  system.file(paste0("workflow/", name), package = "Prostar2")
}

# ---- Tests -----------------------------------------------------------------

## ----- Prostar2 -----
test_that("Prostar2 checks that MagellanNTK and omXplore are available", {
  cap <- new.env()
  mock_prostar2(cap)
  
  Prostar2("PipelineProtein")
  
  expect_identical(cap$pkgs, c("MagellanNTK", "omXplore"))
})

test_that("Prostar2 checks the packages before launching the app", {
  cap <- new.env()
  mock_prostar2(cap)
  
  Prostar2("PipelineProtein")
  
  expect_identical(cap$order, c("pkgs_require", "MagellanNTK"))
})

test_that("Prostar2 does not launch the app when a package is missing", {
  cap <- new.env()
  mock_prostar2(cap)
  testthat::local_mocked_bindings(
    pkgs_require = function(...) stop("missing package"),
    .package = "Prostar2"
  )
  
  expect_error(Prostar2("PipelineProtein"))
  expect_null(cap$mag)
})

test_that("Prostar2 calls MagellanNTK once with the expected argument names", {
  cap <- new.env()
  mock_prostar2(cap)
  
  Prostar2("PipelineProtein")
  
  expect_named(
    cap$mag,
    c("obj", "workflow.path", "workflow.name", "convert.path")
  )
  expect_identical(sum(cap$order == "MagellanNTK"), 1L)
})

test_that("Prostar2 starts MagellanNTK without any dataset", {
  cap <- new.env()
  mock_prostar2(cap)
  
  Prostar2("PipelineProtein")
  
  expect_null(cap$mag$obj)
})

test_that("Prostar2 gives the workflow name unchanged to MagellanNTK", {
  cap <- new.env()
  mock_prostar2(cap)
  
  Prostar2("PipelineProtein_Filtering")
  
  expect_identical(cap$mag$workflow.name, "PipelineProtein_Filtering")
})

test_that("Prostar2 builds the workflow path from the part before the underscore", {
  cap <- new.env()
  mock_prostar2(cap)
  
  Prostar2("PipelineProtein_Normalization")
  
  expect_identical(cap$mag$workflow.path, wf_path("PipelineProtein"))
})

test_that("Prostar2 uses the same workflow path for all sub-workflows of a pipeline", {
  cap <- new.env()
  mock_prostar2(cap)
  
  Prostar2("PipelineProtein_Filtering")
  path_1 <- cap$mag$workflow.path
  Prostar2("PipelineProtein_Imputation")
  path_2 <- cap$mag$workflow.path
  
  expect_identical(path_1, path_2)
})

test_that("Prostar2 accepts a workflow name without underscore", {
  cap <- new.env()
  mock_prostar2(cap)
  
  Prostar2("PipelineProtein")
  
  expect_identical(cap$mag$workflow.name, "PipelineProtein")
  expect_identical(cap$mag$workflow.path, wf_path("PipelineProtein"))
})

test_that("Prostar2 uses PipelineProtein_Convert as the default converter", {
  cap <- new.env()
  mock_prostar2(cap)
  
  Prostar2("PipelineProtein")
  
  expect_identical(cap$mag$convert.path, wf_path("PipelineProtein"))
})

test_that("Prostar2 builds the convert path from the part of convert.name before the underscore", {
  cap <- new.env()
  mock_prostar2(cap)
  
  Prostar2("PipelineProtein", convert.name = "PipelinePeptide_Convert")
  
  expect_identical(cap$mag$convert.path, wf_path("PipelinePeptide"))
})

test_that("Prostar2 handles different workflow and converter pipelines", {
  cap <- new.env()
  mock_prostar2(cap)
  
  Prostar2("PipelineProtein_Filtering",
           convert.name = "PipelinePeptide_Convert")
  
  expect_identical(cap$mag$workflow.path, wf_path("PipelineProtein"))
  expect_identical(cap$mag$convert.path, wf_path("PipelinePeptide"))
})

test_that("Prostar2 gives an empty path for an unknown pipeline", {
  cap <- new.env()
  mock_prostar2(cap)
  
  # system.file() returns "" when the folder does not exist
  Prostar2("UnknownPipeline_Step", convert.name = "OtherPipeline_Convert")
  
  expect_identical(cap$mag$workflow.path, "")
  expect_identical(cap$mag$convert.path, "")
})

test_that("Prostar2 returns the value returned by MagellanNTK", {
  cap <- new.env()
  mock_prostar2(cap, magellan_value = "app_result")
  
  expect_identical(Prostar2("PipelineProtein"), "app_result")
})

test_that("Prostar2 does not forward usermod and verbose to MagellanNTK", {
  cap <- new.env()
  mock_prostar2(cap)
  
  Prostar2("PipelineProtein", usermod = "dev", verbose = TRUE)
  
  expect_false(any(c("usermod", "verbose") %in% names(cap$mag)))
})

test_that("Prostar2 errors when wf.name is NULL (default)", {
  cap <- new.env()
  mock_prostar2(cap)
  
  # strsplit(NULL, "_") fails: wf.name has no usable default
  expect_error(Prostar2())
  expect_null(cap$mag)
})

test_that("Prostar2 errors when wf.name is not a character", {
  cap <- new.env()
  mock_prostar2(cap)
  
  expect_error(Prostar2(wf.name = 1))
  expect_null(cap$mag)
})

test_that("Prostar2 errors when convert.name is NULL", {
  cap <- new.env()
  mock_prostar2(cap)
  
  expect_error(Prostar2("PipelineProtein", convert.name = NULL))
  expect_null(cap$mag)
})
