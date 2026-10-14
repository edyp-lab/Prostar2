library(testthat)
library(Prostar2)

# ---- Initialization --------------------------------------------------------

skip_if_no_od_deps <- function() {
  skip_if_not_installed("MagellanNTK")
  skip_if_not_installed("DaparToolshed")
  skip_if_not_installed("shinyjs")
  skip_if_not_installed("DaparToolshedData")
}

with_od_server <- function(expr, ...) {
  quo <- rlang::enquo(expr)
  shiny::testServer(open_dataset_server, args = list(...), expr = !!quo)
}

# Table of datasets found in packages
pkgs_od <- function() {
  data.frame(Package = c("pkgA", "pkgA", "pkgB"),
             Item = c("dsA1", "dsA2", "dsB1"),
             stringsAsFactors = FALSE)
}

# Mocks the collaborators.
#   cap$list    : arguments given to GetListDatasets()
#   cap$getdata : name given to DaparToolshedData::GetData()
#   cap$pkgs    : value given to pkgs_require()
#   cap$toggle  : TRUE if shinyjs::toggleState() was called
# `get_data` is the function used as DaparToolshedData::GetData().
mock_od <- function(cap, get_data = function(name) list(a = "dataset"),
                    env = parent.frame()) {
  testthat::local_mocked_bindings(
    GetListDatasets = function(class, demo_package, ...) {
      cap$list <- list(class = class, demo_package = demo_package)
      pkgs_od()
    },
    .package = "MagellanNTK",
    .env = env
  )
  testthat::local_mocked_bindings(
    GetData = function(name, ...) {
      cap$getdata <- name
      get_data(name)
    },
    DaparToolshedData = function(...) {
      data.frame(Title = c("demoX", "demoY"), stringsAsFactors = FALSE)
    },
    .package = "DaparToolshedData",
    .env = env
  )
  testthat::local_mocked_bindings(
    paramshistory = function(object, ...) {
      data.frame(a = 1, b = 2, c = 3, d = 4)
    },
    `paramshistory<-` = function(object, value) object,
    .package = "DaparToolshed",
    .env = env
  )
  testthat::local_mocked_bindings(
    toggleState = function(...) {
      cap$toggle <- TRUE
      invisible(NULL)
    },
    .package = "shinyjs",
    .env = env
  )
  testthat::local_mocked_bindings(
    pkgs_require = function(ll.deps) {
      cap$pkgs <- ll.deps
      invisible(NULL)
    },
    infos_dataset_server = function(id, dataIn, ...) {
      cap$infos <- list(id = id, dataIn = dataIn)
      invisible(NULL)
    },
    infos_dataset_ui = function(id) shiny::div(id = id, "fake infos"),
    .package = "Prostar2",
    .env = env
  )
}

# Selects the "package dataset" source and a package
choose_pkg <- function(session, pkg) {
  session$setInputs(chooseSource = "packageDataset")
  session$flushReact()
  output_choosePkg <- NULL
  session$setInputs(pkg = pkg)
  session$flushReact()
}

# ---- Tests -----------------------------------------------------------------

test_that("open_dataset_ui returns a tagList", {
  expect_s3_class(open_dataset_ui("x"), "shiny.tag.list")
})

test_that("open_dataset_ui has the namespaced outputs", {
  html <- as.character(open_dataset_ui("x"))
  
  for (id in c("chooseSource_UI", "customDataset_UI", "packageDataset_UI",
               "Description_infos_dataset_UI")) {
    expect_match(html, paste0('id="x-', id, '"'), fixed = TRUE, info = id)
  }
})

test_that("open_dataset_ui displays the three selectors inline", {
  html <- as.character(open_dataset_ui("x"))
  
  expect_equal(
    lengths(regmatches(html, gregexpr("display: inline-block", html, fixed = TRUE))),
    3
  )
})

test_that("open_dataset_ui uses the namespace given", {
  a <- as.character(open_dataset_ui("a"))
  b <- as.character(open_dataset_ui("b"))
  
  expect_match(a, 'id="a-chooseSource_UI"', fixed = TRUE)
  expect_no_match(a, 'id="b-chooseSource_UI"', fixed = TRUE)
  expect_match(b, 'id="b-chooseSource_UI"', fixed = TRUE)
})

test_that("open_dataset returns a shiny app", {
  expect_s3_class(open_dataset(), "shiny.appobj")
})

test_that("the server works with its default arguments", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  
  expect_no_error(shiny::testServer(open_dataset_server, expr = {
    session$flushReact()
  }))
})

test_that("the widgets and custom values start with their default values", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  
  with_od_server({
    session$flushReact()
    
    expect_identical(rv.widgets$chooseSource, "customDataset")
    expect_identical(rv.widgets$demoDataset, "None")
    expect_null(rv.custom$dataRead)
    expect_identical(rv.custom$name, "default.name")
    expect_identical(rv.custom$error_msg, "")
  })
})

test_that("the module returns an empty selection with a trigger at start", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  
  with_od_server({
    res <- session$returned()
    
    expect_false(is.null(res$trigger))
    expect_null(res$name)
    expect_null(res$dataset)
  })
})

test_that("the source selector offers the two sources, custom first", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  
  with_od_server({
    html <- output$chooseSource_UI$html
    
    expect_match(html, "Dataset source", fixed = TRUE)
    expect_match(html, 'value="customDataset"', fixed = TRUE)
    expect_match(html, 'value="packageDataset"', fixed = TRUE)
    expect_match(html, 'value="customDataset"[^>]*selected')
  })
})

test_that("the file input is shown for a custom dataset, with the extensions", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  
  with_od_server({
    html <- output$customDataset_UI$html
    
    expect_match(html, "Open file", fixed = TRUE)
    expect_match(html, 'type="file"', fixed = TRUE)
    expect_match(html, ".rds", fixed = TRUE)
  }, extension = c(".rds", ".RData"))
})

test_that("the file input is hidden for a package dataset", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  
  with_od_server({
    session$setInputs(chooseSource = "packageDataset")
    session$flushReact()
    
    expect_error(output$customDataset_UI, class = "shiny.silent.error")
  })
})

test_that("the package selectors are hidden for a custom dataset", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  
  with_od_server({
    expect_error(output$packageDataset_UI, class = "shiny.silent.error")
    expect_error(output$choosePkg, class = "shiny.silent.error")
    expect_error(output$chooseDemoDataset, class = "shiny.silent.error")
  })
})

test_that("the package selectors are shown for a package dataset", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  
  with_od_server({
    session$setInputs(chooseSource = "packageDataset")
    session$flushReact()
    html <- output$packageDataset_UI$html
    
    for (id in c("choosePkg", "chooseDemoDataset", "error_msg_ui",
                 "load_btn_UI")) {
      expect_match(html, id, fixed = TRUE, info = id)
    }
  })
})

test_that("the packages are searched with the class and the demo package", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  
  with_od_server({
    session$setInputs(chooseSource = "packageDataset")
    session$flushReact()
    output$choosePkg
    
    expect_identical(cap$list$class, "QFeatures")
    expect_identical(cap$list$demo_package, "myDemoPkg")
  }, class = "QFeatures", demo_package = "myDemoPkg")
})

test_that("the package list contains DaparToolshedData and the packages found", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  
  with_od_server({
    session$setInputs(chooseSource = "packageDataset")
    session$flushReact()
    html <- output$choosePkg$html
    
    expect_match(html, "Choose package", fixed = TRUE)
    for (p in c("DaparToolshedData", "pkgA", "pkgB")) {
      expect_match(html, p, fixed = TRUE, info = p)
    }
  })
})

test_that("the demo datasets of DaparToolshedData come from its titles", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  
  with_od_server({
    choose_pkg(session, "DaparToolshedData")
    html <- output$chooseDemoDataset$html
    
    expect_identical(cap$pkgs, "DaparToolshedData")
    expect_match(html, "demoX", fixed = TRUE)
    expect_match(html, "demoY", fixed = TRUE)
  })
})

test_that("the demo datasets of another package come from the table of datasets", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  
  with_od_server({
    choose_pkg(session, "pkgA")
    html <- output$chooseDemoDataset$html
    
    expect_identical(cap$pkgs, "pkgA")
    expect_match(html, "dsA1", fixed = TRUE)
    expect_match(html, "dsA2", fixed = TRUE)
    expect_no_match(html, "dsB1", fixed = TRUE)
  })
})

test_that("the demo datasets wait for a package", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  
  with_od_server({
    session$setInputs(chooseSource = "packageDataset")
    session$flushReact()
    
    expect_error(output$chooseDemoDataset, class = "shiny.silent.error")
  })
})

test_that("the load button waits for a package and a demo dataset", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  
  with_od_server({
    session$setInputs(chooseSource = "packageDataset")
    session$flushReact()
    expect_error(output$load_btn_UI, class = "shiny.silent.error")
  })
})

test_that("the load button is shown once a package and a dataset are chosen", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  
  with_od_server({
    choose_pkg(session, "DaparToolshedData")
    session$setInputs(demoDataset = "demoX")
    session$flushReact()
    
    expect_match(output$load_btn_UI$html, "Load file", fixed = TRUE)
  })
})

test_that("the error message is hidden when empty and shown when set", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  
  with_od_server({
    expect_error(output$error_msg_ui, class = "shiny.silent.error")
    
    rv.custom$error_msg <- "Something failed"
    session$flushReact()
    html <- output$error_msg_ui$html
    
    expect_match(html, "Something failed", fixed = TRUE)
    expect_match(html, "color: red", fixed = TRUE)
  })
})

# ---- open_dataset_server: loading a demo dataset ---------------------------

# Chooses DaparToolshedData and a demo dataset, then clicks 'Load file'
load_demo <- function(session, name = "demoX") {
  choose_pkg(session, "DaparToolshedData")
  session$setInputs(demoDataset = name)
  session$flushReact()
  session$setInputs(load_dataset_btn = 1)
  session$flushReact()
}

test_that("nothing is loaded while no demo dataset is chosen", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  
  with_od_server({
    session$setInputs(load_dataset_btn = 1)
    session$flushReact()
    
    expect_null(cap$getdata)
    expect_null(session$returned()$dataset)
  })
})

test_that("a DaparToolshedData dataset is loaded and returned", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  
  with_od_server({
    load_demo(session)
    res <- session$returned()
    
    expect_identical(cap$getdata, "demoX")
    expect_identical(res$dataset, list(a = "dataset"))
    expect_identical(res$name, "demoX")
    expect_identical(rv.custom$dataRead, list(a = "dataset"))
    expect_identical(rv.custom$error_msg, "")
  })
})

test_that("loading a dataset updates the trigger", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  
  with_od_server({
    before <- session$returned()$trigger
    Sys.sleep(0.01)
    load_demo(session)
    
    expect_false(identical(session$returned()$trigger, before))
  })
})

test_that("a failed download shows an error message and returns nothing", {
  skip_if_no_od_deps()
  cap <- new.env()
  mock_od(cap, get_data = function(name) stop("no internet"))
  
  with_od_server({
    load_demo(session)
    
    expect_match(rv.custom$error_msg, "Unable to download the requested dataset",
                 fixed = TRUE)
    expect_null(session$returned()$dataset)
    expect_null(rv.custom$dataRead)
  })
})

test_that("the load button is toggled around the loading", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  
  with_od_server({
    load_demo(session)
    
    expect_true(cap$toggle)
  })
})

# ---- open_dataset_server: opening a file -----------------------------------

test_that("an RDS file is read and returned with its file name", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(a = "from rds"), path)
  
  with_od_server({
    capture.output(
      session$setInputs(file = list(name = "mydata.rds", datapath = path))
    )
    res <- session$returned()
    
    expect_identical(res$dataset, list(a = "from rds"))
    expect_identical(res$name, "mydata.rds")
    expect_identical(rv.custom$dataRead, list(a = "from rds"))
  })
})

test_that("an RData file is loaded and the object named like the file is returned", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  dir <- withr::local_tempdir()
  path <- file.path(dir, "myobj.RData")
  myobj <- list(a = "from RData")
  save(myobj, file = path)
  
  with_od_server({
    capture.output(
      session$setInputs(file = list(name = "myobj.RData", datapath = path))
    )
    res <- session$returned()
    
    expect_identical(res$dataset, list(a = "from RData"))
    expect_identical(res$name, "myobj")
  })
})

test_that("a file which can not be read gives an empty dataset", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  path <- withr::local_tempfile(fileext = ".txt")
  writeLines("not an R file", path)
  
  with_od_server({
    capture.output(
      session$setInputs(file = list(name = "bad.txt", datapath = path))
    )
    
    expect_null(rv.custom$dataRead)
    expect_null(session$returned()$dataset)
  })
})

# ---- open_dataset_server: description --------------------------------------

test_that("the description waits for a dataset", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  
  with_od_server({
    expect_error(output$Description_infos_dataset_UI,
                 class = "shiny.silent.error")
    expect_null(cap$infos)
  })
})

test_that("the description starts the infos module on the dataset", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  
  with_od_server({
    load_demo(session)
    html <- output$Description_infos_dataset_UI$html
    
    expect_identical(cap$infos$id, "Description_infosdataset")
    expect_identical(cap$infos$dataIn(), list(a = "dataset"))
    expect_match(html, "Description_infosdataset", fixed = TRUE)
  })
})

# ---- open_dataset_server: remote reset -------------------------------------

test_that("a remote reset clears the returned selection", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  remote <- shiny::reactiveVal(NULL)
  
  with_od_server({
    load_demo(session)
    expect_false(is.null(session$returned()$dataset))
    
    remote(1)
    session$flushReact()
    res <- session$returned()
    
    expect_null(res$dataset)
    expect_null(res$name)
    expect_null(rv.custom$dataRead)
    expect_identical(rv.custom$remoteReset, 1)
  }, remoteReset = remote)
})

test_that("a NULL remote reset does nothing", {
  skip_if_no_od_deps()
  cap <- new.env(); mock_od(cap)
  remote <- shiny::reactiveVal(NULL)
  
  with_od_server({
    load_demo(session)
    remote(NULL)
    session$flushReact()
    
    expect_false(is.null(session$returned()$dataset))
  }, remoteReset = remote)
})
