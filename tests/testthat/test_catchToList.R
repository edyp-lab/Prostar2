library(testthat)
library(Prostar2)

# ---- Tests -----------------------------------------------------------------

## ----- catchToList -----
test_that("catchToList returns a list with 'value', 'warnings' and 'error'", {
  res <- catchToList(1 + 1)
  
  expect_type(res, "list")
  expect_named(res, c("value", "warnings", "error"))
})

test_that("catchToList returns the value of a successful expression", {
  res <- catchToList(mean(1:10))
  
  expect_equal(res$value, 5.5)
  expect_null(res$warnings)
  expect_null(res$error)
})

test_that("catchToList preserves the type and structure of the value", {
  expect_equal(catchToList(1:3)$value, 1:3)
  expect_equal(catchToList("a")$value, "a")
  expect_equal(catchToList(list(a = 1, b = "x"))$value, list(a = 1, b = "x"))
  expect_equal(
    catchToList(data.frame(x = 1:2))$value,
    data.frame(x = 1:2)
  )
})

test_that("catchToList returns NULL value without warnings or error for NULL", {
  res <- catchToList(NULL)
  
  expect_null(res$value)
  expect_null(res$warnings)
  expect_null(res$error)
})

test_that("catchToList evaluates a multi-statement block and returns the last value", {
  res <- catchToList({
    x <- 2
    y <- 3
    x * y
  })
  
  expect_equal(res$value, 6)
  expect_null(res$warnings)
  expect_null(res$error)
})

test_that("catchToList captures a single warning and still returns the value", {
  res <- catchToList({
    warning("careful")
    42
  })
  
  expect_equal(res$value, 42)
  expect_identical(res$warnings, "careful")
  expect_null(res$error)
})

test_that("catchToList captures multiple warnings in order", {
  res <- catchToList({
    warning("First warning")
    warning("Second warning")
    42
  })
  
  expect_equal(res$value, 42)
  expect_identical(res$warnings, c("First warning", "Second warning"))
  expect_null(res$error)
})

test_that("catchToList captures warnings raised by base functions", {
  res <- catchToList(log(-1))
  
  expect_true(is.nan(res$value))
  expect_length(res$warnings, 1)
  expect_match(res$warnings, "NaN")
  expect_null(res$error)
})

test_that("catchToList muffles warnings so they do not propagate", {
  expect_no_warning(
    catchToList({
      warning("muffled")
      1
    })
  )
})

test_that("catchToList captures an error and returns NULL value", {
  res <- catchToList(stop("Something went wrong"))
  
  expect_null(res$value)
  expect_identical(res$error, "Something went wrong")
  expect_null(res$warnings)
})

test_that("catchToList does not let errors propagate", {
  expect_no_error(catchToList(stop("boom")))
})

test_that("catchToList captures errors raised by base functions", {
  res <- catchToList(log("a"))

  # Get the message R produces in the current locale
  expected <- tryCatch(log("a"), error = function(e) conditionMessage(e))

  expect_null(res$value)
  expect_length(res$error, 1)
  expect_identical(res$error, expected)
})

test_that("catchToList keeps warnings emitted before an error", {
  res <- catchToList({
    warning("early warning")
    stop("fatal")
  })
  
  expect_null(res$value)
  expect_identical(res$warnings, "early warning")
  expect_identical(res$error, "fatal")
})

test_that("catchToList stops evaluating after an error", {
  side_effect <- FALSE
  
  catchToList({
    stop("stop here")
    side_effect <- TRUE
  })
  
  expect_false(side_effect)
})

test_that("catchToList captures custom condition messages", {
  res_w <- catchToList({
    warning(warningCondition("custom warn", class = "myWarning"))
    "ok"
  })
  res_e <- catchToList(stop(errorCondition("custom err", class = "myError")))
  
  expect_identical(res_w$warnings, "custom warn")
  expect_equal(res_w$value, "ok")
  expect_identical(res_e$error, "custom err")
  expect_null(res_e$value)
})

test_that("catchToList does not capture messages", {
  res <- NULL
  expect_message(
    res <- catchToList({
      message("hello")
      1
    }),
    "hello"
  )
  
  expect_equal(res$value, 1)
  expect_null(res$warnings)
  expect_null(res$error)
})

test_that("catchToList evaluates the expression lazily in the caller's environment", {
  z <- 10
  res <- catchToList(z + 1)
  
  expect_equal(res$value, 11)
})

test_that("catchToList calls do not share state between invocations", {
  first <- catchToList({
    warning("w1")
    stop("e1")
  })
  second <- catchToList(5)
  
  expect_identical(first$warnings, "w1")
  expect_identical(first$error, "e1")
  expect_equal(second$value, 5)
  expect_null(second$warnings)
  expect_null(second$error)
})

