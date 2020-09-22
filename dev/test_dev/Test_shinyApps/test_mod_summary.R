library(shiny)

x <- reactiveVal(1:10)
testServer(summaryServer, args = list(var = x), {
  print(range_val())
  print(output$min)
})




test_that("output updates when reactive input changes", {
  x <- reactiveVal()
  testServer(summaryServer, args = list(var = x), {
    x(1:10)
    expect_equal(range_val(), c(1, 10))
    expect_equal(output$mean, "5.5")
    
    x(10:20)
    expect_equal(range_val(), c(10, 20))
    # expect_equal(output$min, "10")
  }) 
})