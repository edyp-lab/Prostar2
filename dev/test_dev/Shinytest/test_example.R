app <- shinytest::ShinyDriver$new('.')
app$setInputs(name = "Hadley")
app$getValue("greeting")

app$click("reset")
app$getValue("greeting")
