library(shiny)

ui <- function(request) {
  
  fluidPage(
    textInput("txt", "Enter text"),
    checkboxInput("caps", "Capitalize"),
    verbatimTextOutput("out"),
    bookmarkButton()
  )
  
}

server <- function(input, output, session) {
  output$out <- renderText({
    if (input$caps)
      toupper(input$txt)
    else
      input$txt
  })
  
  # Define default path (to be changed later)
  switch(Sys.info()[['sysname']],
         Windows = {path <<- gsub("\\\\", "/", file.path(Sys.getenv("USERPROFILE"),"Desktop",fsep="/"))},
         Mac = {path <<- "~/Desktop"})
  
  onBookmark(function(state) {
    
    # Change working directory to target directory for bookmarks:
    id = gsub(sprintf("%s/shiny_bookmarks/",getwd()), "", state$dir) # get unique id
    state$dir = sprintf("%s/shiny_bookmarks/%s", path, id)
    
  })
  
}

enableBookmarking(store="server")
shinyApp(ui, server)