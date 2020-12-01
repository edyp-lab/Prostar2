library(shiny)

#' modalButton
#' 
#' Creates a modal dialog box. \code{header} and \code{content} can 
#' include shiny inputs and outputs.
#' 
#' @param inputId       Specifies the input slot that will be used to access the value.
#' @param label         The contents of the button or link–usually a text label, but you could also use any other HTML, like an image.
#' @param icon          An optional icon to appear on the button.
#' @param header   HTML for the header
#' @param content  HTML for the content
#' 
#' @return html for modal button and window
#' @export
#' 
#' @example 
#' # In ui.R
#' modalButton("helpModal", "", icon = icon("info-circle"), 
#'   header = tags$h3("Help for my app"), 
#'   content = tags$p("Some detailed help"))
#' 
modalButton <- 
  function(inputId, label, icon = NULL, header = "", content = "")
  {
    # reference: http://getbootstrap.com/2.3.2/javascript.html#modals  
    
    # create the button  
    button <- tags$button(
      type = "button", 
      class = "btn btn-info", 
      `data-toggle` = "modal", 
      `data-target` = paste0("#", inputId, sep = ""), 
      list(icon, label)
    )
    
    # create the window 
    window <- tags$div(
      id = inputId, 
      class = "modal hide fade",
      tabindex = "-1",
      role = "dialog",
      `aria-labelledby` = paste0(inputId, "Label"),
      `aria-hidden` = "true",
      tags$div(
        class = "modal-header",
        tags$button(
          type = "button",
          class = "close",
          `data-dismiss` = "modal", 
          `aria-hidden` = "true",
          "x"
        ),
        tags$html(id = paste0(inputId, "Label"), header)
      ),
      tags$div(
        class = "modal-body",
        content
      ),
      tags$div(
        class = "modal-footer",
        tags$button(
          class = "btn",
          `data-dismiss` = "modal",
          `aria-hidden` = "true",
          "Close"
        )
      )
    )
    
    tags$html(button, window)
  }

  ui = fluidPage(
    fluidRow(
      column(
        width = 2,
        modalButton(
          "myModal",
          label = "click me",
          icon = icon("info-circle"),
          header = tags$h3("Help!"),
          content = tags$html(
            tags$p("I need somebody"),
            tags$h3("Help!"),
            tags$p("Not just anybody")
          )
        )
      )  
    )  
  )
  
  server = function(input, output, session){}



shinyApp(ui, server)