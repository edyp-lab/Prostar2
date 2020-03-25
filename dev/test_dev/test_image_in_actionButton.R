## exemple de code pour creer des vignettes sur lequelles on peut cliquer pour generer un evenement
## utile pour le module des plots
library(shiny)
library(shinyBS)
library(shinyjs)

library(shinyjqui)


mod_A_ui <- function(id){
  ns <- NS(id)
  tagList (
    actionButton(ns("openModal"), "Open modal"),
   # tags$head(tags$style(".modal-dialog { width:75% }")),
    
    
    shinyBS::bsModal("modal_test",
                     "Descriptive statistics",
                     trigger = ns("openModal"),
                     size = "large",
                     #uiOutput(ns('toto'))
                     "bar"
    )
  )
  
}


mod_A_server <- function(input, output, session){
  ns <- session$ns
  # 
  # 
  # output$toto <- renderUI({
  #   tagList(
  #     shinyjs::useShinyjs(),
  #     
  #     
  #     fluidPage(
  #       tags$style(".topimg {
  #                           margin-left:-25px;
  #                           margin-right:-20px;
  #                           margin-top:-20px;
  #                           margin-bottom:-20px;
  #                           padding: 10px;
  #                         }"),
  #       div( style="display:inline-block; vertical-align: middle; padding: 7px",
  #            tags$button(
  #              id = ns("btn1"),
  #              info = 'test',
  #              class = "btn action-button",
  #              div(class="topimg",imageOutput(ns('image1'), height=30, width=30))
  #            )
  #       ),
  #       div( style="display:inline-block; vertical-align: middle; padding: 7px",
  #            tags$button(
  #              id = ns("btn2"),
  #              class = "btn action-button",
  #              div(class="topimg",imageOutput(ns('image2'), height=30, width=30))
  #            )),
  #       div(style="display:inline-block; vertical-align: middle; padding: 7px",
  #           tags$button(
  #             id = ns("btn3"),
  #             class = "btn action-button",
  #             div(class="topimg",imageOutput(ns('image3'), height=30, width=30))
  #           ) )
  #     ),
  #     
  #     #
  #     br(),br(),br(),
  #     shinyjs::hidden(uiOutput(ns('image1_large'))),
  #     shinyjs::hidden(uiOutput(ns('image2_large'))),
  #     shinyjs::hidden(uiOutput(ns('image3_large')))
  #     
  #   )
  # })
  # output$image1 <- renderImage({
  #   filename <- normalizePath(file.path('../../R/Drafts/images','desc_quantiData.png'))
  #   list(src = filename,
  #        width = 40,
  #        height = 40)
  # }, deleteFile = FALSE)
  # 
  # output$image1_large <- renderUI({p('plot 1') })
  # output$image2_large <- renderUI({h3('plot 2') })
  # output$image3_large <- renderUI({h4('plot 3') })
  # 
  # 
  # observeEvent(input$btn1,{
  #   print('click on btn 1')
  #   shinyjs::show('image1_large')
  #   shinyjs::hide('image2_large')
  #   shinyjs::hide('image3_large')
  # })
  # 
  # 
  # observeEvent(input$btn2,{
  #   print('click on btn 2')
  #   shinyjs::hide('image1_large')
  #   shinyjs::show('image2_large')
  #   shinyjs::hide('image3_large')
  # })
  # 
  # observeEvent(input$btn3,{
  #   print('click on btn 3')
  #   shinyjs::hide('image1_large')
  #   shinyjs::hide('image2_large')
  #   shinyjs::show('image3_large')
  # })
  # 
  # 
  # output$image2 <- renderImage({
  #   filename <- normalizePath(file.path('../../R/Drafts/images','desc_corrmatrix.png'))
  #   list(src = filename,
  #        width = 40,
  #        height = 40)
  # }, deleteFile = FALSE)
  # 
  # output$image3 <- renderImage({
  #   filename <- normalizePath(file.path('../../R/Drafts/images','desc_heatmap.png'))
  #   list(src = filename,
  #        width = 40,
  #        height = 40)
  # }, deleteFile = FALSE)
  # 
}


shinyApp(
  ui = shinyUI(
    mod_A_ui('test')
  ),
  server = function(input, output, session){

    callModule(mod_A_server, 'test')

  }
)