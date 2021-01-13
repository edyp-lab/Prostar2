# Module UI

#' @title   mod_open_demo_dataset_ui and mod_open_demo_dataset_server
#' 
#' @description  A shiny Module.
#'
#' @param id shiny id
#' 
#' @param input internal
#' 
#' @param output internal
#' 
#' @param session internal
#' 
#' @param pipeline.def xxx
#' 
#' @return An object of class [`xxxx`]
#' 
#' @rdname mod_open_demo_dataset
#'
#' @keywords internal
#' 
#' @export 
#' 
#' @importFrom shiny NS tagList 
#' @import shinyjs
#' 
mod_open_demo_dataset_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    mod_choose_pipeline_ui(ns("choosePipe")),
    div(
      div(
        style="display:inline-block; vertical-align: middle; padding-right: 20px;",
        uiOutput(ns("chooseDemoDataset")),
        uiOutput(ns("linktoDemoPdf"))
        )
      ),
   shinyjs::hidden(actionButton(ns("loadDemoDataset"), "Load demo dataset",class = actionBtnClass)),
   hr(),
   mod_infos_dataset_ui(ns("infos"))
  )
}

# Module Server

#' @rdname mod_open_demo_dataset
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @import DAPARdata2
#' @importFrom BiocGenerics get
#' @importFrom utils data
#' @importFrom BiocManager install
#' @importFrom shinyjs info
#' @import QFeatures
#' 
mod_open_demo_dataset_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
    rv.openDemo <- reactiveValues(
      dataRead = NULL,
      pipe = NULL,
      dataOut = NULL
    )
    
    rv.openDemo$pipe <- mod_choose_pipeline_server("choosePipe", 
                                                   dataType = 'Protein',
                                                   package = 'MSPipelines')
    
    
    observe({
      shinyjs::toggle('loadDemoDataset', condition= (!is.null(rv.openDemo$pipe())) && rv.openDemo$pipe() != '' && length(input$demoDataset) >0)
    })
    
    
    ### function for demo mode
    output$chooseDemoDataset <- renderUI({
      print("DAPARdata is loaded correctly")
      selectInput(ns("demoDataset"),
                  "Demo dataset",
                  choices = utils::data(package="DAPARdata2")$results[,"Item"],
                  selected = character(0),
                  width='200px')
    })
    
    
    
    observeEvent(input$loadDemoDataset, {
      nSteps <- 1
      withProgress(message = '',detail = '', value = 0, {
        incProgress(1/nSteps, detail = 'Loading dataset')
        utils::data(list=input$demoDataset, package='DAPARdata2')
        rv.openDemo$dataRead <- BiocGenerics::get(input$demoDataset)
        if (class(rv.openDemo$dataRead)!="QFeatures") {
          shinyjs::info("Warning : this file is not a QFeatures file ! 
                      Please choose another one.")
          return(NULL)
        }
        
        MultiAssayExperiment::metadata(rv.openDemo$dataRead)$pipelineType <- rv.openDemo$pipe()
        rv.openDemo$dataOut <- rv.openDemo$dataRead
        
      }) # End withProgress
      
      return(reactive({rv.openDemo$dataOut }))
    }) # End observeEvent
    
    
    output$linktoDemoPdf <- renderUI({
      req(input$demoDataset)
      
      # file<- paste(system.file(package = "DAPARdata2"),"/doc/",
      #              input$demoDataset,".pdf", sep="")
      # cmd <- paste("cp ",file," www", sep="")
      # system(cmd)
      # filename <-paste0(input$demoDataset,".pdf", sep="")
      # p("Dataset documentation ",a(href=filename, target='_blank', "(pdf)"))
      # 
    })
    
    mod_infos_dataset_server('infos', 
                             obj = reactive({rv.openDemo$dataOut})
    )
                             
    return(reactive({rv.openDemo$dataOut}))
    
    
  })
  
}

## To be copied in the UI
# mod_open_demo_dataset_ui("open_demo_dataset_ui_1")

## To be copied in the server
# callModule(mod_open_demo_dataset_server, "open_demo_dataset_ui_1")

