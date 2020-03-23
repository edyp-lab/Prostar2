# Module UI
  
#' @title   mod_open_demo_dataset_ui and mod_open_demo_dataset_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param pipeline.def xxx
#' 
#' @return An object of class [`xxxx`]
#' 
#' @rdname mod_open_demo_dataset
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_open_demo_dataset_ui <- function(id){
  ns <- NS(id)
  tagList(
   # br(),br(),br(),
    tags$div(
      tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                uiOutput(ns("chooseDemoDataset"))
      ),
      tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                p(""),
                actionButton(ns("loadDemoDataset"), "Load demo dataset",class = actionBtnClass)
      ),
      tags$div( style="display:inline-block; vertical-align: middle;",
                p(""),
                uiOutput(ns("linktoDemoPdf"))
      )
    ),
    mod_choose_pipeline_ui(ns("choosePipe")),
    hr(),
    
    mod_infos_dataset_ui(ns("infos"))
  )
}
    
# Module Server
    
#' @rdname mod_open_demo_dataset
#' @export
#' @keywords internal
#' @importFrom DAPAR PipelineProtein PipelinePeptide
#' @import DAPARdata
#' @importFrom utils data
#' @importFrom BiocManager install
#' @importFrom shinyjs info
#' @importFrom Biobase pData

mod_open_demo_dataset_server <- function(input, output, session, pipeline.def){
  ns <- session$ns
  
  rv.openDemo <- reactiveValues(
    dataOut = NULL,
    pipe = NULL
  )
  
  
  rv.openDemo$pipe <- callModule(mod_choose_pipeline_server, "choosePipe", pipeline.def = reactive({pipeline.def()}))
  # mod_infos_dataset prend un objet mae
  callModule(mod_infos_dataset_server, 
             'infos', 
             obj = reactive({
               req(rv.openDemo$dataOut)
               rv.openDemo$dataOut
             })
  )
  
  
  ### function for demo mode
  output$chooseDemoDataset <- renderUI({
    if(require("DAPARdata", lib.loc=DAPARdata.loc)){
      print("DAPARdata is loaded correctly")
      selectInput(ns("demoDataset"),
                  "Demo dataset",
                  choices = utils::data(package="DAPARdata")$results[,"Item"],
                  width='200px')
    } else {
      print("Trying to install DAPARdata")
      BiocManager::install("DAPARdata")
      if(require(DAPARdata)){
        print("DAPARdata installed and loaded")
        selectInput(ns("demoDataset"),
                    "Demo dataset",
                    choices = utils::data(package='DAPARdata')$results[,"Item"],
                    width='200px'   )
      } else {
        stop("Could not install the package DAPARdata")
      }
    }
    
  })
  
  
  observeEvent(req(input$loadDemoDataset), {
    
    
    nSteps <- 1
    withProgress(message = '',detail = '', value = 0, {
      incProgress(1/nSteps, detail = 'Loading dataset')
      utils::data(list=input$demoDataset)
      data <- get(input$demoDataset)
      if (class(data)[1]!="MSnSet") {
        shinyjs::info("Warning : this file is not a MSnSet file ! 
                      Please choose another one.")
        return(NULL)
      }
      proteinID <- data@experimentData@other$proteinId
      typeOfData <- data@experimentData@other$typeOfData
      ll.pipeline <- rv.openDemo$pipe()
      print("ll.pipeline")
      print(ll.pipeline)
      print("typeOfData")
      print(typeOfData)
      switch(typeOfData,
             protein = {
               rv.openDemo$dataOut <- PipelineProtein(analysis= input$demoDataset, 
                                                      pipelineType = names(ll.pipeline), 
                                                      dataType ='protein',
                                                      processes=unlist(ll.pipeline), 
                                                      experiments=list(original=data), 
                                                      colData=Biobase::pData(data))
             },
             peptide = {
               rv.openDemo$dataOut <- PipelinePeptide(analysis= input$demoDataset, 
                                                      pipelineType = names(ll.pipeline), 
                                                      dataType ='peptide',
                                                      processes=unlist(ll.pipeline),
                                                      proteinID = proteinID,
                                                      experiments=list(original=data), 
                                                      colData=Biobase::pData(data))
             }
      ) # end swith
      
    }) # End withProgress
    
  }) # End observeEvent
  
  
  output$linktoDemoPdf <- renderUI({
    req(input$demoDataset)
    
    file<- paste(system.file(package = "DAPARdata"),"/doc/",
                 input$demoDataset,".pdf", sep="")
    cmd <- paste("cp ",file," www/.", sep="")
    system(cmd)
    filename <-paste0(input$demoDataset,".pdf", sep="")
    p("Dataset documentation ",a(href=filename, target='_blank', "(pdf)"))
    
  })
  
 
  return(reactive({rv.openDemo$dataOut}))
}
    
## To be copied in the UI
# mod_open_demo_dataset_ui("open_demo_dataset_ui_1")
    
## To be copied in the server
# callModule(mod_open_demo_dataset_server, "open_demo_dataset_ui_1")
 
