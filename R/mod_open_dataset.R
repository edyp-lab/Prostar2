# Module UI
  
#' @title   mod_open_dataset_ui and mod_open_dataset_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param pipeline.def xxx
#' 
#' @return An object of class [`xxxx`]
#' @rdname mod_open_dataset
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_open_dataset_ui <- function(id){
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Open file", multiple = FALSE),
    mod_choose_pipeline_ui(ns("choosePipe")),
    actionButton(ns("loadDataset"), "Load dataset",class = actionBtnClass),
    br(),
    p("Once the 'Load' button (above) clicked, all importing functions ('Open file', 'Demo data' and 'Convert data') will be disabled 
      (because successive dataset loading can make Prostar unstable). To work on another dataset, use first the 'Reload Prostar' functionality from 
      the 'Dataset manager' menu: it will make Prostar restart with a fresh R session where import functions are enabled."),
    
    hr(),
    
    mod_infos_dataset_ui(ns("infos"))
  )
}
    
# Module Server
    
#' @rdname mod_open_dataset
#' @export
#' @keywords internal
#' @importFrom DAPAR PipelineProtein PipelinePeptide typeOfData
#'     
mod_open_dataset_server <- function(input, output, session,pipeline.def){
  ns <- session$ns
  
  
  rv.openDataset <- reactiveValues(
    dataOut = NULL,
    pipe = NULL
  )
  
  
  rv.openDataset$pipe <- callModule(mod_choose_pipeline_server, "choosePipe", pipeline.def=reactive({pipeline.def()}))
  
  
  callModule(mod_infos_dataset_server, 
             'infos', 
             obj = reactive({
               req(rv.openDataset$dataOut)
               rv.openDataset$dataOut
             })
  )
  
  
  DeleteExtension <- function(name){
    return(strsplit(name,'.', fixed=T)[[1]][1])
  }
  
  observeEvent( input$loadDataset,ignoreInit =TRUE,{ 
    req(input$file)
    data <- readRDS(input$file$datapath)
    
    withProgress(message = '',detail = '', value = 0, {
      incProgress(1, detail = 'Loading dataset')
      switch(class(data)[1],
             MultiAssayExperiment= {rv.openDataset$dataOut <- data},
             MSnSet= {
               typeOfData <- DAPAR::typeOfData(data)
               ll.pipeline <- rv.openDataset$pipe()
               
               switch(typeOfData,
                      peptide = {rv.openDataset$dataOut <- PipelinePeptide(analysis= DeleteExtension(input$file$name), 
                                                                       pipelineType = names(ll.pipeline), 
                                                                       dataType ='peptide',
                                                                       processes=unlist(ll.pipeline),
                                                                       experiments=list(original=data), 
                                                                       colData=Biobase::pData(data))
                      },
                      protein = {rv.openDataset$dataOut <- PipelineProtein(analysis= DeleteExtension(input$file$name), 
                                                                       pipelineType = names(ll.pipeline), 
                                                                       dataType ='protein',
                                                                       processes=unlist(ll.pipeline), 
                                                                       experiments=list(original=data), 
                                                                       colData=Biobase::pData(data)
                      )
                      }, 
                      p2p = {rv.openDataset$dataOut <- NULL}
               )
             },
             default= {
               shinyjs::info("Warning : this file is not a MSnset file ! 
                                                Please choose another one.")
               return(NULL)
             }
      ) # end of switch statement
      
    })
    
  })
  
  
  return(reactive({rv.openDataset$dataOut }))
}
    
## To be copied in the UI
# mod_open_dataset_ui("open_dataset_ui_1")
    
## To be copied in the server
# callModule(mod_open_dataset_server, "open_dataset_ui_1")
 
