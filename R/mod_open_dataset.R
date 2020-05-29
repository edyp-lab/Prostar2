# Module UI
  
#' @title   mod_open_dataset_ui and mod_open_dataset_server
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
#' @rdname mod_open_dataset
#'
#' @keywords internal
#' 
#' @export 
#' 
#' @importFrom shiny NS tagList 
#' @importFrom shinyjs useShinyjs
#' 
mod_open_dataset_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fileInput(ns("file"), "Open file", multiple = FALSE),
    mod_choose_pipeline_ui(ns("choosePipe")),
    uiOutput(ns('ui_select_KID')) ,
    actionButton(ns("loadDataset"), "Load dataset",class = actionBtnClass),
    br(),
    p("Once the 'Load' button (above) clicked, all importing functions ('Open file', 'Demo data' and 'Convert data') will be disabled 
      (because successive dataset loading can make Prostar unstable). To work on another dataset, use first the 'Reload Prostar' functionality from 
      the 'Dataset manager' menu: it will make Prostar restart with a fresh R session where import functions are enabled.")
  )
}
    
# Module Server
    
#' @rdname mod_open_dataset
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @importFrom DAPAR PipelineProtein PipelinePeptide typeOfData
#' @importFrom shinyjs info alert
#'     
mod_open_dataset_server <- function(input, output, session, pipeline.def){
  ns <- session$ns
  
  
  rv.openDataset <- reactiveValues(
    dataOut = NULL,
    pipe = NULL,
    dataRead = NULL,
    ret = NULL,
    keyID = NULL,
    parentProtId = NULL
  )
  
  
  rv.openDataset$pipe <- callModule(mod_choose_pipeline_server, "choosePipe", pipeline.def=reactive({pipeline.def()}))

  
  DeleteExtension <- function(name){
    return(strsplit(name,'.', fixed=T)[[1]][1])
  }
  
  
  
  
  observeEvent(req(input$file),{
    
    tryCatch({
      rv.openDataset$dataRead <- readRDS(input$file$datapath)
      
    }, warning = function(w) {
      shinyjs::alert('Input format not recognized.')
      warning(w)
      return(NULL)
    }, error = function(e) {
      shinyjs::info('Input format not recognized.')
      return(NULL)
    }, finally = {
      #cleanup-code 
    })
    
    
  })
  
  
  
  
  output$ui_select_KID <- renderUI({
    req(rv.openDataset$dataRead )
    
    if (!(class(rv.openDataset$dataRead )[1] == "MSnSet")){ return(NULL)}
    
    mod_select_keyID_ui(ns('select_KID'))
  })
  
  
  
  rv.openDataset$ret <- callModule(mod_select_keyID_server, 
                                   'selectKID', 
                                   dataIn=reactive({Biobase::fData(rv.openDataset$dataRead)}), 
                                   typeOfData = reactive({rv.openDataset$dataRead@experimentData@other$typeOfData}))
  
  observe({
    rv.openDataset$keyID <- rv.openDataset$ret()$keyId
    rv.openDataset$parentProtId <- rv.openDataset$ret()$parentProtId
    rv.openDataset$data <- rv.openDataset$ret()$data
  })
  
  observeEvent(input$loadDataset,ignoreInit =TRUE,{ 
    req(rv.openDataset$dataRead )
    

      withProgress(message = '',detail = '', value = 0, {
      incProgress(1, detail = 'Loading dataset')
      switch(class(rv.openDataset$dataRead )[1],
             Features= {rv.openDataset$dataOut <- rv.openDataset$dataRead },
             MSnSet= {
               typeOfData <- rv.openDataset$dataRead@experimentData@other$typeOfData
               ll.pipeline <- rv.openDataset$pipe()
               
               rv.openDataset$dataOut <- convertMSnset2Features(obj = data,
                                                                 analysis = DeleteExtension(input$file$name),
                                                                 parentProtId = NULL,
                                                                 keyId = NULL,
                                                                 pipelineType = names(ll.pipeline), 
                                                                 processes = unlist(ll.pipeline)
                                                                )
                      },
             default= {
               shinyjs::info("Warning : This type of data is not implemented in Prostar.")
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
 
