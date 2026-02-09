#' @title Change the default functions in `MagellanNTK`
#'
#' @description This module allows to change
#'
#' @param id xxx
#' @param class xxx
#' @param extension xxx
#' @param demo_package xxx
#' @param remoteReset xxx
#' @param is.enabled A boolean
#'
#' @name generic_mod_open_dataset
#'
#' @examples
#' if (interactive()){
#' shiny::runApp(open_dataset())
#' }
#'
#' @return NA
#'
NULL




#' @export
#' @rdname generic_mod_open_dataset
#' @importFrom shiny NS tagList
#' @importFrom shinyjs useShinyjs hidden toggle toggleState info hide show 
#' disabled inlineCSS extendShinyjs
#'
open_dataset_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns('chooseSource_UI')),
    uiOutput(ns("customDataset_UI")),
    uiOutput(ns("packageDataset_UI")),
    uiOutput(ns('Description_infos_dataset_UI'))
  )
}


#' @rdname generic_mod_open_dataset
#'
#' @export
#' @importFrom BiocGenerics get
#' @importFrom utils data
#' @importFrom shinyjs useShinyjs hidden toggle toggleState info hide show 
#' disabled inlineCSS extendShinyjs
#' @import shiny
#'
open_dataset_server <- function(
    id,
  class = NULL,
  extension = NULL,
  demo_package = NULL,
  remoteReset = reactive({NULL}),
  is.enabled = reactive({TRUE})) {
  
  widgets.default.values <- list(
    chooseSource = "customDataset",
    file = character(0),
    load_dataset_btn = 0,
    pkg = NULL,
    demoDataset = "None"
  )
  
  rv.custom.default.values <- list(
    remoteReset = NULL,
    dataRead = NULL,
    name = "default.name",
    packages = NULL,
    error_msg = ""
  )
  
  dataOut <- reactiveValues(
    trigger = MagellanNTK::Timestamp(),
    name = NULL,
    dataset = NULL
  )
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    core <- paste0(
      MagellanNTK::Get_Code_Declare_widgets(names(widgets.default.values)),
      MagellanNTK::Get_Code_for_ObserveEvent_widgets(names(widgets.default.values)),
      MagellanNTK::Get_Code_for_rv_reactiveValues(),
      MagellanNTK::Get_Code_Declare_rv_custom(names(rv.custom.default.values)),
      #MagellanNTK::Get_Code_for_dataOut(),
      #MagellanNTK::Get_Code_for_remoteReset(widgets = TRUE, custom = TRUE, dataIn = 'NULL'),
      sep = "\n"
    )
    eval(str2expression(core))
    
    
    observeEvent(remoteReset(), ignoreInit = TRUE, ignoreNULL = TRUE, {
      lapply(names(rv.widgets), function(x){
        rv.widgets[[x]] <- widgets.default.values[[x]]
      })
      
      rv.custom$dataRead <- NULL
      rv.custom$remoteReset <- remoteReset()
      # rv.widgets$file <- NULL
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$name <- NULL
      dataOut$dataset <- NULL
      
    })
    
    
    
    output$chooseSource_UI <- renderUI({
      widget <- selectInput(ns("chooseSource"), "Dataset source",
        choices = c(
          "Custom dataset" = "customDataset",
          "package dataset" = "packageDataset"
        ),
        selected = rv.widgets$chooseSource,
        width = "200px"
      )
      
      MagellanNTK::toggleWidget(widget, is.enabled())
      
    })
    
    
    
    output$customDataset_UI <- renderUI({
      req(rv.widgets$chooseSource == "customDataset")
      rv.custom$remoteReset
      
      widget <- fileInput(ns("file"), "Open file",
        accept = extension, 
        multiple = FALSE, 
        width = "400px"
      )
      
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    
    
    isEmptyNumeric <- function(x) {
      return(identical(x, numeric(0)))
    }
    
    output$packageDataset_UI <- renderUI({
      req(rv.widgets$chooseSource == "packageDataset")
      tagList(
        uiOutput(ns("choosePkg")),
        uiOutput(ns("chooseDemoDataset")),
        #uiOutput(ns("linktoDemoPdf"))
        uiOutput(ns('error_msg_ui')),
        uiOutput(ns("load_btn_UI"))
      )
    })
    
    output$error_msg_ui <- renderUI({
      req(rv.custom$error_msg)
      
      tags$p(style = "color: red;", rv.custom$error_msg)
    })

    output$load_btn_UI <- renderUI({
      req(rv.widgets$demoDataset)
      req(rv.widgets$pkg)
      
        widget <- actionButton(ns("load_dataset_btn"), "Load file")
        MagellanNTK::toggleWidget(widget, is.enabled())
    })
    
    
    output$choosePkg <- renderUI({
      req(rv.widgets$chooseSource == "packageDataset")
      
      shiny::withProgress(message = "", detail = "", value = 0.5, {
        shiny::incProgress(0.5, detail = paste0("Searching for ", class, " datasets"))
        rv.custom$packages <- MagellanNTK::GetListDatasets(class, demo_package)
      })
      
      
      req(rv.custom$packages)
      
      widget <- shiny::selectizeInput(ns("pkg"), "Choose package",
        choices = rv.custom$packages[, "Package"],
        width = "200px"
      )
      
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    
    
    ## function for demo mode
    output$chooseDemoDataset <- renderUI({
      req(rv.widgets$chooseSource == "packageDataset")
      req(rv.widgets$pkg)
      pkgs.require(rv.widgets$pkg)
      
      req(rv.custom$packages)
      
      ind <- which(rv.custom$packages[, "Package"] == rv.widgets$pkg)
      
      widget <- selectInput(ns("demoDataset"),
        "Demo dataset",
        choices = rv.custom$packages[ind, "Item"],
        selected = character(0),
        width = "200px"
      )
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    
    # observe({
    #   req(rv.widgets$chooseSource)
    #   req(rv.widgets$pkg)
    #   req(rv.widgets$file)
    #   
    #   cond1 <- rv.widgets$chooseSource == "customDataset" && !is.null(rv.widgets$file$datapath)
    #   cond2 <- rv.widgets$chooseSource == "packageDataset" && !is.null(rv.widgets$pkg) && rv.widgets$pkg != ""
    #   
    #   shinyjs::toggleState("load_dataset_btn", condition = cond1 || cond2)
    # })
    
    
    # output$linktoDemoPdf <- renderUI({
    #   req(rv.widgets$demoDataset)
    #   req(rv.widgets$chooseSource == "packageDataset")
    # })

    
    observeEvent(input$file,{
      rv.widgets$file <- input$file
      rv.custom$dataRead <- NULL
      tryCatch({
        # Try with readRDS()
        rv.custom$name <- rv.widgets$file$name
        rv.custom$dataRead <- readRDS(rv.widgets$file$datapath)
      },
        warning = function(w) {return(NULL)},
        error = function(e) {return(NULL)}
      )
      
      if (is.null(rv.custom$dataRead)) {
        rv.custom$dataRead <- tryCatch(
          {
            load(file = rv.widgets$file$datapath)
            rv.custom$name <- unlist(strsplit(rv.widgets$file$name, split = ".", fixed = TRUE))[1]
            get(rv.custom$name)
          },
          warning = function(w) {return(NULL)},
          error = function(e) {return(NULL)}
        )
      }

      
      dataOut$dataset <- rv.custom$dataRead
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$name <- rv.custom$name
    })
    
    
    
    # Part of open custom dataset
    ## -- Open a  File --------------------------------------------
    observeEvent(rv.widgets$load_dataset_btn, ignoreInit = FALSE, {
      req(rv.widgets$demoDataset != "None")
      
      rv.custom$error_msg <- ""
      shinyjs::toggleState("load_dataset_btn")
      utils::data(list = rv.widgets$demoDataset, package = rv.widgets$pkg)
      rv.custom$name <- rv.widgets$demoDataset
      shiny::withProgress(message = paste0("Getting dataset ", rv.widgets$demoDataset), {
        shiny::incProgress(0.5)
        print(paste0(rv.widgets$demoDataset, ' : shiny::withProgress(message = paste0("Builds Excel file", rv.widgets$demoDataset)'))
        tryCatch({
          rv.custom$dataRead <- DaparToolshedData::GetData(rv.widgets$demoDataset)
          #rv.custom$remoteReset <- rv.custom$remoteReset + 1
          
          dataOut$dataset <- rv.custom$dataRead
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$name <- rv.custom$name
        }, 
        error = function(e) {message("Error: Unable to download the requested dataset. Please check your internet connection and try again.")
          rv.custom$error_msg <- "Error: Unable to download the requested dataset. Please check your internet connection and try again."}
        )
      })
      shinyjs::toggleState("load_dataset_btn")
    })
    
    output$Description_infos_dataset_UI <- renderUI({
      
      req(rv.custom$dataRead)
      
      infos_dataset_server(
        id = "Description_infosdataset",
        dataIn = reactive({rv.custom$dataRead})
      )
      
      infos_dataset_ui(id = ns("Description_infosdataset"))
    })
    
    return(reactive({dataOut}))
    
  })
}





#' @export
#' @rdname generic_mod_open_dataset
open_dataset <- function() {
  ui <- fluidPage(
    tagList(
      open_dataset_ui("qf_file"),
      textOutput("res")
    )
  )
  
  server <- function(input, output, session) {
    rv <- reactiveValues(
      obj = NULL,
      result = NULL
    )
    
    
    rv$result <- open_dataset_server("qf_file")
    
    observeEvent(req(rv$result()), {
      rv$obj <- rv$result()
    })
    
    
    output$res <- renderText({
      rv$obj
      print(rv$obj)
      paste0("Names of the datasets: ", names(rv$obj))
    })
  }
  
  app <- shinyApp(ui, server)
}