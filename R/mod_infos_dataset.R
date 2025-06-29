#' @title   infos_dataset_ui and infos_dataset_server
#' @description  A shiny Module.
#' 
#' @param id shiny id
#' @param dataIn An instance of the class `QFeatures`.
#' 
#' @return A shiny app
#'
#' 
#' @name infos_dataset
#' 
#' @examples
#' \dontrun{
#' data(Exp1_R25_prot, package = 'DaparToolshedData')
#' shiny::runApp(infos_dataset(Exp1_R25_prot))
#' }

NULL



#'
#'
#' @rdname infos_dataset
#'
#' @export 
#' @importFrom shiny NS tagList 
#' @import QFeatures
#' @importFrom MagellanNTK format_DT_ui format_DT_server
#' 
infos_dataset_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('title')),
    
    fluidRow(
      column(width=6,
             MagellanNTK::format_DT_ui(ns('dt')),
             br(),
             uiOutput(ns('samples_tab_ui'))
      ),
       column(width=6,
              uiOutput(ns('choose_SE_ui')),
              uiOutput(ns('show_SE_ui'))
       )
    )
  )
}





# Module Server

#' @rdname infos_dataset
#' @export
#' 
#' @keywords internal
#' 
#' @importFrom tibble as_tibble
#' 
infos_dataset_server <- function(id,
  dataIn = reactive({NULL}),
  remoteReset = reactive({0}),
  is.enabled = reactive({TRUE})
  ){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
    rv <- reactiveValues(
      dataIn = NULL
    )
    observeEvent(req(inherits(dataIn(),'QFeatures')), {
       rv$dataIn <- dataIn()
    })
      
      
      
      output$samples_tab_ui <- renderUI({
        req(rv$dataIn)
        
        
        MagellanNTK::format_DT_server('samples_tab',
          dataIn = reactive({
            req((rv$dataIn))
            
            data.frame(colData(rv$dataIn))
          }),
          max.rows = nrow(colData(rv$dataIn)),
          hc_style = reactive({
            list(
              cols = colnames(colData(rv$dataIn)),
              vals = colnames(colData(rv$dataIn))[2],
              unique = unique(colData(rv$dataIn)$Condition),
              pal = RColorBrewer::brewer.pal(3,'Dark2')[seq(length(unique(colData(rv$dataIn)$Condition)))])
          })
        )
        
        tagList(
          h4("Samples"),
          MagellanNTK::format_DT_ui(ns('samples_tab'))
        )
        
      })
      
      



MagellanNTK::format_DT_server('dt',
  dataIn = reactive({
        req(Get_QFeatures_summary())
        tibble::as_tibble(Get_QFeatures_summary())
        }))



#observe({browser()})


    output$title <- renderUI({
      req(rv$dataIn)
      name <- metadata(rv$dataIn)$analysis
    
      tagList(
          h3("Dataset summary"),
        p(paste0("Name of analysis:", name$analysis)),
        p(paste0("Original file:", metadata(rv$dataIn)$file))
        )
    })



    output$choose_SE_ui <- renderUI({
      req(rv$dataIn)
      selectInput(ns("selectInputSE"),
        "Select a dataset for further information",
        choices = c("None", names(experiments(rv$dataIn)))
      )
    })

    
    Get_QFeatures_summary <- reactive({

      req(rv$dataIn)
      nb_assay <- length(rv$dataIn)
      names_assay <- unlist(names(rv$dataIn))
      pipeline <- metadata(rv$dataIn)$pipelineType

      columns <- c("Pipeline Type",
        "Number of assay(s)",
                   "List of assay(s)"
                   )

      vals <- c( if(is.null(metadata(rv$dataIn)$pipelineType)) '-' else metadata(rv$dataIn)$pipelineType,
                 length(rv$dataIn),
                 if (length(rv$dataIn)==0) '-' 
        else HTML(paste0('<ul>', paste0('<li>', names_assay, "</li>", collapse=""), '</ul>', collapse=""))
      )



      do <- data.frame(Definition = columns,
                       Value = vals
      )

      do
    })
    
    
    
    
    Get_SE_Summary <- reactive({
      req(rv$dataIn)
      req(input$selectInputSE != "None")


        .se <- rv$dataIn[[input$selectInputSE]]
        
        typeOfData <- typeDataset(.se)
        nLines <- nrow(.se)
        .nNA <- QFeatures::nNA(.se)
        percentMV <- round(100*.nNA$nNA[,'pNA'], digits = 2)
        nEmptyLines <-  length(which(.nNA$nNArows[,'nNA']==ncol(.se)))

        val <- c(typeOfData, nLines, percentMV, nEmptyLines)
        row_names <- c("Type of data",
                       "Number of lines",
                       "% of missing values",
                       "Number of empty lines")

        if (tolower(typeOfData) == 'peptide'){

          if(length(metadata(.se)$list.matAdj) > 0){
            adjMat.txt <- "<span style=\"color: lime\">OK</span>"
          } else{
            adjMat.txt <- "<span style=\"color: red\">Missing</span>"
          }

          if(!is.null(metadata(.se)$list.cc)){
            cc.txt <- "<span style=\"color: lime\">OK</span>"
          } else{
            cc.txt <- "<span style=\"color: red\">Missing</span>"
          }

          val <- c(val, adjMat.txt, cc.txt)
          row_names <- c(row_names, "Adjacency matrices", "Connex components")
        }


        do <- data.frame(Definition = row_names,
                         Value = val,
                         row.names = row_names)
        do
    })
    
    
    
    
    Get_SE_History <- reactive({
      req(rv$dataIn)
      req(input$selectInputSE != "None")
      input$selectInputSE
      
      
      .se <- rv$dataIn[[input$selectInputSE]]
      #.name <- names(rv$dataIn)
      se_history <- '-'
  
      if (!is.null(paramshistory(.se))){
        se_history <- lapply(paramshistory(.se), function(x) 
          ConvertListToHtml(paste0(names(x), ' = ', x))
          )
        
        #se_history_values <- lapply(paramshistory(.se), function(x) x)

      data.frame(
        Name = names(se_history), 
        History = unlist(se_history)
        )
      } else {
        data.frame(Name = '-', 
          History = '-')
      }
      
    })
    
    
    
    output$properties_ui <- renderUI({
      req(input$selectInputSE)
      req(rv$dataIn)

      if (input$selectInputSE != "None") {
        checkboxInput(ns('properties_button'), "Display details?", value = FALSE)
      }
    })
    
    
    
    observeEvent(input$selectInputSE,{

      if (isTRUE(input$properties_button)) {
        output$properties_ui <- renderUI({
          checkboxInput(ns('properties_button'), "Display details?", value = TRUE)
        })
      }
      else{ return(NULL)}
    })
    
    
    # output$properties <- renderPrint({
    #   req(input$properties_button)
    # 
    #   if (input$selectInputSE != "None" && isTRUE(input$properties_button)) {
    # 
    #     data <- experiments(obj())[[input$selectInputSE]]
    #     metadata(data)
    #   }
    # })
    
    MagellanNTK::format_DT_server('dt2',
      dataIn = reactive({Get_SE_Summary()})
    )
    
    MagellanNTK::format_DT_server('history',
      dataIn = reactive({Get_SE_History()})
    )
    
    output$show_SE_ui <- renderUI({
      req(input$selectInputSE != "None")
      req(rv$dataIn)

        tagList(
          MagellanNTK::format_DT_ui(ns('dt2')),
          br(),
          #uiOutput(ns('info'))
          MagellanNTK::format_DT_ui(ns('history'))
        )
    })
    
    
    
    # output$info <- renderUI({
    #   req(input$selectInputSE)
    #   req(rv$dataIn)
    # 
    #   if (input$selectInputSE != "None") {
    # 
    #     typeOfDataset <- Get_SE_Summary()["Type of data", 2]
    #     pourcentage <- Get_SE_Summary()["% of missing values", 2]
    #     nb.empty.lines <- Get_SE_Summary()["Number of empty lines", 2]
    #     if (pourcentage > 0 && nb.empty.lines > 0) {
    #       tagList(
    #         tags$h4("Info"),
    #         if (typeOfDataset == "protein"){
    #           tags$p("The aggregation tool
    #              has been disabled because the dataset contains
    #              protein quantitative data.")
    #         },
    # 
    #         if (pourcentage > 0){
    #           tags$p("As your dataset contains missing values, you should
    #              impute them prior to proceed to the differential analysis.")
    #         },
    #         if (nb.empty.lines > 0){
    #           tags$p("As your dataset contains lines with no values, you
    #              should remove them with the filter tool
    #              prior to proceed to the analysis of the data.")
    #         }
    #       )
    #     }
    #   }
    # })
    
    
    
    
    # NeedsUpdate <- reactive({
    #   req(obj())
    #   PROSTAR.version <- metadata(experiments(obj()))$versions$Prostar_Version
    #   
    #   if(compareVersion(PROSTAR.version,"1.12.9") != -1 && !is.na(PROSTAR.version) && PROSTAR.version != "NA") {
    #     return (FALSE)
    #   } else {
    #     return(TRUE)
    #   }
    # })
    
    
  })
  
  
}



#' @export
#' @rdname infos_dataset
#' 
infos_dataset <- function(obj){
  
  ui <- fluidPage(infos_dataset_ui("mod_info"))
  
  server <- function(input, output, session) {
    infos_dataset_server("mod_info", 
      dataIn = reactive({obj}))
  }

  app <- shiny::shinyApp(ui, server)
}