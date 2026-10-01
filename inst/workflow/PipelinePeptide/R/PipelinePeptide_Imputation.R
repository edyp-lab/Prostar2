#' @title PipelinePeptide Imputation module
#'
#' @description
#' This module contains the imputation step of the peptide pipeline.
#'
#' @param id A `character(1)` which is the 'id' of the module.
#' @param dataIn An instance of the class `MultiAssayExperiment`
#' @param steps.enabled A vector of boolean which has the same length of the steps
#' of the pipeline. This information is used to enable/disable the widgets. It is not
#' a communication variable between the caller and this module, thus there is no
#' corresponding output variable
#' @param remoteReset It is a remote command to reset the module. An `integer()` that
#' indicates is the pipeline has been reseted by a program of higher level
#' Basically, it is the program which has called this module
#' @param steps.status A vector of `character()` which indicates the status of each step
#' which can be either 'validated', 'undone' or 'skipped'. Enabled or disabled in the UI.
#' @param current.pos A `integer(1)` which acts as a remote command to make
#'  a step active in the timeline. Default is 1.
#' @param path A `character()` which is the path to the directory which
#' contains the files and directories of the pipeline.
#'
#' @return An instance of the class `MultiAssayExperiment`
#'
#' @examples
#' if (interactive()) {
#'   Prostar2("PipelinePeptide_Imputation")
#' }
#'
#' @name PipelinePeptide_Imputation
#'
#' @importFrom stats setNames rnorm
#' @importFrom shinyjs useShinyjs
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#'
NULL


#' @rdname PipelinePeptide_Imputation
#' @export
#'
PipelinePeptide_Imputation_conf <- function(){
  MagellanNTK::Config(
    fullname = 'PipelinePeptide_Imputation',
    mode = 'process',
    steps = c('Imputation'),
    mandatory = c(FALSE)
  )
}


#' @rdname PipelinePeptide_Imputation
#' @export
#'
PipelinePeptide_Imputation_ui <- function(id){
  ns <- NS(id)
}


#' @rdname PipelinePeptide_Imputation
#' @export
#'
PipelinePeptide_Imputation_server <- function(id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({NULL}),
  remoteReset = reactive({0}),
  steps.status = reactive({NULL}),
  current.pos = reactive({1}),
  path = NULL,
  btnEvents = reactive({NULL})
){
  pkgs_require(c('QFeatures', 'SummarizedExperiment', 'S4Vectors'))
  
  # Default values for widgets
  widgets.default.values <- list(
    Imp_algorithm = "None",
    Pirat_extension = "base",
    Pirat_alpha.factor = 2,
    BPCA_nPcs = 2
  )
  
  # Default values for reactive values
  rv.custom.default.values <- list(
    history = MagellanNTK::InitializeHistory(),
    Pirat_dataformat = NULL,
    Pirat_showlog = FALSE
  )
  
  ### -------------------------------------------------------------###
  ###                                                              ###
  ### -------------------- MODULE SERVER --------------------------###
  ###                                                              ###
  ### -------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Code hosted by MagellanNTK to create the process
    # DO NOT MODIFY THESE LINES
    core.code <- MagellanNTK::Get_Workflow_Core_Code(
      mode = 'process',
      name = id,
      w.names = names(widgets.default.values),
      rv.custom.names = names(rv.custom.default.values)
    )
    
    eval(str2expression(core.code))
    add_resourcePath()
    
    
    ###########################################################################-
    #
    #-----------------------------DESCRIPTION-----------------------------------
    #
    ###########################################################################-
    output$Description <- renderUI({
      file <- normalizePath(file.path(
        system.file('workflow', package = 'Prostar2'),
        unlist(strsplit(id, '_'))[1], 
        'md', 
        paste0(id, '.Rmd')))
      
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns('open_dataset_UI'))
        ),
        content = div(id = ns('div_content'),
                      if (file.exists(file))
                        includeMarkdown(file)
                      else
                        p('No Description available')
        )
      )
    })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Description', btnEvents()))
      req(dataIn())
      
      # Copy the input dataset to use it during this step
      rv$dataIn <- dataIn()
      
      # Copy data to a Pirat compliant format
      rv.custom$Pirat_dataformat <- list(
        peptides_ab = t(SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]])),
        adj = as.matrix(SummarizedExperiment::rowData(rv$dataIn[[length(rv$dataIn)]])$adjacencyMatrix)
      ) 
      
      # DO NOT MODIFY THE NEXT THREE LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status['Description'] <- MagellanNTK::stepStatus$VALIDATED
    })
    
    ###########################################################################-
    #
    #------------------------------IMPUTATION-----------------------------------
    #
    ###########################################################################-
    output$Imputation <- renderUI({
      shinyjs::useShinyjs()
      
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns("Imp_UI")),
          uiOutput(ns("Imp_param_UI"))
        ),
        content = tagList(
          uiOutput(ns("Pirat_messbox")),
          uiOutput(ns("Imp_warning")),
          uiOutput(ns('Imp_plotPirat_UI')),
          uiOutput(ns('Imp_mvplots_ui'))
        )
      )
    })
    
    #### _sidebar -----
    # Widget - show widgets if no empty lines
    output$Imp_UI <- renderUI({
      req(rv$dataIn)
      
      .data <- SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]])
      nbEmptyLines <- DaparToolshed::getNumberOfEmptyLines(.data)
      
      if (nbEmptyLines > 0) {
        tags$p("Your dataset contains empty lines (fully filled with missing
    values). In order to use the imputation tool, you must delete them by
      using the filter tool.")
      } else if (sum(is.na(.data)) == 0) {
        tags$p("Your dataset does not contain missing values.")
      } else {
        tagList(
          uiOutput(ns("Imp_algorithm_UI")),
          uiOutput(ns("Imp_paramPirat_UI")),
          uiOutput(ns("Imp_paramPirat_T_UI")),
          uiOutput(ns("Imp_paramBPCA_UI"))
        )
      }
    })
    
    # Widget - type of imputation to perform
    output$Imp_algorithm_UI <- renderUI({
      widget <- selectInput(ns("Imp_algorithm"), "Method",
                            choices = list(
                              "None" = "None",
                              "Pirat" = "Pirat",
                              "impSeq" = "impSeq",
                              "BPCA" = "BPCA"
                            ),
                            selected = rv.widgets$Imp_algorithm,
                            width = "200px")
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Imputation"])
    })
    
    # Widget - Pirat parameters
    output$Imp_paramPirat_UI <- renderUI({
      req(rv.widgets$Imp_algorithm == "Pirat")
      
      # Extension input
      widget1 <- selectInput(ns("Pirat_extension"), 
                             "Extension", 
                             choices = c(
                               "base" = "base", 
                               "2" = "2", 
                               "S" = "S"),
                             selected = rv.widgets$Pirat_extension,
                             width = "200px")
      # Alpha factor input
      widget2 <- numericInput(
        ns("Pirat_alpha.factor"), 
        "Alpha factor", 
        value = rv.widgets$Pirat_alpha.factor,
        min = 0,
        step = 1,
        width = "200px")
      
      # Show widgets 
      tagList(
        MagellanNTK::toggleWidget(widget1, rv$steps.enabled["Imputation"]),
        MagellanNTK::toggleWidget(widget2, rv$steps.enabled["Imputation"])
      )
    })
    
    # Widget - BCPA parameters
    output$Imp_paramBPCA_UI <- renderUI({
      req(rv.widgets$Imp_algorithm == "BPCA")
      # nPcs
      widget1 <- numericInput(
        ns("BPCA_nPcs"), 
        "nPcs", 
        value = rv.widgets$BPCA_nPcs,
        min = 1,
        step = 1,
        width = "200px")
      
      # Show widgets 
      MagellanNTK::toggleWidget(widget1, rv$steps.enabled["Imputation"])
    })
    
    
    #### _content -----
    # Plot - NA plots (ui)
    output$Imp_mvplots_ui <- renderUI({
      widget <- mod_mv_plots_ui(ns("mvplots"))
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Imputation"])
    })
    
    # Plot - NA plots (server)
    observe({
      req(rv$dataIn)
      
      pal <- DaparToolshed::GetColorsForConditions(
        unique(DaparToolshed::design_qf(rv$dataIn)$Condition),
        DaparToolshed::ExtendPalette(length(unique(DaparToolshed::design_qf(rv$dataIn)$Condition)))
      )
      
      mod_mv_plots_server("mvplots",
                          data = reactive({rv$dataIn[[length(rv$dataIn)]]}),
                          grp = reactive({omXplore::get_group(rv$dataIn)}),
                          mytitle = reactive({"POV imputation"}),
                          pal = pal,
                          pattern = reactive({c("Missing", "Missing POV", "Missing MEC")})
      )
    })

    # Plot - Pirat plot (ui)
    output$Imp_plotPirat_UI <- renderUI({
      req(rv.widgets$Imp_algorithm == "Pirat")
      
      fluidRow(
        column(width = 6,
               h5("Empirical densities of correlations between peptides chosen randomly and between sibling peptides"),
               plotOutput(ns("plot_correlation_pirat")),
               uiOutput(ns("plot_reload_btn"))),
        column(width = 6,
               h5("Regression of the log-probability of missing onto mean observed abundance"),
               plotOutput(ns("plot_missingness_mechanism"))))
    })
    
    # Plot - Pirat plot - correlation (server)
    output$plot_correlation_pirat <- renderPlot({
      req(rv$dataIn)
      req(rv.widgets$Imp_algorithm == "Pirat")
      req(!is.null(input$pirat_plot_reload_btn) || input$pirat_plot_reload_btn == 0)
      
      Pirat::plot_pep_correlations(pep.data = rv.custom$Pirat_dataformat)
    })
    
    # Widget - reload button for Pirat correlation plot 
    output$plot_reload_btn <- renderUI({
      widget <- shiny::actionButton(ns("pirat_plot_reload_btn"), "Reload plot")
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Imputation"])
    })    
    
    # Plot - Pirat plot - missingness mechanism (server)
    output$plot_missingness_mechanism <- renderPlot({ 
      req(rv$dataIn)
      req(rv.widgets$Imp_algorithm == "Pirat")
      
      missmechPiratPlot(rv.custom$Pirat_dataformat)
    })
    
    # Pirat box messages
    output$Pirat_messbox <- renderUI({
      req(rv.widgets$Imp_algorithm == "Pirat")
      div(style="max-height: 150px; overflow: auto;",
          id = ns("notif_box"),  # ID pour for notification box
          style = "border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9; margin-top: 20px;",
          div(id = ns("notif_message"))  # Where messages will be displayed
      )
    })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Imputation', btnEvents()))
      req(rv$dataIn)

      if (is.null(rv$dataIn) || 
           rv.widgets$Imp_algorithm == "None"){
        shinyjs::info(btnVentsMasg)
        
      } else {
        withProgress(message = "", detail = "", value = 0, {
          incProgress(0.25, detail = "Initializing imputation")
  
          # Perform imputation
          result <- imputationPept(data = rv$dataIn,
                                     method = rv.widgets$Imp_algorithm,
                                     history = rv.custom$history,
                                     dataPirat = rv.custom$Pirat_dataformat,
                                     extension = rv.widgets$Pirat_extension,
                                     alpha.factor = rv.widgets$Pirat_alpha.factor,
                                     nPcs = rv.widgets$BPCA_nPcs)
          
          # Update values
          .tmp <- result$data
          rv.custom$history <- result$history
  
          if(inherits(.tmp, "try-error") || inherits(.tmp, "try-warning")) {
            mod_SweetAlert_server(id = 'sweetalert_perform_POVimputation_button',
                                  text = .tmp,
                                  type = 'error' )
          } else {
            incProgress(1, detail = "Finalizing imputation")
            .tmp <- DaparToolshed::UpdateMetacellAfterImputation(.tmp)
        
            rv$dataIn <- Prostar2::addDatasets(rv$dataIn,
                                               .tmp,
                                               'Imputation')
            names(rv$dataIn)[length(rv$dataIn)] <- 'Imputation'
            
            # DO NOT MODIFY THE THREE FOLLOWING LINES
            dataOut$trigger <- MagellanNTK::Timestamp()
            dataOut$value <- NULL
            rv$steps.status['Imputation'] <- MagellanNTK::stepStatus$VALIDATED
          }
        })
      }
    })

    ###########################################################################-
    #
    #-------------------------------------SAVE----------------------------------
    #
    ###########################################################################-
    output$Save <- renderUI({
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(),
        content = tagList(
          uiOutput(ns("save_txt")),
          uiOutput(ns("dl_ui"))
        )
      )
    })
    
    #### _content -----
    # Save text (before saving)
    output$save_txt <- renderUI({
      req(rv$steps.status["Save"] != MagellanNTK::stepStatus$VALIDATED)
      req(config@mode == "process")
      
      save_txt_ui()
    })
    
    # Download (ui) (after saving)
    output$dl_ui <- renderUI({
      req(rv$steps.status['Save'] == MagellanNTK::stepStatus$VALIDATED)
      req(config@mode == 'process')
      
      Prostar2::download_dataset_ui(ns(paste0(id, '_createQuickLink')))
    })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Save', btnEvents()))
      
      if (!("Imputation" %in% names(rv$dataIn))) {
        shinyjs::info(btnVentsMasg)
        
      } else {
        shiny::withProgress(message = paste0("Saving process", id), {
          shiny::incProgress(0.5)
          
          # Add the history
          rv$dataIn <- prepareQFsave(
            data = rv$dataIn,
            history = rv.custom$history,
            namePipeline = 'PipelinePeptide'
          )
          
          # DO NOT MODIFY THE NEXT THREE LINES
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- rv$dataIn
          rv$steps.status['Save'] <- MagellanNTK::stepStatus$VALIDATED
          
          # Download (server)
          Prostar2::download_dataset_server(paste0(id, '_createQuickLink'), dataIn = reactive({dataOut$value}))
          shiny::incProgress(1)
        })
      }
    })
    
    ####### _END_ -----
    
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
  })
}
