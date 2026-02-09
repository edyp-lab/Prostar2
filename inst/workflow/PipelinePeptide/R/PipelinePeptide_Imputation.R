#' @title Shiny example process module.
#'
#' @description
#' This module contains the configuration informations for the corresponding pipeline.
#' It is called by the nav_pipeline module of the package MagellanNTK
#' 
#' The name of the server and ui functions are formatted with keywords separated by '_', as follows:
#' * first string `mod`: indicates that it is a Shiny module
#' * `pipeline name` is the name of the pipeline to which the process belongs
#' * `process name` is the name of the process itself
#' 
#' This convention is important because MagellanNTK call the different
#' server and ui functions by building dynamically their name.
#' 
#' In this example, `PipelinePeptide_Imputation_ui()` and `PipelinePeptide_Imputation_server()` define
#' the code for the process `PipelinePeptide_Imputation` which is part of the pipeline called `PipelinePeptide`.
#' 
#' @examples
#' if (interactive()){
#' library(MagellanNTK)
#' data(Exp2_R100_pept, package = 'DaparToolshedData')
#' path <- system.file('workflow/PipelinePeptide', package = 'Prostar2')
#' shiny::runApp(proc_workflowApp("PipelinePeptide_Imputation", path, dataIn = Exp2_R100_pept))
#' }
#' 
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' 
NULL

#' @rdname PipelinePeptide
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

#' @param id xxx
#' 
#' @rdname PipelinePeptide
#' 
#' @author Samuel Wieczorek
#' 
#' @export
#'
PipelinePeptide_Imputation_ui <- function(id){
  ns <- NS(id)
}


#' @param id xxx
#'
#' @param dataIn The dataset
#'
#' @param steps.enabled A vector of boolean which has the same length of the steps
#' of the pipeline. This information is used to enable/disable the widgets. It is not
#' a communication variable between the caller and this module, thus there is no
#' corresponding output variable
#'
#' @param remoteReset It is a remote command to reset the module. A boolean that
#' indicates is the pipeline has been reseted by a program of higher level
#' Basically, it is the program which has called this module
#' 
#' @param steps.status xxx
#' 
#' @param current.pos xxx
#'
#' @rdname PipelinePeptide
#' 
#' @importFrom stats setNames rnorm
#' @importFrom shinyjs useShinyjs
#' 
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
  pkgs.require(c('QFeatures', 'SummarizedExperiment', 'S4Vectors'))
  
  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- list(
    Imp_algorithm = "None",
    Pirat_extension = "base",
    Pirat_alpha.factor = 2,
    Pirat_mcar = FALSE,
    Pirat_max.pg.size.pirat.t = 1
  )
  
  rv.custom.default.values <- list(
    result_open_dataset = reactive({NULL}),
    
    history = MagellanNTK::InitializeHistory(),
    Pirat_dataformat = NULL,
    Pirat_showlog = FALSE
  )
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    core.code <- MagellanNTK::Get_Workflow_Core_Code(
      mode = 'process',
      name = id,
      w.names = names(widgets.default.values),
      rv.custom.names = names(rv.custom.default.values)
    )
    
    eval(str2expression(core.code))
    add.resourcePath()
    
    
    ###########################################################################-
    #
    #-----------------------------DESCRIPTION-----------------------------------
    #
    ###########################################################################-
    output$Description <- renderUI({
      # file <- normalizePath(file.path(session$userData$workflow.path, 
      #   'md', paste0(id, '.md')))
      
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
                      #div(id = ns("chunk"), style = "width: 100px; height: 100px;" ),
                      if (file.exists(file))
                        includeMarkdown(file)
                      else
                        p('No Description available')
                      #uiOutput(ns('Description_infos_dataset_UI'))
        )
      )
    })
    
    output$open_dataset_UI <- renderUI({
      req(session$userData$runmode == 'process')
      req(is.null(dataIn()))
      req(NULL)
      rv.custom$result_open_dataset <- MagellanNTK::open_dataset_server(
        id = "open_dataset",
        class = 'QFeatures',
        extension = "qf",
        remoteReset = reactive({remoteReset()})
      )
      
      MagellanNTK::open_dataset_ui(id = ns("open_dataset"))
    })
    
    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Description', btnEvents()))
      req(dataIn())
      rv$dataIn <- dataIn()
      
      if(!is.null(rv.custom$result_open_dataset()$dataset))
        rv$dataIn <- rv.custom$result_open_dataset()$dataset
      
      rv.custom$Pirat_dataformat <- list(
        peptides_ab = t(SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]])),
        adj = as.matrix(SummarizedExperiment::rowData(rv$dataIn[[length(rv$dataIn)]])$adjacencyMatrix)
      ) 
      
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
      path <- file.path(system.file('www/css', package = 'MagellanNTK'),'MagellanNTK.css')
      includeCSS(path)
      
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
    
    output$Imp_mvplots_ui <- renderUI({
      widget <- mod_mv_plots_ui(ns("mvplots"))
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Imputation"])
    })
    
    observe({
      req(rv$dataIn)
      mod_mv_plots_server("mvplots",
                          data = reactive({rv$dataIn[[length(rv$dataIn)]]}),
                          grp = reactive({get_group(rv$dataIn)}),
                          mytitle = reactive({"POV imputation"}),
                          pal = reactive({NULL}),
                          pattern = reactive({c("Missing", "Missing POV", "Missing MEC")})
      )
    })
    
    MagellanNTK::mod_popover_for_help_server( ### Correlation plot title and informations
      "plot_correlation_title",
      title = h4("Empirical densities of correlations between peptides chosen randomly and between sibling peptides"),
      content = HTML(paste0("The more the within-PG correlation distribution is right-shifted with respect to that of random correlations, the better Pirat’s performances.", 
                            "<br>", "<br>",
                            "See the FAQ for more infomations.")))
    
    output$plot_reload_btn <- renderUI({ ### Reload plot button
      widget <- shiny::actionButton(ns("pirat_plot_reload_btn"), "Reload plot")
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Imputation"])
    })
    
    output$plot_correlation_pirat <- renderPlot({ ### Correlation plot
      req(rv$dataIn)
      req(rv.widgets$Imp_algorithm == "Pirat")
      req(!is.null(input$pirat_plot_reload_btn) | input$pirat_plot_reload_btn == 0)
      
      Pirat::plot_pep_correlations(pep.data = rv.custom$Pirat_dataformat)
    })
    
    
    MagellanNTK::mod_popover_for_help_server( ###  title and informations
      "plot_missingness_mechanism_title",
      title = h4("Regression of the log-probability of missing onto mean observed abundance"),
      content = HTML(paste0("Fitting of missingness mechanism.", "<br>",
                            "Show the estimation of the parameters for gamma",
                            "<br>", "<br>",
                            "See the FAQ for more infomations.")))
    
    output$plot_missingness_mechanism <- renderPlot({ ### 
      req(rv$dataIn)
      req(rv.widgets$Imp_algorithm == "Pirat")
      
      mv_rates <- colMeans(is.na(rv.custom$Pirat_dataformat$peptides_ab))
      mean_abund <- colMeans(rv.custom$Pirat_dataformat$peptides_ab, na.rm = T)
      mean_abund_sorted <- sort(mean_abund, index.return = T)
      mv_rates_sorted <- mv_rates[mean_abund_sorted$ix]
      kernel_size <- 10
      probs <- rep(0, length(mean_abund) - kernel_size + 1)
      for (i in 1:length(probs)) {
        probs[i] <- mean(mv_rates_sorted[i:(i + kernel_size - 1)])
      }
      not0 <- probs != 0
      m_ab_sorted <- mean_abund_sorted$x[not0]
      probs <- probs[not0]
      res.reg <- lm(log(probs) ~ m_ab_sorted[1:length(probs)])
      sum.reg.reg <- summary(res.reg)
      plot(m_ab_sorted[1:length(probs)], log(probs),
           ylab="log(p_mis)", 
           xlab="observed mean")
      abline(res.reg, col="red")
      mylabel = bquote(italic(R)^2 == .(format(summary(res.reg)$r.squared, digits = 3)))
      text(x = m_ab_sorted[1]+1, y = (log(probs)[1]+log(probs)[length(log(probs))])/2, labels = mylabel)
    })
    
    output$Imp_plotPirat_UI <-renderUI({
      req(rv.widgets$Imp_algorithm == "Pirat")
      
      tagList(
      MagellanNTK::mod_popover_for_help_ui(ns("plot_correlation_title")),
      plotOutput(ns("plot_correlation_pirat")),
      uiOutput(ns("plot_reload_btn")),
      MagellanNTK::mod_popover_for_help_ui(ns("plot_missingness_mechanism_title")),
      plotOutput(ns("plot_missingness_mechanism")))
    })
    
    output$Pirat_messbox <- renderUI({
      req(rv.widgets$Imp_algorithm == "Pirat")
      print("oui")
      div(style="max-height: 150px; overflow: auto;",
          id = ns("notif_box"),  # ID pour la boîte de notification
          style = "border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9; margin-top: 20px;",
          div(id = ns("notif_message"))  # Conteneur où les messages seront affichés
      )
    })
    
    show_log_console <- function(
    expr,
    id_notif = NULL,
    cond_info = TRUE,
    cond_warning = TRUE,
    cond_error = TRUE,
    color_info = "grey",
    color_warning = "blue",
    color_error = "red",
    new_first = TRUE,
    console_message = TRUE,
    prefix = "Imputation"
    ) {
      notifications <- reactiveVal(list()) # To store messages 
      prefix <- paste0(prefix, if (prefix == "") " " else "-")
      msg_actions <- list( ### Create functions to use depending on message type
        message = function(m) { ### For info
          if (cond_info){
            ### Show custom message in console
            if (console_message) message_console(m$message, level = "INFO", info_text = paste0(prefix, "INFO"), color_info = color_info)
            ### Get message
            if (!all(m$message %in% c("\r", "\n", ""))){
              new_notification <- paste0("<b>[", prefix, "INFO]</b> <i>", format(Sys.time(), "%d-%m-%Y %X"), "</i> — ", m$message)
              current_notifications <- notifications()
              if (new_first) current_notifications <- c(paste0('<span style="color: ', color_info, '">', new_notification, '</span>'), current_notifications)
              else current_notifications <- c(current_notifications, paste0('<span style="color: ', color_info, '">', new_notification, '</span>'))
              notifications(current_notifications) # Update list of messages
              ### Show message in app
              if (!is.null(id_notif)) shinyjs::html(id_notif, paste("<ul>", paste("<li>", current_notifications, "</li>", collapse = ""), "</ul>"))
            }
          }
        },
        warning = function(m) { ### For warning
          if (cond_warning){
            ### Show custom message in console
            if (console_message) message_console(m$message, level = "WARNING", warning_text = paste0(prefix, "WARNING"), color_warning = color_warning)
            ### Get message
            if (!all(m$message %in% c("\r", "\n", ""))){
              new_notification <- paste0("<b>[", prefix, "WARNING]</b> <i>", format(Sys.time(), "%d-%m-%Y %X"), "</i> — ", m$message)
              current_notifications <- notifications()
              if (new_first) current_notifications <- c(paste0('<span style="color: ', color_warning, '">', new_notification, '</span>'), current_notifications)
              else current_notifications <- c(current_notifications, paste0('<span style="color: ', color_warning, '">', new_notification, '</span>'))
              notifications(current_notifications) # Update list of messages
              ### Show message in app
              if (!is.null(id_notif)) shinyjs::html(id_notif, paste("<ul>", paste("<li>", current_notifications, "</li>", collapse = ""), "</ul>"))
            }
          }
        },
        error = function(m) { ### For error
          if (cond_error){
            ### Show custom message in console
            if (console_message) message_console(m$message, level = "ERROR", error_text = paste0(prefix, "ERROR"), color_error = color_error)
            ### Get message
            if (!all(m$message %in% c("\r", "\n", ""))){
              new_notification <- paste0("<b>[", prefix, "ERROR]</b> <i>", format(Sys.time(), "%d-%m-%Y %X"), "</i> — ", m$message)
              current_notifications <- notifications()
              if (new_first) current_notifications <- c(paste0('<span style="color: ', color_warning, '">', new_notification, '</span>'), current_notifications)
              else current_notifications <- c(current_notifications, paste0('<span style="color: ', color_warning, '">', new_notification, '</span>'))
              notifications(current_notifications) # Update list of messages
              ### Show message in app
              if (!is.null(id_notif)) shinyjs::html(id_notif, paste("<ul>", paste("<li>", current_notifications, "</li>", collapse = ""), "</ul>"))
            }
          }
        }
      )
      
      if (console_message) { ### Show custom message in console
        tryCatch(
          suppressMessages(suppressWarnings( # Remove R messages in console
            withCallingHandlers( # Add custom messages in console
              expr,
              message = function(m) msg_actions$message(m),
              warning = function(m) msg_actions$warning(m)
            ))),
          error = function(m) {
            msg_actions$error(m)
            return(NULL)}
          
        )
      } else { ### Show message only in app
        tryCatch(
          withCallingHandlers(
            expr,
            message = function(m) msg_actions$message(m),
            warning = function(m) msg_actions$warning(m)
          )
          ,
          error = function(m) {
            msg_actions$error(m)
            return(NULL)
          }
        )
      }
      return(notifications())
    }
    
    message_console <- function(msg,
                                level = "INFO",
                                info_text = "INFO",
                                warning_text = "WARNING",
                                error_text = "ERROR",
                                color_info = "grey",
                                color_warning = "blue",
                                color_error = "red",
                                color_other = "black"){
      msg <- paste0(msg, collapse = "")
      # Text in the prefix depending on message type
      level_text <- switch(toupper(level), 
                           "INFO" = info_text,
                           "WARNING" = warning_text,
                           "ERROR" = error_text,
                           level
      )
      # Create message
      msg <- if(!(msg %in% c("\r", "\n", ""))){ paste0("[", level_text, "] ", format(Sys.time(), "%d-%m-%Y %X"), " — ", msg)}
      # Show custom message in console depending on message type
      switch(toupper(level),
             "INFO" = message(print_color(msg, color_info)),
             "WARNING" = print_color(paste0(msg, "\n"), color_warning),
             "ERROR" = print_color(paste0(msg, "\n"), color_error),
             cat(print_color(msg, color_other), sep = "\n")
      )
    }
    
    
    output$Imp_UI <- renderUI({
      # Checks if
      req(rv$dataIn)
      .data <- SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]])
      nbEmptyLines <- DaparToolshed::getNumberOfEmptyLines(.data)
      if (nbEmptyLines > 0) {
        tags$p("Your dataset contains empty lines (fully filled with missing
    values). In order to use the imputation tool, you must delete them by
      using the filter tool.")
      } else if (sum(is.na(.data)) == 0) {
        tags$p("Your dataset does not contains missing values.")
      } else {
        tagList(
          uiOutput(ns("Imp_algorithm_UI")),
          uiOutput(ns("Imp_paramPirat_UI")),
          uiOutput(ns("Imp_paramPirat_T_UI"))
        )
      }
    })
    

    output$Imp_algorithm_UI <- renderUI({
      widget <- selectInput(ns("Imp_algorithm"), "Method",
                            choices = list(
                              "None" = "None",
                              "Pirat" = "Pirat"
                            ),
                            selected = rv.widgets$Imp_algorithm,
                            width = "150px"
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Imputation"])
    })
    
    
    output$Imp_paramPirat_UI <- renderUI({
      req(rv.widgets$Imp_algorithm == "Pirat")
        # Extension input
        widget1 <- selectInput(ns("Pirat_extension"), 
                               "Extension", 
                               choices = c(
                                 "base" = "base", 
                                 "2" = "2", 
                                 #"T" = "T", 
                                 "S" = "S"),
                               selected = rv.widgets$Pirat_extension
        )
        # Alpha factor input
        widget2 <- numericInput(
          ns("Pirat_alpha.factor"), 
          "Alpha factor", 
          value = rv.widgets$Pirat_alpha.factor,
          min = 0,
          step = 1
        )
        # MCAR input
        widget3 <- shinyWidgets::awesomeCheckbox(ns("Pirat_mcar"), 
                                   "MCAR", 
                                   value = rv.widgets$Pirat_mcar
        )
        
        # Show widgets 
        tagList(
          MagellanNTK::toggleWidget(widget1, rv$steps.enabled["Imputation"]),
          MagellanNTK::toggleWidget(widget2, rv$steps.enabled["Imputation"]),
          MagellanNTK::toggleWidget(widget3, rv$steps.enabled["Imputation"])
        )
    })
  
    output$Imp_paramPirat_T_UI <- renderUI({ ### Transcriptomic specific widgets
      req(rv.widgets$Pirat_extension == "T")
      # Max PG size input
      widget <- shinyWidgets::autonumericInput(
        ns("Pirat_max.pg.size.pirat.t"), 
        "Max PG size", 
        value = rv.widgets$Pirat_max.pg.size.pirat.t, 
        decimalPlaces = 0,
        minimumValue = 0,
        digitGroupSeparator = " ",
        modifyValueOnWheel = TRUE,
        align = "left"
      )
      # add rna.cond.mask if extension == 'T'
      # add pep.cond.mask if extension == 'T'
      
      # Show widgets
      tagList(
        MagellanNTK::toggleWidget(widget, rv$steps.enabled["Imputation"])
      )
    })


    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Imputation', btnEvents()))
      req(rv$dataIn)

      if (is.null(rv$dataIn) || 
           rv.widgets$Imp_algorithm == "None"){
        shinyjs::info(btnVentsMasg)
      } else {
        withProgress(message = "", detail = "", value = 0, {
          incProgress(0.25, detail = "Initializing imputation")
  
          .tmp <- NULL
          .param <- list()
  
          try({
            switch(rv.widgets$Imp_algorithm,
                   None = .tmp <- rv$dataIn[[length(rv$dataIn)]],
                   Pirat = {
                     incProgress(0.5, detail = "Pirat imputation")
                     rv.custom$Pirat_showlog <- TRUE
                     Pirat_logtxt <- show_log_console(prefix = "PIRAT", id_notif = "notif_message", console_message = FALSE, {
                       Pirat_dataimput <- Pirat::my_pipeline_llkimpute(rv.custom$Pirat_dataformat,
                                                     alpha.factor = rv.widgets$Pirat_alpha.factor,
                                                     rna.cond.mask = NULL, #if extension == 'T' only
                                                     pep.cond.mask = NULL, #if extension == 'T' only
                                                     extension = rv.widgets$Pirat_extension,
                                                     mcar = rv.widgets$Pirat_mcar,
                                                     max.pg.size.pirat.t = rv.widgets$Pirat_max.pg.size.pirat.t, #if extension == 'T' only
                                                     verbose = TRUE)
                     })
                     
                     rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Imputation', 'Imputation', 'algorithm', rv.widgets$Imp_algorithm)
                     rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Imputation', 'Imputation', 'extension', rv.widgets$Pirat_extension)
                     rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Imputation', 'Imputation', 'alpha.factor', rv.widgets$Pirat_alpha.factor)
                     rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Imputation', 'Imputation', 'mcar', rv.widgets$Pirat_mcar)
                     #rna.cond.mask = NULL, 
                     #pep.cond.mask = NULL,
                     #max.pg.size.pirat.t = rv.widgets$Pirat_max.pg.size.pirat.t
                     
                     .tmp <- rv$dataIn[[length(rv$dataIn)]] 
                     SummarizedExperiment::assay(.tmp, withDimnames=FALSE) <- t(Pirat_dataimput$data.imputed)
                   }
            )
          })
  
          if(inherits(.tmp, "try-error") || inherits(.tmp, "try-warning")) {
            mod_SweetAlert_server(id = 'sweetalert_perform_POVimputation_button',
                                  text = .tmp,
                                  type = 'error' )
          } else {
            incProgress(1, detail = "Finalizing imputation")
            .tmp <- DaparToolshed::UpdateMetacellAfterImputation(.tmp)
        
            rv$dataIn <- Prostar2::addDatasets(
              rv$dataIn,
              .tmp,
              'Imputation')
            DaparToolshed::paramshistory(rv$dataIn[[length(rv$dataIn)]]) <- rv.custom$history
            names(rv$dataIn)[length(rv$dataIn)] <- 'Imputation'
          }
        })
        
        req(Pirat_dataimput$data.imputed)
        # DO NOT MODIFY THE THREE FOLLOWING LINES
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- NULL
        rv$steps.status['Imputation'] <- MagellanNTK::stepStatus$VALIDATED
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
          uiOutput(ns('dl_ui'))
        )
      )
    })
    
    
    output$dl_ui <- renderUI({
      req(rv$steps.status['Save'] == MagellanNTK::stepStatus$VALIDATED)
      req(config@mode == 'process')
      
      Prostar2::download_dataset_ui(ns(paste0(id, '_createQuickLink')))
    })
    
    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Save', btnEvents()))
      req("Imputation" %in% names(rv$dataIn))
      
      shiny::withProgress(message = paste0("Saving process", id), {
        shiny::incProgress(0.5)
        S4Vectors::metadata(rv$dataIn)$name.pipeline <- 'PipelinePeptide'
        
        # DO NOT MODIFY THE THREE FOLLOWING LINES
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- rv$dataIn
        rv$steps.status['Save'] <- MagellanNTK::stepStatus$VALIDATED
        
        Prostar2::download_dataset_server(paste0(id, '_createQuickLink'), dataIn = reactive({dataOut$value}))
      })
    })
    
    # <<< end ------------------------------------------------------------------
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
  }
  )
}
