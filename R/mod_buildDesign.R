#' @title xxxx
#'
#' @description
#' xxxx
#'
#' @name build-design
#'
#' @param id xxx
#' @param quantCols xxx
#' @param remoteReset A `logical(1)` which acts as a remote command to reset
#' the module to its default values. Default is FALSE.
#' @param is.enabled xxx
#'
#' @return NA
#'
#' @examples
#' if (interactive()){
#' shiny::runApp(mod_buildDesign(letters[seq(6)]))
#' }
#' 
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' @importFrom MagellanNTK Get_Code_Declare_widgets Get_Code_for_ObserveEvent_widgets Get_Code_for_rv_reactiveValues Get_Code_Declare_rv_custom Get_Code_for_dataOut format_DT_server Timestamp toggleWidget
#'
NULL


#' @rdname build-design
#' @importFrom shiny NS tagList actionLink fluidRow column uiOutput hr 
#' @importFrom shinyjs useShinyjs hidden toggle disable enable show
#' @export
#'
mod_buildDesign_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    tags$style(HTML("
    .Design_mod_content label{
      float:left;
    }
    .Design_mod_content .radio-inline {
      margin-left: 30px;
    }")),
    # tags$p(
    #   "If you do not know how to fill the experimental design, you can
    #         click on the '?' next to each design in the list that appear
    #         once the conditions are checked or got to the ",
    #   actionLink(ns("linkToFaq1"), "FAQ", style = "background-color: white"),
    #   " page."
    # ),
    div(style = "display: flex; gap: 20px;",
      div(#width = "100%",
          div(style = "display: flex; justify-content: space-between;", 
              h4("Design"), 
              uiOutput(ns("reset_btn")), style = "margin-bottom: 5px;"),
          rhandsontable::rHandsontableOutput(ns("hot")),
      ),
      div(
        div(class = "Design_mod_content", style = "display: flex; gap: 20px; margin-bottom: 20px;",
          div(
            tags$b("1 - Fill the \"Condition\" column to identify the conditions to compare."),
            uiOutput(ns("UI_reorder"), style = "white-space: nowrap; margin-left: 15px;"),
          ),
          uiOutput(ns("UI_checkConditions"))
        ),
        
        div(style = "display: flex; gap: 20px;",
            uiOutput(ns("UI_hierarchicalExp")),
            uiOutput(ns("checkDesign"))
        )
      )
    )
    # tags$div(
    #     style = "display:inline-block; vertical-align: top;",
    #     shinyjs::hidden(div(id = "showExamples", uiOutput(ns("designExamples"))))
    #   )
  )
}


#' @rdname build-design
#' @export
#' @importFrom stats setNames
mod_buildDesign_server <- function(
    id,
    quantCols = reactive({NULL}),
    remoteReset = reactive({0}),
    is.enabled = reactive({TRUE})) {
  
  requireNamespace("magrittr")
  
  widgets.default.values <- list(
    convert_reorder = "No",
    chooseExpDesign = "FlatDesign"
  )
  
  rv.custom.default.values <- list(
    hot = data.frame(
      quantCols = as.character(quantCols()),
      Condition = rep("", length(quantCols())),
      stringsAsFactors = FALSE
    ),
    conditionsChecked = NULL,
    newOrder = NULL,
    resettingHot = TRUE
  )
  
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    core <- paste0(
      MagellanNTK::Get_Code_Declare_widgets(names(widgets.default.values)),
      MagellanNTK::Get_Code_for_ObserveEvent_widgets(names(widgets.default.values)),
      MagellanNTK::Get_Code_for_rv_reactiveValues(),
      MagellanNTK::Get_Code_Declare_rv_custom(names(rv.custom.default.values)),
      MagellanNTK::Get_Code_for_dataOut(),
      MagellanNTK::Get_Code_for_remoteReset(widgets = TRUE, custom = TRUE, dataIn = NA),
      sep = "\n"
    )
    eval(str2expression(core))

    
    # Init ----
    dataOut <- reactiveValues(
      trigger = NULL,
      design = NULL,
      order = NULL
    )
    
    
    # Reset ----
    output$reset_btn <- renderUI({
      widget <- actionButton(ns('reset'), 'Reset design')
      
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    
    observeEvent({list(remoteReset(), input$reset)}, ignoreInit = TRUE, ignoreNULL = TRUE, {
        rv.custom$hot <- data.frame(
          quantCols = as.character(quantCols()),
          Condition = rep("", length(quantCols())),
          stringsAsFactors = FALSE
        )
        
        rv.custom$resettingHot <- TRUE
        
        rv.widgets$convert_reorder <- "No"
        rv.custom$conditionsChecked <- NULL
        rv.custom$designChecked <- NULL
        rv.widgets$chooseExpDesign <- "FlatDesign"
        
        dataOut$trigger <- NULL
        dataOut$design <- NULL
        dataOut$order <- NULL
      })
    
    
    # Text ----
    observeEvent(req(input$linkToFaq1), {
      updateTabsetPanel(session, "navPage", "faqTab")
    })
    
    
    # Table ----
    output$hot <- rhandsontable::renderRHandsontable({
      hot <- rhandsontable::rhandsontable(
        rv.custom$hot,
        rowHeaders = NULL,
        fillHandle = list(
          direction = "vertical",
          autoInsertRow = FALSE,
          maxRows = nrow(rv.custom$hot)
        )
      ) |>
        rhandsontable::hot_rows(rowHeights = 30) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = TRUE,
          allowColEdit = FALSE,
          allowInsertRow = FALSE,
          allowInsertColumn = FALSE,
          allowRemoveRow = TRUE,
          allowRemoveColumn = FALSE,
          autoInsertRow = FALSE
        ) |>
        rhandsontable::hot_cols(renderer = color_renderer()) |>
        rhandsontable::hot_col(col = "quantCols", readOnly = TRUE)
      
      if (isTRUE(rv.custom$conditionsChecked$valid)){
        hot <- hot |>
          rhandsontable::hot_col(
            col = "Condition",
            readOnly = TRUE
          )
      }
      
      if (!is.null(rv.widgets$chooseExpDesign) && isTRUE(rv.custom$conditionsChecked$valid)) {
        switch(rv.widgets$chooseExpDesign,
               FlatDesign = {
                 if ("Bio.Rep" %in% colnames(rv.custom$hot)) {
                   hot <- hot |>
                     rhandsontable::hot_col(
                       col = "Bio.Rep",
                       readOnly = TRUE
                     )
                 }
               },
               twoLevelsDesign = {
                 if ("Tech.Rep" %in% colnames(rv.custom$hot)) {
                   hot <- hot |>
                     rhandsontable::hot_col(
                       col = "Tech.Rep",
                       readOnly = TRUE
                     )
                   if (isTRUE(rv.custom$designChecked$valid)){
                     hot <- hot |>
                       rhandsontable::hot_col(
                         col = "Bio.Rep",
                         readOnly = TRUE
                       )
                   }
                 }
               },
               threeLevelsDesign = {
                 if ("Analyt.Rep" %in% colnames(rv.custom$hot)) {
                   hot <- hot |>
                     rhandsontable::hot_col(col = "Analyt.Rep", readOnly = TRUE)
                   if (isTRUE(rv.custom$designChecked$valid)){
                     hot <- hot |>
                       rhandsontable::hot_col(
                         col = "Tech.Rep",
                         readOnly = TRUE
                       ) |>
                       rhandsontable::hot_col(
                         col = "Bio.Rep",
                         readOnly = TRUE
                       )
                   }
                 }
               }
        )
      }
      
      hot
    })
    
    color_renderer <- reactive({
      rv.custom$hot$Condition
      conds <- rv.custom$hot$Condition

      req(length(conds) > 0)
      if (length(which(conds == "")) == 0) {
        uniqueConds <- unique(conds)
      } else {
        uniqueConds <- unique(conds[-which(conds == "")])
      }

      nUniqueConds <- length(uniqueConds)
      pal <- ExtendPalette(nUniqueConds)

      txt <- "function (instance, td, row, col, prop, value, cellProperties) {
  Handsontable.renderers.TextRenderer.apply(this, arguments);"
      c <- 1
      for (i in seq_along(conds)) {
        if (conds[i] != "") {
          txt <- paste0(
            txt, "if(row==", (i - 1), " && col==",
            c, ") {td.style.background = '",
            pal[which(conds[i] == uniqueConds)], "';}"
          )
        }
      }
      txt <- paste0(txt, "}")

      return(txt)
    })    
    
    observeEvent(req(input$hot), {
      if (rv.custom$resettingHot) {
        rv.custom$resettingHot <- FALSE
      } else {
        rv.custom$hot <- rhandsontable::hot_to_r(input$hot)
      }
    })
    
    
    # Conditions ----
    output$UI_reorder <- renderUI({
      widget <- radioButtons(ns("convert_reorder"),
                             "Order by conditions ?",
                             choices = setNames(nm = c("No", "Yes")),
                             #selected = rv.widgets$keep_vs_remove,
                             inline = TRUE
                             )

      MagellanNTK::toggleWidget(widget, !isTRUE(rv.custom$conditionsChecked$valid))
    })
    
    output$UI_checkConditions <- renderUI({
      req(rv.custom$hot)
      rv.custom$conditionsChecked
      rv.widgets$convert_reorder
      req(sum(rv.custom$hot$Condition == "") == 0)
      widget <- actionButton(ns("btn_checkConds"), 
                             "Check conditions"
            )
      
      txt <- NULL
      img <- NULL
      if (!is.null(rv.custom$conditionsChecked)) {
              if (isTRUE(rv.custom$conditionsChecked$valid)) {
                img <- img(src = "images/Ok.png", height = 25)
              } else {
                img <- img(src = "images/Problem.png", height = 25)
                txt <- p(style = "color: red;", 
                         rv.custom$conditionsChecked$warn)
                
              }
      } 
      
      tagList(
        div(style = "display: flex;",
            MagellanNTK::toggleWidget(widget, !isTRUE(rv.custom$conditionsChecked$valid)),
            img),
        txt
      )
    })    
    
    observeEvent(input$btn_checkConds, {
      req(rv.widgets$convert_reorder)

      # if (length(grep("Bio.Rep", colnames(rv.custom$hot))) > 0) {
      #   return(NULL)
      # }
      req(!("Bio.Rep" %in% colnames(rv.custom$hot)))

      if (rv.widgets$convert_reorder == "Yes") {
        rv.custom$newOrder <- order(rv.custom$hot[, "Condition"])
        rv.custom$hot <- rv.custom$hot[rv.custom$newOrder, ]
      }

      rv.custom$conditionsChecked <- checkConditions(rv.custom$hot$Condition)
    })
    
    
    # Design ----
    output$UI_hierarchicalExp <- renderUI({
      req(rv.custom$conditionsChecked)
      req(rv.custom$conditionsChecked$valid)
      
      widget <- radioButtons(ns("chooseExpDesign"), "",
          choices = c(
            "Flat design (automatic)" = "FlatDesign",
            "2 levels design (complete Bio.Rep column)" = "twoLevelsDesign",
            "3 levels design (complete Bio.Rep and Tech.Rep columns)" = "threeLevelsDesign"
          ),
          selected = rv.widgets$chooseExpDesign
        )

      tagList(
        div(id = ns('div_UI_hierarchicalExp'),
          div(id = ns('div_choosetype'),
            style = "display:inline-block; vertical-align: middle;",
            tags$b("2 - Choose the type of experimental design and complete it accordingly")
          )# ,
          # div(id = ns('div_btn_helpDesign'),
          #   style = "display:inline-block; vertical-align: middle;",
          #   tags$button(
          #     id = "btn_helpDesign", tags$sup("[?]"),
          #     class = "Prostar_tooltip"
          #   )
          # )
        ),
        MagellanNTK::toggleWidget(widget, !isTRUE(rv.custom$designChecked$valid))
      )
    })


    output$designExamples <- renderUI({
      req(rv.widgets$chooseExpDesign)

      switch(rv.widgets$chooseExpDesign,
        FlatDesign = {
          tags$p("There is nothing to do for the flat design: the 'Bio.Rep'
           column is already filled.")
        },
        twoLevelsDesign = {
          tagList(
            h4("Example for a 2-levels design"),
            Prostar2::mod_designExample_server("buildDesignExampleTwo", 2),
            Prostar2::mod_designExample_ui(ns("buildDesignExampleTwo"))
          )
        },
        threeLevelsDesign = {
          tagList(
            h4("Example for a 3-levels design"),
            Prostar2::mod_designExample_server("buildDesignExampleThree", 3),
            Prostar2::mod_designExample_ui(ns("buildDesignExampleThree"))
          )
        }
      )
    })


    observe({
      shinyjs::onclick("btn_helpDesign", {
        shinyjs::toggle(id = "showExamples", anim = TRUE)
      })
    })


    observeEvent({rv.widgets$chooseExpDesign
      rv.custom$conditionsChecked$valid}, {
      req(isTRUE(rv.custom$conditionsChecked$valid))
      rv.custom$hot
      rv.custom$designChecked <- NULL
      switch(rv.widgets$chooseExpDesign,
        FlatDesign = {
          rv.custom$hot <- data.frame(rv.custom$hot[, seq_len(2)],
            Bio.Rep = seq_len(nrow(rv.custom$hot)),
            stringsAsFactors = FALSE
          )
        },
        twoLevelsDesign = {
          rv.custom$hot <- data.frame(rv.custom$hot[, seq_len(2)],
            Bio.Rep = rep("", nrow(rv.custom$hot)),
            Tech.Rep = seq_len(nrow(rv.custom$hot)),
            stringsAsFactors = FALSE
          )
        },
        threeLevelsDesign = {
          rv.custom$hot <- data.frame(rv.custom$hot[, seq_len(2)],
            Bio.Rep = rep("", nrow(rv.custom$hot)),
            Tech.Rep = rep("", nrow(rv.custom$hot)),
            Analyt.Rep = seq_len(nrow(rv.custom$hot)),
            stringsAsFactors = FALSE
          )
        }
      )
    })


    observeEvent(input$btn_checkDesign, {
      rv.custom$designChecked <- checkDesign(rv.custom$hot)
    })


    output$checkDesign <- renderUI({
      req(rv.widgets$chooseExpDesign)
      rv.custom$designChecked
      req(rv.custom$conditionsChecked)

      req(rv.custom$conditionsChecked$valid)

      switch(isolate({
        rv.widgets$chooseExpDesign
      }),
      FlatDesign = {},
      twoLevelsDesign = {
        if (sum(rv.custom$hot$Bio.Rep == "") > 0) {
          return(NULL)
        }
      },
      threeLevelsDesign = {
        if ((sum(rv.custom$hot$Bio.Rep == "") + sum(rv.custom$hot$Tech.Rep == "")) > 0) {
          return(NULL)
        }
      }
      )
      
      widget <- actionButton(ns("btn_checkDesign"), 
                             "Check design")
      img <- NULL
      txt <- NULL
      
      if (!is.null(rv.custom$designChecked)) {
        if (isTRUE(rv.custom$designChecked$valid)) {
          img <- img(src = "images/Ok.png", height = 25)
        } else {
          img <- img(src = "images/Problem.png", height = 25)
          txt <- unique(rv.custom$designChecked$warn)
        }
      }

      tagList(
        div(style = "display: flex;",
            MagellanNTK::toggleWidget(widget, !isTRUE(rv.custom$designChecked$valid)),
            img),
        div(style = "color: red;",
          tags$ul(
            lapply(txt, function(x) {tags$li(x)})
          )
        )
      )
    })

    observeEvent(req(rv.custom$designChecked$valid), {
      req(isTRUE(rv.custom$conditionsChecked$valid))
      
      dataOut$trigger <- MagellanNTK::Timestamp()
      
        dataOut$design <- rv.custom$hot
        #dataOut$design <- NULL

      if (rv.widgets$convert_reorder == "Yes") {
        dataOut$order <- rv.custom$newOrder
      } else {
        dataOut$order <- order(rownames(rv.custom$hot))
      }
    })


    return(reactive({dataOut}))
  })
} 




#' @rdname build-design
#' @export
mod_buildDesign <- function(quantCols) {
  ui <- fluidPage(
    #actionButton('reset', 'Reset'),
    mod_buildDesign_ui("buildDesign")
  )

  server <- function(input, output, session) {
    
    res <- reactiveVal(NULL)
    
    observe({
    res <- mod_buildDesign_server("buildDesign", 
      quantCols = reactive({quantCols})
      )
  })
 
    observeEvent(req(res()$design), ignoreInit = TRUE,{
      message(res()$design)
    })
  }

  app <- shinyApp(ui, server)
}
