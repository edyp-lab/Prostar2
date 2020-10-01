# Module UI

#' @title   mod_build_design_ui and mod_build_design_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param sampleNames A vector de names for the samples in the dataset
#'
#' @rdname mod_build_design
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @importFrom shinyBS bsModal
#' @importFrom shinyjs useShinyjs
#' @import rhandsontable 
mod_build_design_ui <- function(id){
  ns <- NS(id)
  shinyjs::useShinyjs()
  tagList(
    tags$p("If you do not know how to fill the experimental design, you can click
           on the '?' next to each design in the list that appear once the conditions
           are checked or got to the ",
           actionLink(ns("linkToFaq1"), "FAQ", style="background-color: white"),
           " page."),
    
    fluidRow(
      column(width=6,tags$b("1 - Fill the \"Condition\" column to identify the conditions to compare.")),
      column(width=6,uiOutput(ns("UI_checkConditions"))  )
    ),
    fluidRow(
      column(width=6,uiOutput(ns("UI_hierarchicalExp"))),
      column(width=6,uiOutput(ns("checkDesign") ))
    ),
    hr(),
    selectInput(ns("convert_reorder"), "Order by conditions ?",
                choices=c("No"="No", "Yes"="Yes"),
                width="100px"),
    h4("Design table"),
    tags$div(
      tags$div(style="display:inline-block; vertical-align: top;",
               rhandsontable::rHandsontableOutput(ns("hot"),width="100%"),
      ),
      tags$div(style="display:inline-block; vertical-align: top;",
               shinyjs::hidden(uiOutput(ns('showExamples')))
      )
    )
  )
}

# Module Server

#' @rdname mod_build_design
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @import rhandsontable
#' @importFrom DAPAR2 CheckDesign check.conditions
#' @import shinyjs
#' 
mod_build_design_server <- function(id, sampleNames){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rv.buildDesign <- reactiveValues(
      design_df = NULL,
      hot_table = NULL,
      designChecked = NULL,
      newOrder = NULL,
      conditionsChecked = NULL,
      designSaved = NULL,
      level = NULL,
      out = NULL
    )
    
    
    mod_insert_md_server('FAQ', URL_FAQ)
    
    mod_build_design_example_server('designExamples')
    
    
    output$showExamples <- renderUI({
      mod_build_design_example_ui(ns('designExamples') )
    })
    
    color_renderer <- reactive({
      req(rv.buildDesign$design_df)
      conds <- rv.buildDesign$design_df$Condition
      pal <- RColorBrewer::brewer.pal(8, 'Dark2')
      txt <- "function (instance, td, row, col, prop, value, cellProperties) {
                      Handsontable.renderers.TextRenderer.apply(this, arguments);"
      c <- 1
      for (i in 1:length(conds)){
        if (conds[i] != "")
          txt <- paste0(txt, "if(row==",(i-1)," && col==",c, ") {td.style.background = '",pal[which(conds[i] == unique(conds))],"';}")
      }
      txt <- paste0(txt,"}")
      
      return (txt)
    })
    
    
    
    
    
    #----------------------------------------------------------
    observeEvent(input$btn_checkConds,{
      input$convert_reorder
      
      if (length(grep("Bio.Rep", colnames(rv.buildDesign$design_df))) > 0)  { return(NULL)}
      
      if (isTRUE(input$convert_reorder)) {
        rv.buildDesign$newOrder <- order(rv.buildDesign$design_df["Condition"])
        rv.buildDesign$design_df <- rv.buildDesign$design_df[rv.buildDesign$newOrder,]
      }
      
      rv.buildDesign$conditionsChecked <- DAPAR2::check.conditions(rv.buildDesign$design_df$Condition)
      
    })
    
    
    
    #----------------------------------------------------------
    observeEvent(req(sampleNames()),{
      rv.buildDesign$design_df  <- data.frame(Sample.name = as.character(sampleNames()),
                                              Condition = rep("",length(sampleNames())),
                                              stringsAsFactors = FALSE)
    })
    
    
    
    
    #-------------------------------------------------------------
    output$hot <- rhandsontable::renderRHandsontable({
      rv.buildDesign$design_df
      input$chooseExpDesign
      
      tmp <- rhandsontable::rhandsontable(rv.buildDesign$design_df,
                                          rowHeaders=NULL, 
                                          fillHandle = list(direction='vertical', 
                                                            autoInsertRow=FALSE,
                                                            maxRows=nrow(rv.buildDesign$design_df)
                                          )
      ) %>%
        rhandsontable::hot_rows(rowHeights = 30) %>%
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, 
                                        allowColEdit = FALSE,
                                        allowInsertRow = FALSE,
                                        allowInsertColumn = FALSE,
                                        allowRemoveRow = TRUE,
                                        allowRemoveColumn = FALSE,
                                        autoInsertRow=FALSE) %>%
        rhandsontable:: hot_cols(renderer = color_renderer()) %>%
        rhandsontable::hot_col(col = "Sample.name", readOnly = TRUE)
      
      if (!is.null(input$chooseExpDesign)) {
        switch(input$chooseExpDesign,
               FlatDesign = {
                 if ("Bio.Rep" %in% colnames(rv.buildDesign$design_df))
                   tmp <- tmp %>% rhandsontable::hot_col(col = "Bio.Rep", readOnly = TRUE)
               },
               twoLevelsDesign = {
                 if ("Tech.Rep" %in% colnames(rv.buildDesign$design_df))
                   tmp <- tmp %>% rhandsontable::hot_col(col =  "Tech.Rep", readOnly = TRUE)
               } ,
               threeLevelsDesign = {
                 if ("Analyt.Rep" %in% colnames(rv.buildDesign$design_df))
                   tmp <- tmp %>% rhandsontable::hot_col(col = "Analyt.Rep", readOnly = TRUE)
               }
        )
      }
      
      tmp
      
    })
    
    
    # 
    # 
    # 
    # #----------------------------------------------------------
    output$UI_checkConditions  <- renderUI({
      
      req(rv.buildDesign$design_df)
      rv.buildDesign$conditionsChecked
      input$convert_reorder
      
      if ((sum(rv.buildDesign$design_df$Condition == "")==0) && (input$convert_reorder != "None")){
        tags$div(
          tags$div(style="display:inline-block;",
                   actionButton(ns("btn_checkConds"), "Check conditions", class = actionBtnClass)
          ),
          
          tags$div(style="display:inline-block;",
                   if(!is.null(rv.buildDesign$conditionsChecked)){
                     
                     if (isTRUE(rv.buildDesign$conditionsChecked$valid)){
                       img <- "www/images/Ok.png"
                       txt <- "Correct conditions"
                     }else {
                       img <- "www/images/Problem.png"
                       txt <- "Invalid conditions"
                     }
                     tagList(
                       tags$div(
                         tags$div(style="display:inline-block;",tags$img(src = img, height=25)),
                         tags$div(style="display:inline-block;",tags$p(txt))
                       ),
                       if(!isTRUE(rv.buildDesign$conditionsChecked$valid)){
                         tags$p(rv.buildDesign$conditionsChecked$warn)
                       }
                     )
                   }
          )
        )
      }
    })
    
    
    
    # #------------------------------------------------------------------------------
    output$UI_hierarchicalExp <- renderUI({
      req(rv.buildDesign$conditionsChecked)
      if (!isTRUE(rv.buildDesign$conditionsChecked$valid)){
        return(NULL) } 
      
      
      tagList(
        div(
          div(
            # edit1
            style="display:inline-block; vertical-align: middle;",
            tags$b("2 - Choose the type of experimental design and complete it accordingly")
          ),
          div(
            # edit2
            style="display:inline-block; vertical-align: middle;",
            #tags$button(id=ns("btn_helpDesign"), tags$sup("[?]"), class="Prostar_tooltip")
            actionLink(ns("btn_helpDesign"), tags$sup("?"),style="background-color: white, color: blue")
          )
        ),
        
        radioButtons(ns("chooseExpDesign"), "",
                     choices = c("Flat design (automatic)" = "FlatDesign" ,
                                 "2 levels design (complete Bio.Rep column)" = "twoLevelsDesign" ,
                                 "3 levels design (complete Bio.Rep and Tech.Rep columns)" = "threeLevelsDesign" ),
                     selected=character(0))
      )
      
    })
    
    
    
    # observe({
    #   shinyjs::onclick('btn_helpDesign', {
    #     print('click on btn_helpDesign')
    #   })
    # })
    # 
    
    
    # #------------------------------------------------------------------------------
    observeEvent(input$btn_helpDesign,{
      #req(input$chooseExpDesign)
      shinyjs::toggle(id = "showExamples", anim = TRUE)
      
    })
    
    
    
    
    #------------------------------------------------------------------------------
    observeEvent(input$chooseExpDesign, {
      rv.buildDesign$design_df
      #rv.buildDesign$designChecked <- FALSE
      
      switch(input$chooseExpDesign,
             FlatDesign = {
               rv.buildDesign$design_df <- data.frame(rv.buildDesign$design_df[,1:2],
                                                      Bio.Rep = seq(1:nrow(rv.buildDesign$design_df)),
                                                      stringsAsFactors = FALSE)
               rv.buildDesign$level <- 1
             },
             twoLevelsDesign = {
               rv.buildDesign$design_df  <- data.frame(rv.buildDesign$design_df[,1:2],
                                                       Bio.Rep = rep("",nrow(rv.buildDesign$design_df)),
                                                       Tech.Rep = seq(1:nrow(rv.buildDesign$design_df)),
                                                       stringsAsFactors = FALSE)
               rv.buildDesign$level <- 2
             },
             threeLevelsDesign = {
               rv.buildDesign$design_df  <- data.frame(rv.buildDesign$design_df[,1:2],
                                                       Bio.Rep = rep("",nrow(rv.buildDesign$design_df)),
                                                       Tech.Rep = rep("",nrow(rv.buildDesign$design_df)),
                                                       Analyt.Rep = seq(1:nrow(rv.buildDesign$design_df)),
                                                       stringsAsFactors = FALSE)
               rv.buildDesign$level <- 3
             }
      )
    })
    
    
    #------------------------------------------------------------------------------
    observeEvent(input$hot,{
      rv.buildDesign$design_df <-  rhandsontable::hot_to_r(input$hot)
    })
    
    
    
    #------------------------------------------------------------------------------
    observeEvent(input$btn_checkDesign,{
      rv.buildDesign$designChecked <- DAPAR2::CheckDesign(rv.buildDesign$design_df)
    })
    
    
    #------------------------------------------------------------------------------
    output$checkDesign <- renderUI({
      req(rv.buildDesign$conditionsChecked)
      req(rv.buildDesign$design_df)
      req(input$chooseExpDesign)
      rv.buildDesign$designChecked
      
      if (!isTRUE(rv.buildDesign$conditionsChecked$valid)){
        return(NULL) } 
      
      replicateComplete <- length(which(rv.buildDesign$design_df[,3:ncol(rv.buildDesign$design_df)] == ''))==0
      if (replicateComplete) {
        tags$div(
          tags$div(
            style="display:inline-block;",
            actionButton(ns("btn_checkDesign"), "Check design", class = actionBtnClass)
          ),
          
          tags$div(
            style="display:inline-block;",
            if(!is.null(rv.buildDesign$designChecked)){
              
              if (isTRUE(rv.buildDesign$designChecked$valid)){
                img <- "www/images/Ok.png"
                txt <- "Correct design"
                rv.buildDesign$out <- rv.buildDesign$design_df
                #rvNavProcess$Done[4] <- TRUE
              }else {
                img <- "www/images/Problem.png"
                txt <- "Invalid design"
              }
              tagList(
                tags$div(
                  tags$div(style="display:inline-block;",tags$img(src = img, height=25)),
                  tags$div(style="display:inline-block;",tags$p(txt))
                )
              )
            } 
          )
          
        )
      }
      
    })
    
    
    return(reactive({rv.buildDesign$out}))
    
    
  })
  
}

## To be copied in the UI
# mod_build_design_ui("build_design_ui_1")

## To be copied in the server
# callModule(mod_build_design_server, "build_design_ui_1")

