library(shinyjs)


source(file.path('.', 'mod_timeline.R'), local=TRUE)$value
source(file.path('.', 'mod_super_timeline.R'), local=TRUE)$value
source(file.path('../../../../R', 'global.R'), local=TRUE)$value
source(file.path('../../../../R', 'config.R'), local=TRUE)$value
source(file.path('../../../../R', 'mod_insert_md.R'), local=TRUE)$value
source(file.path('.', 'mod_wf_wf1_Original.R'), local=TRUE)$value
source(file.path('.', 'mod_wf_wf1_Filtering.R'), local=TRUE)$value
source(file.path('.', 'mod_wf_wf1_Normalization.R'), local=TRUE)$value
source(file.path('.', 'mod_wf_wf1_Imputation.R'), local=TRUE)$value
source(file.path('.', 'mod_wf_wf1_HypothesisTest.R'), local=TRUE)$value
source(file.path('.', 'formal_funcs.R'), local=TRUE)$value


options(shiny.fullstacktrace = T)
options(shiny.reactlog=F) 


ui <- fluidPage(
  tagList(
    mod_super_timeline_ui("super_nav"),
    hr(),
    wellPanel(
      h3('Prostar (caller)'),
      fluidRow(
        column(width=2,
               tags$b(h4(style = 'color: blue;', "Data input")),
               uiOutput('show_dataIn')
        ),
        column(width=2,
               tags$b(h4(style = 'color: blue;', "Data output")),
               uiOutput('show_rv_dataOut'))
      )
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  rv <- reactiveValues(
    current.obj = Exp1_R25_prot[1:10, , -1],
    tmp = NULL
  )
  
  config <- reactiveValues(
    type = 'pipeline',
    process.name = 'Pipeline',
    steps = append(list(Original = T), pipeline.defs$protein )
  )
  
  rv$tmp <- mod_super_timeline_server("super_nav", 
                                dataIn = reactive({
                                  names(rv$current.obj)[1] <- 'Original'
                                rv$current.obj}),
                                config = config)
 
  
   output$show_dataIn <- renderUI({
     tagList(lapply(names(rv$current.obj), function(x){tags$p(x)}))
   })
   output$show_rv_dataOut <- renderUI({
     req(rv$tmp())
    tagList(
       lapply(names(rv$tmp()), function(x){tags$p(x)})
     )
   })

}


shinyApp(ui, server)
