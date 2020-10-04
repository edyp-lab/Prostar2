library(shinyjs)


source(file.path('./Timelines', 'mod_timeline.R'), local=TRUE)$value
source(file.path('.', 'mod_super_timeline.R'), local=TRUE)$value
source(file.path('../../../../R', 'global.R'), local=TRUE)$value
source(file.path('.', 'mod_wf_wf1_A.R'), local=TRUE)$value
source(file.path('.', 'mod_wf_wf1_B.R'), local=TRUE)$value
source(file.path('.', 'mod_wf_wf1_C.R'), local=TRUE)$value
source(file.path('.', 'formal_funcs.R'), local=TRUE)$value


options(shiny.fullstacktrace = T)
options(shiny.reactlog=TRUE) 

ui <- fluidPage(
  tagList(
    mod_super_timeline_ui("super_nav"),
    hr(),
    wellPanel(
      h3('Prostar (caller)'),
      fluidRow(
        column(width=2,
               p('Data input :'),
               uiOutput('show_dataIn')
        ),
        column(width=2,
               p('Data output :'),
               uiOutput('show_rv_tmp_dataOut'))
      )
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  rv <- reactiveValues(
    current.obj = Exp1_R25_prot[1:10,,-1],
    tmp = NULL
  )
  
  rv$tmp <- mod_super_timeline_server("super_nav", 
                                dataIn = reactive({rv$current.obj}) )
  
   observeEvent(rv$tmp(), {
     print('TEST SUPER_TIMELINE : retour du module mod_super_timeline_server : rv$tmp$dataOut() = ')
     print(rv$tmp())
     })


  
   output$show_dataIn <- renderUI({
     tagList(lapply(names(rv$current.obj), function(x){tags$p(x)}))
   })
   output$show_rv_dataOut <- renderUI({
     req(rv$dataOut)
     tagList(
       lapply(names(rv$dataOut), function(x){tags$p(x)})
     )
   })

}


shinyApp(ui, server)
