library(rhandsontable)

for (f in list.files('../../R', pattern='.R')){
  source(file.path('../../R', f), local=TRUE)$value
}

ui <- fluidPage(
  tagList(
    mod_convert_ms_file_ui('convert'),
    mod_infos_dataset_ui('infos')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    convertData = NULL
    )
  
  
  rv$convertData <- callModule(mod_convert_ms_file_server, 'convert', pipeline.def=reactive({pipeline.defs}))
  
  callModule(mod_infos_dataset_server, 
             'infos', 
             obj = reactive({
               req(rv$convertData())
               rv$convertData()
             })
  )
}


shinyApp(ui, server)
