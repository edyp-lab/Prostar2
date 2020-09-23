source("./copy_paste_test_wParam.R")

library(shiny)

# nom des step > nb de screens
# quelle etapes mandatory ?
# test attention pas match nb screen avec mandatory/isDone...
# download button


ui <- fluidPage(
  textInput('module_name','name'),
  textInput('module_process','process'),
  textInput('module_steps','steps (sep by \',\')'),
  uiOutput('widgets'),
  actionButton('validate', 'Generate module')
)


server <- function(input, output, session){
  
  output$widgets <- renderUI({
    fluidRow(column(2,
                    textInput('module_widgets','widgets')),
             column(1,
                    "="),
             column(2,
                    textInput('module_widgets_value','default values')))
  })
  
  
  observeEvent(input$validate, {
    
    name=paste0(input$module_name,'_shinyTest')
    process=input$module_process
    test=paste0('c(',unlist(strsplit(steps,',')),')')
    # "screen1,table2,plop" to 'c(\'screen1\', \'table2\', \'plop\')
    widgets=paste0(input$module_widgets,'=',input$module_widgets_value)
    print(name)
    print(process)
    print(test)
    print(widgets)
    
    createModule('protein_Filtering',
                'Filtering',
                test,
                widgets)
    # createModule(name='protein_Filtering',
    #              process='Filtering',
    #              steps = 'c(\'screen1\', \'table2\', \'plop\', \'toto\', \'titi\')',
    #              widgets_list='ChooseFilters = "None",seuilNA = 0, seuil_plop=50',
    #              append=TRUE)
  })
  
  
  
}


shinyApp(ui=ui, server=server)
