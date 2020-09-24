source("./copy_paste_test_wParam.R")

library(shiny)

# nom des step > nb de screens
# quelle etapes mandatory ?
# test attention pas match nb screen avec mandatory/isDone...
# download button


ui <- fluidPage(
  
  #useShinyFeedback(),
  
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
    
    #----------------------------------------------------------------#
    
    process=input$module_process
    
    #----------------------------------------------------------------#
    
    steps=unlist(strsplit(input$module_steps,',')) #separe chaque etape
    
    steps<-paste0('\'',steps,'\'') #chaque mot encadre
    steps<-paste0(steps,collapse = ",") #chaine char sep par ,
    steps<-paste0('c(',steps,')') #chaine char like vector
    
    #----------------------------------------------------------------#
    
    if(is.na(as.numeric(input$module_widgets_value))){
      widget_value <- paste0('\"',input$module_widgets_value,'\"')
    } else{
      widget_value <- input$module_widgets_value
    }
    widgets=paste0(input$module_widgets,'=',widget_value)
    
    
    #----------------------------------------------------------------#
    
    
    createModule(name,
                 process,
                 steps,
                 widgets)
  })
  
  
  
}


shinyApp(ui=ui, server=server)
