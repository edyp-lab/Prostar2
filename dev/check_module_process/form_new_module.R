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
  fluidRow(column(4, textInput('module_widgets','widgets (sep by \',\')')),
           column(1, "="),
           column(4, textInput('module_widgets_value','default values (sep by \',\')'))),
  actionButton('validate', 'Generate module')
)


server <- function(input, output, session){
  
  
  observeEvent(input$validate, {
    
    call_createModule()

  })
  
  call_createModule <- function(){
    
    name <- paste0(input$module_name,'_shinyTest')
    
    #----------------------------------------------------------------#
    
    process <- input$module_process
    
    #----------------------------------------------------------------#
    
    steps <- unlist(strsplit(input$module_steps,',')) #separe chaque etape
    
    steps<-paste0('\'',steps,'\'') #chaque mot encadre
    steps<-paste0(steps,collapse = ",") #chaine char sep par ,
    steps<-paste0('c(',steps,')') #chaine char like vector
    
    #----------------------------------------------------------------#
    
    widgets <- c()
    for(i in 1:length(unlist(strsplit(input$module_widgets,',')))){
      
      widget <- unlist(strsplit(input$module_widgets,','))
      widget_value <- unlist(strsplit(input$module_widgets_value,','))
      
      if(is.na(as.numeric(widget_value[i]))){
        widget_value[i] <- paste0('\"',widget_value[i],'\"')
      } else{
        widget_value[i] <- widget_value[i]
      }
      
      widget <- paste0(widget[i],'=',widget_value[i])
      
      widgets <- c(widgets,widget)
      widgets <- paste0(widgets,collapse = ',')
    }
    
    #----------------------------------------------------------------#
    
    
    createModule(name,
                 process,
                 steps,
                 widgets)
  }
  
}


shinyApp(ui=ui, server=server)
