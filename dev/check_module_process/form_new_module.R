source("./copy_paste_test_wParam.R")

library(shiny)


# download button


ui <- fluidPage(
  
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
    
    widgets_list <- c()
    for(i in 1:length(unlist(strsplit(input$module_widgets,',')))){
      
      widget <- unlist(strsplit(input$module_widgets,','))
      widget_value <- unlist(strsplit(input$module_widgets_value,','))
      
      if(is.na(as.numeric(widget_value[i]))){
        widget_value[i] <- paste0('\"',widget_value[i],'\"')
      } else{
        widget_value[i] <- widget_value[i]
      }
      
      widget <- paste0(widget[i],'=',widget_value[i])
      
      widgets_list <- c(widgets_list,widget)
      widgets_list <- paste0(widgets_list,collapse = ',')
    }
    
    #----------------------------------------------------------------#
    
    file <- paste0("./test/temp_mod_pipe_",name,".R")
    
    cat(NULL, file=file)
    append=TRUE
    
    create_ui(name=name,file=file, append = append)
    
    create_start_server(name=name,file=file, append = append)
    
    create_rNav(process=process,steps = steps,file=file, append = append)
    
    create_rvModule(process=process,widgets_list=widgets_list,file=file, append = append)
    
    create_reset(widgets_list=widgets_list,nb_screen=length(unlist(strsplit(steps,","))),file=file, append = append)
    
    create_screen(process=process,file=file, append = append)
    
    create_widgets_from_input(widgets_list=widgets_list,file=file, append = append)
    
    create_end_server(name=name,file=file, append = append)
    
    create_watch_file(name=name,process=process)
  }
  
}


shinyApp(ui=ui, server=server)
