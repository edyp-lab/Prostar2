


ui <- "
#' pipe_name UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import shinyjs
#' @importFrom shinyalert useShinyalert
#' 
mod_pipe_name_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    div(id=ns('div_nav_pipe_process'), mod_navigation_ui(ns('nav_pipe_process')))
  )
}
"



start_server <- "
#' pipe_name Server Function
#'
#' @noRd 
#' 
#' @import DAPAR2
#' @import QFeatures
#' @importFrom shinyalert shinyalert
#' 
mod_pipe_name_server <- function(input, output, session, obj, indice){
  ns <- session$ns
  
  mod_navigation_server('nav_pipe_process', style=2, pages=r.nav)
"



rNav <- "
## Section navigation module
  # Variable to manage the different screens of the module
  r.nav <- reactiveValues(
    name = 'process',
    stepsNames = steps,
    ll.UI = list(ll.UI_list),
    isDone =  rep(FALSE,nb_screen),
    mandatory =  rep(FALSE,nb_screen),
    reset = FALSE
  )
"


rvModule <- "
## reactive values for variables in the module
  rv <- reactiveValues(
    name = 'process',
    dataIn = NULL,
    dataOut = NULL,
    i = NULL,
    settings = NULL,
    
    widgets = list(widgets_list)
  )
"



reset <- "
observeEvent(req(r.nav$reset),{
    
    rv$widgets <- list(widgets_list)
    
    ## do not modify this part
    rv$dataIn <- obj()
    rv$i <- indice()
    
    r.nav$isDone <- rep(FALSE, nb_screen)
    r.nav$reset <- FALSE
    ## end of no modifiable part
    
    
  })
"



# modalAlert <- "
# #shinyalert modal asking if user wants to process a dataset with an index <i
#   observeEvent(req(rv$dataIn, rv$i ), {
#     
#     a <- (length(rv$dataIn) != rv$i) && !r.nav$isDone[length(r.nav$isDone)]
#     if (!a) return(NULL)
#     
#     shinyalert::shinyalert(
#       title = 'title',
#       text = 'This is a modal',
#       size = 'xs', 
#       closeOnEsc = TRUE,
#       closeOnClickOutside = FALSE,
#       html = FALSE,
#       type = 'info',
#       showConfirmButton = TRUE,
#       showCancelButton = TRUE,
#       confirmButtonText = 'OK',
#       confirmButtonCol = '#15A4E6',
#       cancelButtonText = 'Cancel',
#       timer = 0,
#       imageUrl = '',
#       animation = FALSE
#     )
#   })
# "



# shinyAlert <- "
# observe({
#     req(input$shinyalert)
#     rv$i
#     
#     c1 <- input$shinyalert
#     c2 <- rv$i == length(rv$dataIn)
#     c3 <- r.nav$isDone[length(r.nav$isDone)]
#     if (c1 && !c2 && !c3){
#       #Delete all assays after that one indicated by the indice given in parameter
#       rv$dataIn <- rv$dataIn[ , , -((rv$i+1):length(rv$dataIn))]
#       c1 <- input$shinyalert
#       c2 <- rv$i == length(rv$dataIn)
#       c3 <- r.nav$isDone[length(r.nav$isDone)]
#     } else {
#       # Do nothing, the module interface is still disabled
#     }
#     shinyjs::toggleState('div_nav_pipe_process', condition = !c3 && (c1||c2))
#   })
# "


# "
# disableActionButton <- function(id,session) {
#   session$sendCustomMessage(type='jsCode',
#                             list(code= paste('$('#',id,'').prop('disabled',true)'
#                                              ,sep="")))
# }
# "


end_server <-  "
 return({reactive(rv$dataOut)})

}

## To be copied in the UI
# mod_pipe_name_ui('pipe_name_ui_1')

## To be copied in the server
# mod_pipe_name_server('pipe_name_ui_1')
"


screen_content_ui <- c(
  "mod_infos_dataset_ui('infos')",
  
  "mod_plots_corr_matrix_ui('plots_corr_matrix')"
)


screen_content_server <- c(
  "mod_infos_dataset_server('infos',
                 obj = reactive({rv$dataIn}))",
  
  "rv$settings <- mod_settings_server('settings', obj=reactive({rv$dataIn}))
  
    mod_plots_corr_matrix_server('plots_corr_matrix', 
                 obj = reactive({rv$dataIn}),
                 names = reactive({NULL}),
                 gradientRate = reactive({r$settings()$defaultGradientRate})
                 )"
  )



watch_file <- "
Watch_mod_pipe_name <- mod_pipe_name_server('mod_pipe_name',  
                                                obj = reactive({rv.core$current.obj}),
                                                indice = reactive({rv.core$current.indice})
                                       )




observeEvent(req(Watch_mod_pipe_name()),{
  rv.core$current.obj <- Watch_mod_pipe_name()
})
"
