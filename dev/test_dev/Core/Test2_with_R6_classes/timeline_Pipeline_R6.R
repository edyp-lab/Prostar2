#Timeline_Pipeline_R6.R
TimelinePipeline = R6Class("TimelinePipeline",
                          inherit = Timeline,
                          private = list(
                            modal_txt = "This action will reset this process. The input dataset will be the output of the previous
                      validated process and all further datasets will be removed",
                            # This function catches any event on config$status and analyze it
                            # to decide whether to disable/enable UI parts
                            Analyse_status = function(){
                              if(private$verbose)
                                print(paste0('TL(',config$process.name, ') : Analyse_status() :'))
                              # Display current page
                              # One display all processes, even the validated ones
                              lapply(1:private$length, function(x){
                                shinyjs::toggle(paste0('div_screen', x), condition = x==self$rv$current.pos)})
                            }
                          ),
                          
                          public = list()
)