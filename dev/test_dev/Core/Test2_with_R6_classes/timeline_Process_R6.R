#Timeline_R6.R
TimelineProcess = R6Class("TimelineProcess",
                   inherit = Timeline,
  private = list(
    modal_txt = "This action will reset this process. The input dataset will be the output of the previous
                      validated process and all further datasets will be removed",
    # This function catches any event on config$status and analyze it
    # to decide whether to disable/enable UI parts
    Analyse_status = function(){
      if(private$verbose)
        print(paste0('TL(',config$process.name, ') : Analyse_status_Process() :'))
      
      if ((length(config$status)==1) || (length(config$status)>=2 && sum(unlist(config$status)[2:private$length])== 0 )){
        # This is the case at the initialization of a process or after a reset
        if(private$verbose)
          print(paste0('TL(',config$process.name, ') : Analyse_status() : Init -> Enable all steps'))
        
        # Enable all steps and buttons
        private$toggleState_Steps(cond = TRUE, i = private$length)
      } else if (config$status[[length(private$length)]] == SKIPPED){
        # The entire process is skipped
        if(private$verbose)
          print(paste0('TL(',config$process.name, ') : Analyse_status() : The entire process is skipped'))
        # Disable all steps
        private$toggleState_Steps(cond = FALSE, i = private$length)
      } else {
        # Disable all previous steps from each VALIDATED step
        if(private$verbose)
          print(paste0('TL(',config$process.name, ') : Analyse_status() : Disable all previous steps from each VALIDATED step'))
        ind.max <- max(grep(VALIDATED, unlist(config$status)))
        private$toggleState_Steps(cond = FALSE, i = ind.max)
      }
      
      
      # One only displays the steps that are not skipped
      lapply(1:private$length, function(x){
        shinyjs::toggle(paste0('div_screen', x), condition = x==self$rv$current.pos && config$status[[self$rv$current.pos]] != SKIPPED)})
      
    }
  ),
  
  public = list()
)