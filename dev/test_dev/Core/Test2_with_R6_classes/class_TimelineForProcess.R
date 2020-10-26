#Timeline_R6.R
TimelineForProcess = R6Class("TimelineForProcess",
                   inherit = TimelineManager,
  private = list(
    modal_txt = "This action will reset this process. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed",
    # This function catches any event on config$status and analyze it
    # to decide whether to disable/enable UI parts
    Analyse_status = function(){
      if(private$verbose)
        print(paste0('TL(', self$rv$process.name, ') : Analyse_status_Process() :'))

      if ((length(self$rv$status)==1) || (length(self$rv$status)>=2 && sum(unlist(self$rv$status)[2:private$length])== 0 )){
        # This is the case at the initialization of a process or after a reset
        if(private$verbose)
          print(paste0('TL(',self$rv$process.name, ') : Analyse_status() : Init -> Enable all steps'))
        
        # Enable all steps and buttons
        private$toggleState_Steps(cond = TRUE, i = private$length)
      } else if (self$rv$status[[length(private$length)]] == SKIPPED){
        # The entire process is skipped
        if(private$verbose)
          print(paste0('TL(',self$rv$process.name, ') : Analyse_status() : The entire process is skipped'))
        # Disable all steps
        private$toggleState_Steps(cond = FALSE, i = private$length)
      } else {
        # Disable all previous steps from each VALIDATED step
        if(private$verbose)
          print(paste0('TL(',self$rv$process.name, ') : Analyse_status() : Disable all previous steps from each VALIDATED step'))
        ind.max <- max(grep(VALIDATED, unlist(self$rv$status)))
        private$toggleState_Steps(cond = FALSE, i = ind.max)
      }
      
      
      # One only displays the steps that are not skipped
      lapply(1:private$length, function(x){
        shinyjs::toggle(paste0('div_screen', x), condition = x==self$rv$current.pos && self$rv$status[[self$rv$current.pos]] != SKIPPED)})
      
    }
  ),
  
  public = list()
)