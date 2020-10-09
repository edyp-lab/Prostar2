#' @description Removes the item called 'name' and all further items
#' in the list
#' 
#' 
RemoveItemFromDataset <- function(dataset, name){
  ind <- grep(name, names(dataset))
  if (length(ind) > 0)
    dataset[ , , -c(ind:length(dataset))]
  else
    dataset
}


AddItemToDataset <- function(dataset, name){
addAssay(dataset, 
         dataset[[length(dataset)]], 
          name=name)
}


Reset_Pipeline_Data_logics <- function(){
  rv$dataOut <- dataIn()
}



