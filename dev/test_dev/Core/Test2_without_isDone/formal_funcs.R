#' @description Removes the item called 'name' and all further items
#' in the list
#' 
#' 


AddItemToDataset <- function(dataset, name){
addAssay(dataset, 
         dataset[[length(dataset)]], 
          name=name)
}


