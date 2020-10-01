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