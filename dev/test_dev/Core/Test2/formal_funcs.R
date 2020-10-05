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


# is.validated <- function(value){value == 'validated'}
# is.notrun <- function(value){value == 'notrun'}
# is.skipped  <- function(value){value == 'skipped'}
# 
# SetSkipped <-function(ll, name){ll[[name]] <- 'skipped'}
# SetNotRun <-function(ll, name){ll[[name]] <- 'notrun'}
# SetValidated <- function(ll, name){ll[[name]] <- 'validated'}
