library(visNetwork)



df <- list()


path_to_source <- '../R/'
listfiles <- paste0(path_to_source,list.files(path_to_source))
listfiles <- listfiles[grep('../R/mod_', listfiles)]
listfiles <- c("../R/app_server.R",listfiles)


pattern_def_module  <- '@rdname'
pattern_call_module <- "callModule"
for (f in listfiles){
  res1 <- grep(pattern_def_module, readLines(f), value = TRUE)
  if (length(res1)>1) {res1 <- unique(res1)}
  if(length(res1)==0) res1<-''
  
  
  
  res2 <- grep(pattern_call_module, readLines(f), value = TRUE)
  if(length(res2)==0) res2<-''
  
df[[as.character(substr(f,6,nchar(f)-2))]] <- list(
              def_module=unlist(lapply(res1,
                                       function(x){substr(x, 
                                                          unlist(gregexpr(pattern ='@rdname',x))+8,
                                                          nchar(x)-1)})),
              called_module =  unlist(lapply(res2,
                                             function(x){substr(x, 
                                                                unlist(gregexpr(pattern ='callModule',x))+11,
                                                                unlist(gregexpr(pattern ='_server',x))-1)})))

}


list_all_def_modules <- list_all_called_modules <- NULL

for (i in 1:length(df)){
  list_all_def_modules <- c(list_all_def_modules, df[[i]]$def_module)
  list_all_called_modules <- c(list_all_called_modules, df[[i]]$called_module)
}
list_all_def_modules <- unique(list_all_def_modules)
list_all_called_modules <- c("app_server",unique(list_all_called_modules))

edge.a <- edge.b <- NULL
for (i in 1:length(df)){
  a <- which(names(df)[i]==list_all_called_modules)
  b <-unlist(lapply( df[[i]]$called_module, function(x) {which(x==list_all_called_modules)}))
  if (length(a)>0 && length(b)>0){
    edge.b <- c(edge.b, b)
    edge.a <- c(edge.a, rep(a,length(b)))

  }
}

##remove self linked
s <- NULL
tmp.a <- NULL
tmp.b <- NULL
for (i in 1:length(edge.b))
{
  if (edge.a[i]!=edge.b[i]){
    tmp.a <- c(tmp.a,edge.a[i])
    tmp.b <- c(tmp.b,edge.b[i])
  }
}

nodes <- data.frame(id = 1:length(list_all_called_modules),
                    label = list_all_called_modules,
                    shape = c('circle',rep("square",length(list_all_called_modules)-1)),
                    size=c(30,rep(10,length(list_all_called_modules)-1)),
                    stringsAsFactors = FALSE)


edges <- data.frame(from=tmp.a, 
                    to=tmp.b,
                    arrows = rep('to',length(tmp.a)),
                    stringsAsFactors = FALSE)

visNetwork(nodes, edges)

