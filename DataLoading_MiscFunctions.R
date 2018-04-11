as.numeric.silent <- function(x,...){
  #supressed the warnings of converting a vector to numeric
  return(suppressWarnings(as.numeric(x,...)))
}

filterOptions <- function(filter, data){
  #adds the necessary UI options for the filters to be used
  
  if (filter$Data.Type == "String"){
    #how to clean it up if it is a string
    placeholder <- data[,eval(parse(text = filter$Data.Table))] 
    placeholder2 <- unlist(strsplit(placeholder, ";"))
    output$Values <- sort(unique(gsub("^\\s+", "", placeholder2))) #sorts alphabetically and 
    #removes leading spaces
  }
  else if (output$Data.Type == "Numeric"){
    #it must be numeric
    #how to clean it up if it is numeric
    filter$Min <- data[,min(eval(parse(text = filter$Data.Table)), na.rm = T)]
    filter$Max <- data[,max(eval(parse(text = filter$Data.Table)), na.rm = T)]
  }
  else{
    # it must be a query string
    filter$Query <- 1 #have a value assigned to 1 for query
  }
  
  return(filter)
  
}


'%out%' <- Negate(`%in%`)