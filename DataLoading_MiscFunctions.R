as.numeric.silent <- function(x,...){
  #supressed the warnings of converting a vector to numeric
  return(suppressWarnings(as.numeric(x,...)))
}

filterOptions <- function(filter){
  #adds the necessary UI options for the filters to be used
  if (filter$Data.Type == "String"){
    #how to clean it up if it is a string
    placeholder <- total_pps_data[,eval(parse(text = filter$Data.Table))] 
    placeholder2 <- unlist(strsplit(placeholder, ";"))
    filter$Values <- sort(unique(gsub("^\\s+", "", placeholder2))) #sorts alphabetically and 
    #removes leading spaces
  }
  else if (filter$Data.Type == "Numeric"){
    #it must be numeric
    #how to clean it up if it is numeric
    filter$Min <- total_pps_data[,min(eval(parse(text = filter$Data.Table)), na.rm = T)]
    filter$Max <- total_pps_data[,max(eval(parse(text = filter$Data.Table)), na.rm = T)]
  }
  else if (filter$Data.Type == "Query"){
    # it must be a query string
    filter$Query <- 1 #have a value assigned to 1 for query
  }
  else{
    #it is blank
    filter$Blank <- 0 #place a zero for the blank for the one blank
  }
  
  return(filter)
  
}


'%out%' <- Negate(`%in%`)