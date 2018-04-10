as.numeric.silent <- function(x,...){
  #supressed the warnings of converting a vector to numeric
  return(suppressWarnings(as.numeric(x,...)))
}