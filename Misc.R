cleanmultitemp <- function(multi_data){

  barrel1_indices <- grep("Barrel.1", colnames(dev_multi), ignore.case = T)
  barrel2_indicies <- grep("Barrel.2", colnames(dev_multi), ignore.case = T)
  barrel3_indicies <- grep("Barrel.3", colnames(dev_multi), ignore.case = T)
  
  row_count <- 1
  while(row_count < nrow(multi_data) + 1){
    if(is.na(multi_data$Part.Number[row_count])){
      multi_data[row_count - 1, barrel2_indicies] <- multi_data[row_count, barrel1_indices]
      if(is.na(multi_data$Part.Number[row_count + 1])){
        multi_data[row_count - 1, barrel3_indicies] <- multi_data[row_count+1, barrel1_indices]
      }
      
    }
    
    row_count <- row_count + 1
  }
  
  multi_data <- multi_data[!is.na(multi_data$Part.Number),]
  
  return(multi_data)
  
}


convertnas <- function(pps_data){
  column_count <- 1
  
  while(column_count < ncol(pps_data) + 1){
    pps_data[is.na(pps_data[,column_count]), column_count] <- "NA"
    column_count <- column_count + 1
  }
  
  return(pps_data)
  
}