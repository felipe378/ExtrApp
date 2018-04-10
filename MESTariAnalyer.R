#This will analyze tari data regardless of the type of extrusion

require(xlsx)
require(readxl)
require(purrr)

aggregateTariData <- function(directory){
  #this functions goes through the folder, searches for CSV files and concatenates by rows all the
  #data. This will be unecessary for the autodownload
  
  output <- data.frame() #initializes an empty data frame
  output_colnames <- c('Container', 'Parent Container', 'Container Status', 'Material', 
                       'Material Desc', 'Material Rev', 'Model', 'Serial #', 'SAP Batch', 
                       'Product Family', 'Original Qty', 'Current Qty', 'Final Confirmed Qty', 
                       'Production Order', 'Production Order Type', 'SWR', 'Work Cell', 'Task List', 
                       'Task List Desc', 'Task List Rev', 'Task Item', 'Data Point Name', 
                       'Data Point Desc', 'Data Point  Value', 'High Limit', 'Low Limit', 'Result', 
                       'Submitter', 'Data Collection Date & Time')
  
  file_list <- list.files(directory, pattern = "\\.csv") #lists all the files in the directory
  
  directory_count <- 1 #count for going through the files in the directory
  
  while (directory_count < length(file_list) + 1){
    file_name <- file_list[directory_count]
    #print(paste0("Running File: ", file_name))
    
    if (grepl("-2", file_name, ignore.case = TRUE)){
      #if the sheet was first downloaded as an excel file it may have two csv files associated to it
      #because two excel tabs were created to fit the data
      #the second tab will not have a header
      data <- read.csv(paste0(directory, "/",file_name), header = FALSE, skip = 4, stringsAsFactors = FALSE)
    }
    else{
      #regular csv saved from an excel file
      data <- read.csv(paste0(directory, "/",file_name), header = FALSE, skip = 4, stringsAsFactors = FALSE)
    }
    
    output <- rbind(output, data)
    
    directory_count <- directory_count + 1
  }# end while of running through the folder/directory
  
  colnames(output) <- output_colnames
  
  return(output)
  
}# end aggregateTariData



generateTariList <- function(data){
  #'this will analyze the Tari data, regardless of the type of extrusion and return a list of 4
  #'elements: parameters, time stamps, submitters, and a total data frame of all those three previous
  #'combined
  #'
  
  #Upfront, I will convert the timestamps
  data$'Data Collection Date & Time' <- strptime(data$'Data Collection Date & Time', "%m/%d/%Y %H:%M:%OS %p")
  
  
  datalist <- list()
  
  datapointnames <- unique(data$'Data Point Name')
  datapointnameslength <- length(datapointnames)
  partnumbers <- unique(data$'Material')
  partnumbers <- partnumbers[which(partnumbers != "")]
  partnumberslength <- length(partnumbers)
  batches <- unique(data$'SAP Batch')
  batcheslength <- length(batches)
  
  
  
  extra_names <- c("Material Number", "SAP Batch Number", "SWR Number", "Start Operator ID", "Line",
                  "Start Date", "Start Date and Time", "Container")
  
  parameterdf <- data.frame(matrix(ncol = (8 + datapointnameslength), nrow = batcheslength), 
                            stringsAsFactors = FALSE)
  colnames(parameterdf) <- c(extra_names, datapointnames)
  
  timedf <- data.frame(matrix(ncol = (8 + datapointnameslength), nrow = batcheslength), 
                            stringsAsFactors = FALSE)
  colnames(timedf) <- c(extra_names, datapointnames)
  
  submitterdf <- data.frame(matrix(ncol = (8 + datapointnameslength), nrow = batcheslength), 
                            stringsAsFactors = FALSE)
  colnames(submitterdf) <- c(extra_names, datapointnames)
  
  totaldf <- data.frame(matrix(ncol = (8 + datapointnameslength), nrow = (3*batcheslength)), 
                            stringsAsFactors = FALSE)
  colnames(totaldf) <- c(extra_names, datapointnames)
  
  
  part_count <- 1
  overallbatch_count <- 1
  
  while (part_count < partnumberslength + 1){
    part <- partnumbers[part_count]
    #print(part)
    
    part_data <- data[which(data$'Material' == part),] #gets the information for the part
    partbatches <- unique(part_data$'SAP Batch')
    partbatcheslength <- length(partbatches)
    
    partbatch_count <- 1
    
    while (partbatch_count < partbatcheslength + 1){
      
      partbatch <- partbatches[partbatch_count]
      #print(partbatch)
      
      partbatchdata <- part_data[which(part_data$'SAP Batch' == partbatch),]
      
      #'the next many lines will be the first seven columns (Part Number, SAP Batch Numeber, 
      #'SWR Number, Start Operator ID, Line Number, Start Date, Start Date and Time, and Container)
      
      #getting the workcell name. If there are multiple names, it will ignore "Offline"
      workcell_namesvector <- unique(partbatchdata$'Work Cell')
      if (length(grep("offline", workcell_namesvector, ignore.case = TRUE)) > 0 &&
          length(grep("offline", workcell_namesvector, ignore.case = TRUE)) > 1){
        #if Offline is a workcell for the batch and there is more than one workcell for the batch
        offline_index <- grep("offline", workcell_namesvector, ignore.case = TRUE)
        non_offline_indices <- setdiff(c(1:length(workcell_namesvector)), offline_index)
        workcell_namesvector2 <- workcell_namesvector[non_offline_indices]
        workcell_name <- workcell_namesvector2[1]
      }
      else{
        workcell_name <- workcell_namesvector[1]
      }
      
      starttime <- min(partbatchdata$'Data Collection Date & Time')
      
      general_data <- c(part, 
                        partbatch, 
                        partbatchdata$'SWR'[1], 
                        partbatchdata$'Submitter'[1],
                        workcell_name, 
                        as.character(as.Date(starttime)), 
                        as.character(starttime), 
                        partbatchdata$'Container'[1])
      
      
      parameterdf[overallbatch_count, 1:8] <- general_data
      submitterdf[overallbatch_count, 1:8] <- general_data
      timedf[overallbatch_count, 1:8] <- general_data
      
      totaldf[(3*overallbatch_count) - 2, 1:8] <- general_data
      totaldf[(3*overallbatch_count) - 1, 1:8] <- general_data
      totaldf[(3*overallbatch_count), 1:8] <- general_data
      
      
      
      #Now I will analyze the data point values
      batch_datapointnames <- unique(partbatchdata$'Data Point Name')
      batch_datapointnameslength <- length(batch_datapointnames)
      batch_datapointname_count <- 1
      
      while (batch_datapointname_count < batch_datapointnameslength + 1){
        #loops through the all the unique names of the data points
        
        current_datapointname <- batch_datapointnames[batch_datapointname_count]
        

        search_name <- current_datapointname
        
        if (length(grep("\\(", search_name)) > 0){
          #if there are parentheses in the name
          #if there is one, it should have two
          
          search_name <- gsub("\\(", "\\\\(", search_name)
          search_name <- gsub("\\)", "\\\\)", search_name)
          
        }
        
        if (length(grep("\\[", search_name)) > 0){
          #if there are parentheses in the name
          #if there is one, it should have two
          
          search_name <- gsub("\\[", "\\\\[", search_name)
          search_name <- gsub("\\]", "\\\\]", search_name)
          
        }
        
        search_name_matches <- grep(search_name, partbatchdata$'Data Point Name')
        
        if (length(search_name_matches) > 1){
          search_name_matches <- search_name_matches[1]
        }
        
        #print(current_datapointname)
        #print(grep(search_name, partbatchdata$'Data Point Name'))
        
        parameterdf[overallbatch_count, current_datapointname] <- partbatchdata$'Data Point  Value'[search_name_matches]
        submitterdf[overallbatch_count, current_datapointname] <- partbatchdata$"Submitter"[search_name_matches]
        timedf[overallbatch_count, current_datapointname] <- as.character(partbatchdata$"Data Collection Date & Time"[search_name_matches])
        
        
        totaldf[(3*overallbatch_count) - 2, current_datapointname] <- parameterdf[overallbatch_count, current_datapointname]
        totaldf[(3*overallbatch_count), current_datapointname] <- submitterdf[overallbatch_count, current_datapointname]
        totaldf[(3*overallbatch_count) - 1, current_datapointname] <- timedf[overallbatch_count, current_datapointname]
        
        batch_datapointname_count <- batch_datapointname_count + 1
      }#end while for datapointname
      
      
      partbatch_count <- partbatch_count + 1
      overallbatch_count <- overallbatch_count + 1
    }#end while for partbatch
    
    
    part_count <- part_count + 1
  }#end while for part numbers
  
  
  parameterdf <- parameterdf[which(!is.na(parameterdf$'SAP Batch')),]
  timedf <- timedf[which(!is.na(timedf$'SAP Batch')),]
  submitterdf <- submitterdf[which(!is.na(submitterdf$'SAP Batch')),]
  totaldf <- totaldf[which(!is.na(totaldf$'SAP Batch')),]
  
  datalist$parameters <- parameterdf
  datalist$time <- timedf
  datalist$submitter <- submitterdf
  datalist$total <- totaldf
  
  return(datalist)
  
} #end generateTariList






