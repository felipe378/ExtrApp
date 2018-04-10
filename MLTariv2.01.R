#This code cleans up the data from the "Task and Resource Information" report from MES.

require(xlsx)
require(readxl)
require(purrr)

aggregateTariData <- function(directory){
  #this functions goes through the folder, searches for CSV files and concatenates by rows all the
  #data
  
  output <- data.frame() #initializes an empty data frame
  output_colnames <- c('Container', 'Parent Container', 'Container Status', 'Material', 
                       'Material Desc', 'Material Rev', 'Model', 'Serial #', 'SAP Batch', 
                       'Product Family', 'Original Qty', 'Current Qty', 'Final Confirmed Qty', 
                       'Production Order', 'Production Order Type', 'SWR', 'Work Cell', 'Task List', 
                       'Task List Desc', 'Task List Rev', 'Task Item', 'Data Point Name', 
                       'Data Point Desc', 'Data Point  Value', 'High Limit', 'Low Limit', 'Result', 
                       'Submitter', 'Data Collection Date & Time')
  
  setwd(directory) # sets the working directory to the directory. This way the files can be 
  #accessed by just their respective filenames and not a full workingpath
  
  file_list <- list.files(directory, pattern = "\\.csv") #lists all the files in the directory
  
  directory_count <- 1 #count for going through the files in the directory
  
  while (directory_count < length(file_list) + 1){
    file_name <- file_list[directory_count]
    print(paste0("Running File: ", file_name))
    
    if (grepl("-2", file_name, ignore.case = TRUE)){
      #if the sheet was first downloaded as an excel file it may have two csv files associated to it
      #because two excel tabs were created to fit the data
      #the second tab will not have a header
      data <- read.csv(file_name, header = FALSE, skip = 4, stringsAsFactors = FALSE)
    }
    else{
      #regular csv saved from an excel file
      data <- read.csv(file_name, header = FALSE, skip = 4, stringsAsFactors = FALSE)
    }
    
    output <- rbind(output, data)
    
    directory_count <- directory_count + 1
  }# end while of running through the folder/directory
  
  colnames(output) <- output_colnames
  
  return(output)
  
}# end aggregateTariData



generateTariDataFrame <- function(data){
  #this does that same as generateTariList except it makes a dataframe instead of a list.
  
  test <- list() #this list will store three data frames. One is the run parameters, the other is
  #is the time stamps, and the last is the merging of the two
  
  
  df_rowlength <- length(unique(data$'SAP Batch')) #the nrow of the df is the number of unique
  #SAP batches
  
  column_names <- c("Material Number","SAP Batch Number", "SWR Number", "Operator ID", "Line", "Start Time",
                    "Drying Temperature (F)", "Drying Time (h)",
                    
                    "Extruder 1 Feedthroat Temp",
                    "Ext 1 Barrel 1 Temp", "Ext 1 Barrel 2 Temp", "Ext 1 Barrel 3 Temp",
                    "Ext 1 Clamp Temp", "Ext 1 Filter Temp", "Ext 1 Adapter Temp",
                    "Ext 1 Melt Pump Temp", "Ext 1 Melt Pump Clamp Temp",
                    
                    "Extruder 2 Feedthroat Temp",
                    "Ext 2 Barrel 1 Temp", "Ext 2 Barrel 2 Temp", "Ext 2 Barrel 3 Temp",
                    "Ext 2 Clamp Temp", "Ext 2 Filter Temp", "Ext 2 Adapter Temp",
                    "Ext 2 Melt Pump Temp", "Ext 2 Melt Pump Clamp Temp",
                    
                    
                    "Extruder 3 Feedthroat Temp", 
                    "Ext 3 Barrel 1 Temp", "Ext 3 Barrel 2 Temp", "Ext 3 Barrel 3 Temp",
                    "Ext 3 Clamp Temp", "Ext 3 Filter Temp", "Ext 3 Adapter Temp",
                    "Ext 3 Melt Pump Temp", "Ext 3 Melt Pump Clamp Temp",
                    
                    "Die 1 Temp", "Die 2 Temp",
                    
                    "Ext 1 Screw Speed", "Ext 2 Screw Speed", "Ext 3 Screw Speed",
                    
                    "Ext 1 Barrel Pressure", "Ext 1 Adapter Pressure", "Ext 1 MP Outlet Pressure",
                    "Ext 2 Barrel Pressure", "Ext 2 Adapter Pressure", "Ext 2 MP Outlet Pressure",
                    "Ext 3 Barrel Pressure", "Ext 3 Adapter Pressure", "Ext 3 MP Outlet Pressure",
                    
                    "Puller Speed")
  
  time_column_names <- c("Material Number","SAP Batch Number", "SWR Number", "Operator ID", "Line", "Start Time",
                         "Drying Temperature (F) Time", "Drying Time (h) Time",
                         
                         "Extruder 1 Feedthroat Temp Time",
                         "Ext 1 Barrel 1 Temp Time", "Ext 1 Barrel 2 Temp Time", "Ext 1 Barrel 3 Temp Time",
                         "Ext 1 Clamp Temp Time", "Ext 1 Filter Temp Time", "Ext 1 Adapter Temp Time",
                         "Ext 1 Melt Pump Temp Time", "Ext 1 Melt Pump Clamp Temp Time",
                         "Extruder 2 Feedthroat Temp Time",
                         "Ext 2 Barrel 1 Temp Time", "Ext 2 Barrel 2 Temp Time", "Ext 2 Barrel 3 Temp Time",
                         "Ext 2 Clamp Temp Time", "Ext 2 Filter Temp Time", "Ext 2 Adapter Temp Time",
                         "Ext 2 Melt Pump Temp Time", "Ext 2 Melt Pump Clamp Temp Time",
                         
                         
                         "Extruder 3 Feedthroat Temp Time", 
                         "Ext 3 Barrel 1 Temp Time", "Ext 3 Barrel 2 Temp Time", "Ext 3 Barrel 3 Temp Time",
                         "Ext 3 Clamp Temp Time", "Ext 3 Filter Temp Time", "Ext 3 Adapter Temp Time",
                         "Ext 3 Melt Pump Temp Time", "Ext 3 Melt Pump Clamp Temp Time",
                         
                         "Die 1 Temp Time", "Die 2 Temp Time",
                         
                         "Ext 1 Screw Speed Time", "Ext 2 Screw Speed Time", "Ext 3 Screw Speed Time",
                         
                         "Ext 1 Barrel Pressure Time", "Ext 1 Adapter Pressure Time", "Ext 1 MP Outlet Pressure Time",
                         "Ext 2 Barrel Pressure Time", "Ext 2 Adapter Pressure Time", "Ext 2 MP Outlet Pressure Time",
                         "Ext 3 Barrel Pressure Time", "Ext 3 Adapter Pressure Time", "Ext 3 MP Outlet Pressure Time",
                         
                         "Puller Speed Time")
  
  submit_column_names <- c("Material Number","SAP Batch Number", "SWR Number", "Operator ID", "Line", "Start Time",
                           "Drying Temperature (F) Submitter", "Drying Submitter (h) Submitter",
                           
                           "Extruder 1 Feedthroat Temp Submitter",
                           "Ext 1 Barrel 1 Temp Submitter", "Ext 1 Barrel 2 Temp Submitter", "Ext 1 Barrel 3 Temp Submitter",
                           "Ext 1 Clamp Temp Submitter", "Ext 1 Filter Temp Submitter", "Ext 1 Adapter Temp Submitter",
                           "Ext 1 Melt Pump Temp Submitter", "Ext 1 Melt Pump Clamp Temp Submitter",
                           "Extruder 2 Feedthroat Temp Submitter",
                           "Ext 2 Barrel 1 Temp Submitter", "Ext 2 Barrel 2 Temp Submitter", "Ext 2 Barrel 3 Temp Submitter",
                           "Ext 2 Clamp Temp Submitter", "Ext 2 Filter Temp Submitter", "Ext 2 Adapter Temp Submitter",
                           "Ext 2 Melt Pump Temp Submitter", "Ext 2 Melt Pump Clamp Temp Submitter",
                           
                           
                           "Extruder 3 Feedthroat Temp Submitter", 
                           "Ext 3 Barrel 1 Temp Submitter", "Ext 3 Barrel 2 Temp Submitter", "Ext 3 Barrel 3 Temp Submitter",
                           "Ext 3 Clamp Temp Submitter", "Ext 3 Filter Temp Submitter", "Ext 3 Adapter Temp Submitter",
                           "Ext 3 Melt Pump Temp Submitter", "Ext 3 Melt Pump Clamp Temp Submitter",
                           
                           "Die 1 Temp Submitter", "Die 2 Temp Submitter",
                           
                           "Ext 1 Screw Speed Submitter", "Ext 2 Screw Speed Submitter", "Ext 3 Screw Speed Submitter",
                           
                           "Ext 1 Barrel Pressure Submitter", "Ext 1 Adapter Pressure Submitter", "Ext 1 MP Outlet Pressure Submitter",
                           "Ext 2 Barrel Pressure Submitter", "Ext 2 Adapter Pressure Submitter", "Ext 2 MP Outlet Pressure Submitter",
                           "Ext 3 Barrel Pressure Submitter", "Ext 3 Adapter Pressure Submitter", "Ext 3 MP Outlet Pressure Submitter",
                           
                           "Puller Speed Submitter")
  
  
 
  
  df_columnlength <- length(column_names)
  
  output_df <- data.frame(matrix(ncol = df_columnlength, nrow = df_rowlength), stringsAsFactors = F)
  time_df <- data.frame(matrix(ncol = df_columnlength, nrow = df_rowlength), stringsAsFactors = F)
  submit_df <- data.frame(matrix(ncol = df_columnlength, nrow = df_rowlength), stringsAsFactors = F)
  total_df <- data.frame(matrix(ncol = df_columnlength, nrow = (3*df_rowlength)), stringsAsFactors = F)
  #the total_df includes both data
  
  
  #the dataframe is initialized
  names(output_df) <- column_names #name the columns
  names(time_df) <- time_column_names
  names(submit_df) <- submit_column_names
  names(total_df) <- column_names
  
  tari_material_list <- unique(data[,"Material"]) #list of unique materials
  
  tari_material_list_length <- length(tari_material_list)
  material_count <- 1 #goes through the materials
  overall_batch_count <- 1 #keeps track of all the batches that have been analyzed
  
  while (material_count < tari_material_list_length + 1){
    #this loops through the data frame and gets all the relevant information for a material
    
    current_material <- tari_material_list[material_count] #gets the current material
    
    sub_data <- data[(data[,"Material"] == current_material),] #gets all the rows for the material
    batch_list <- unique(sub_data[,"SAP Batch"])
    #gets the unique batch numbers
    
    batch_list_length <- length(batch_list)
    #initializes a data frame to store the information for all the batches
    #this data frame will then be assigned to the material element in the material list.
    batch_count <- 1 #this loops through all the batches of a material
    
    while(batch_count < batch_list_length + 1){
      
      current_batch <- batch_list[batch_count] #gets the current batch
      sub_data_batch <- sub_data[(sub_data[,"SAP Batch"] == current_batch),] #get the data for the
      #current batch
      
      # Same for three layers
      sap_batch <- getSAPBatch(sub_data_batch)
      swr_number <- getSWRNumber(sub_data_batch)
      operator_id <- getOperatorID(sub_data_batch)
      line <- getLine(sub_data_batch)
      start_time <- getStartTime(sub_data_batch)
      
      
      
      drying_temperature_parameter <- getDryingTemperature(sub_data_batch)$parameter
      drying_time_parameter <- getDryingTime(sub_data_batch)$parameter
      # Ext1_
      Ext1_feedthroat_temp_parameter <- getValue(sub_data_batch,"Ext 1 Feedthroat Temp")$parameter
      Ext1_barrelzone1_temp_parameter <- getValue(sub_data_batch,"Ext 1 Barrel 1 Temp")$parameter
      Ext1_barrelzone2_temp_parameter <- getValue(sub_data_batch,"Ext 1 Barrel 2 Temp")$parameter
      Ext1_barrelzone3_temp_parameter<- getValue(sub_data_batch,"Ext 1 Barrel 3 Temp")$parameter
      Ext1_clamp_temp_parameter <- getValue(sub_data_batch,"Ext 1 Clamp Temp")$parameter
      Ext1_filter_temp_parameter <-getValue(sub_data_batch,"Ext 1 Filter Temp")$parameter
      Ext1_adapter_temp_parameter <- getValue(sub_data_batch,"Ext 1 Adapter Temp")$parameter
      Ext1_meltpump_temp_parameter <-getValue(sub_data_batch,"Ext 1 Melt Pump Temp")$parameter
      Ext1_meltpumpclamp_temp_parameter<-getValue(sub_data_batch,"Ext 1 Melt Pump Clamp Temp")$parameter
      Ext1_barrel_pressure_parameter <- getValue(sub_data_batch,"Ext 1 Barrel Pressure")$parameter
      Ext1_adapter_pressure_parameter<-getValue(sub_data_batch,"Ext 1 Adapter Pressure")$parameter
      Ext1_mpoutlet_pressure_parameter<-getValue(sub_data_batch,"Ext 1 MP Outlet Pressure")$parameter
      Ext1_screw_speed_parameter <- getValue(sub_data_batch,"Ext 1 Screw Speed")$parameter
      
      
      
      # Ext2
      Ext2_feedthroat_temp_parameter <- getValue(sub_data_batch,"Ext 2 Feedthroat Temp")$parameter
      Ext2_barrelzone1_temp_parameter <- getValue(sub_data_batch,"Ext 2 Barrel 1 Temp")$parameter
      Ext2_barrelzone2_temp_parameter <- getValue(sub_data_batch,"Ext 2 Barrel 2 Temp")$parameter
      Ext2_barrelzone3_temp_parameter <- getValue(sub_data_batch,"Ext 2 Barrel 3 Temp")$parameter
      Ext2_clamp_temp_parameter <- getValue(sub_data_batch,"Ext 2 Clamp Temp")$parameter
      Ext2_filter_temp_parameter<-getValue(sub_data_batch,"Ext 2 Filter Temp")$parameter
      Ext2_adapter_temp_parameter <- getValue(sub_data_batch,"Ext 2 Adapter Temp")$parameter
      Ext2_meltpump_temp_parameter<-getValue(sub_data_batch,"Ext 2 Melt Pump Temp")$parameter
      Ext2_meltpumpclamp_temp_parameter<-getValue(sub_data_batch,"Ext 2 Melt Pump Clamp Temp")$parameter
      Ext2_barrel_pressure_parameter <- getValue(sub_data_batch,"Ext 2 Barrel Pressure")$parameter
      Ext2_adapter_pressure_parameter<-getValue(sub_data_batch,"Ext 2 Adapter Pressure")$parameter
      Ext2_mpoutlet_pressure_parameter<-getValue(sub_data_batch,"Ext 2 MP Outlet Pressure")$parameter
      Ext2_screw_speed_parameter <- getValue(sub_data_batch,"Ext 2 Screw Speed")$parameter
      
      
      
      # Ext3
      Ext3_feedthroat_temp_parameter <- getValue(sub_data_batch,"Ext 3 Feedthroat Temp")$parameter
      Ext3_barrelzone1_temp_parameter <- getValue(sub_data_batch,"Ext 3 Barrel 1 Temp")$parameter
      Ext3_barrelzone2_temp_parameter <- getValue(sub_data_batch,"Ext 3 Barrel 2 Temp")$parameter
      Ext3_barrelzone3_temp_parameter <- getValue(sub_data_batch,"Ext 3 Barrel 3 Temp")$parameter
      Ext3_clamp_temp_parameter <- getValue(sub_data_batch,"Ext 3 Clamp Temp")$parameter
      Ext3_filter_temp_parameter<-getValue(sub_data_batch,"Ext 3 Filter Temp")$parameter
      Ext3_adapter_temp_parameter <- getValue(sub_data_batch,"Ext 3 Adapter Temp")$parameter
      Ext3_meltpump_temp_parameter<-getValue(sub_data_batch,"Ext 3 Melt Pump Temp")$parameter
      Ext3_meltpumpclamp_temp_parameter<-getValue(sub_data_batch,"Ext 3 Melt Pump Clamp Temp")$parameter
      Ext3_barrel_pressure_parameter <- getValue(sub_data_batch,"Ext 3 Barrel Pressure")$parameter
      Ext3_adapter_pressure_parameter<-getValue(sub_data_batch,"Ext 3 Adapter Pressure")$parameter
      Ext3_mpoutlet_pressure_parameter<-getValue(sub_data_batch,"Ext 3 MP Outlet Pressure")$parameter
      Ext3_screw_speed_parameter <- getValue(sub_data_batch,"Ext 3 Screw Speed")$parameter
      
      #same for three layers
      die1_temp_parameter <- getValue(sub_data_batch,"Die 1 Temp")$parameter
      die2_temp_parameter <- getValue(sub_data_batch,"Die 2 Temp")$parameter
      puller_speed_parameter <- getValue(sub_data_batch,"Puller Speed")$parameter
      
      
      
      
      #store all values
      parameter_data_vector<-c(current_material,sap_batch,swr_number,operator_id,line,start_time,drying_temperature_parameter,drying_time_parameter,
                               Ext1_feedthroat_temp_parameter,Ext1_barrelzone1_temp_parameter,Ext1_barrelzone2_temp_parameter,Ext1_barrelzone3_temp_parameter,
                               Ext1_clamp_temp_parameter,Ext1_filter_temp_parameter,Ext1_adapter_temp_parameter,Ext1_meltpump_temp_parameter,Ext1_meltpumpclamp_temp_parameter,
                               Ext2_feedthroat_temp_parameter,Ext2_barrelzone1_temp_parameter,Ext2_barrelzone2_temp_parameter,Ext2_barrelzone3_temp_parameter,
                               Ext2_clamp_temp_parameter,Ext2_filter_temp_parameter,Ext2_adapter_temp_parameter,Ext2_meltpump_temp_parameter,Ext2_meltpumpclamp_temp_parameter,
                               Ext3_feedthroat_temp_parameter,Ext3_barrelzone1_temp_parameter,Ext3_barrelzone2_temp_parameter,Ext3_barrelzone3_temp_parameter ,
                               Ext3_clamp_temp_parameter, Ext3_filter_temp_parameter,Ext3_adapter_temp_parameter ,Ext3_meltpump_temp_parameter,Ext3_meltpumpclamp_temp_parameter,
                               die1_temp_parameter,die2_temp_parameter,
                               Ext1_screw_speed_parameter,Ext2_screw_speed_parameter,Ext3_screw_speed_parameter,
                               Ext1_barrel_pressure_parameter ,Ext1_adapter_pressure_parameter,Ext1_mpoutlet_pressure_parameter,
                               Ext2_barrel_pressure_parameter ,Ext2_adapter_pressure_parameter,Ext2_mpoutlet_pressure_parameter,
                               Ext3_barrel_pressure_parameter ,Ext3_adapter_pressure_parameter,Ext3_mpoutlet_pressure_parameter,
                               puller_speed_parameter) #assign parameters to a vector
      
      output_df[overall_batch_count, ] <- parameter_data_vector
      
      
      
      
      
      #Time Parameters#
      drying_temperature_time <- getDryingTemperature(sub_data_batch)$time
      drying_time_time <- getDryingTime(sub_data_batch)$time
      # Ext1_
      Ext1_feedthroat_temp_time <- getValue(sub_data_batch,"Ext 1 Feedthroat Temp")$time
      Ext1_barrelzone1_temp_time <- getValue(sub_data_batch,"Ext 1 Barrel 1 Temp")$time
      Ext1_barrelzone2_temp_time <- getValue(sub_data_batch,"Ext 1 Barrel 2 Temp")$time
      Ext1_barrelzone3_temp_time<- getValue(sub_data_batch,"Ext 1 Barrel 3 Temp")$time
      Ext1_clamp_temp_time <- getValue(sub_data_batch,"Ext 1 Clamp Temp")$time
      Ext1_filter_temp_time <-getValue(sub_data_batch,"Ext 1 Filter Temp")$time
      Ext1_adapter_temp_time <- getValue(sub_data_batch,"Ext 1 Adapter Temp")$time
      Ext1_meltpump_temp_time <-getValue(sub_data_batch,"Ext 1 Melt Pump Temp")$time
      Ext1_meltpumpclamp_temp_time<-getValue(sub_data_batch,"Ext 1 Melt Pump Clamp Temp")$time
      Ext1_barrel_pressure_time <- getValue(sub_data_batch,"Ext 1 Barrel Pressure")$time
      Ext1_adapter_pressure_time<-getValue(sub_data_batch,"Ext 1 Adapter Pressure")$time
      Ext1_mpoutlet_pressure_time<-getValue(sub_data_batch,"Ext 1 MP Outlet Pressure")$time
      Ext1_screw_speed_time <- getValue(sub_data_batch,"Ext 1 Screw Speed")$time
      
      
      
      # Ext2
      Ext2_feedthroat_temp_time <- getValue(sub_data_batch,"Ext 2 Feedthroat Temp")$time
      Ext2_barrelzone1_temp_time <- getValue(sub_data_batch,"Ext 2 Barrel 1 Temp")$time
      Ext2_barrelzone2_temp_time <- getValue(sub_data_batch,"Ext 2 Barrel 2 Temp")$time
      Ext2_barrelzone3_temp_time <- getValue(sub_data_batch,"Ext 2 Barrel 3 Temp")$time
      Ext2_clamp_temp_time <- getValue(sub_data_batch,"Ext 2 Clamp Temp")$time
      Ext2_filter_temp_time<-getValue(sub_data_batch,"Ext 2 Filter Temp")$time
      Ext2_adapter_temp_time <- getValue(sub_data_batch,"Ext 2 Adapter Temp")$time
      Ext2_meltpump_temp_time<-getValue(sub_data_batch,"Ext 2 Melt Pump Temp")$time
      Ext2_meltpumpclamp_temp_time<-getValue(sub_data_batch,"Ext 2 Melt Pump Clamp Temp")$time
      Ext2_barrel_pressure_time <- getValue(sub_data_batch,"Ext 2 Barrel Pressure")$time
      Ext2_adapter_pressure_time<-getValue(sub_data_batch,"Ext 2 Adapter Pressure")$time
      Ext2_mpoutlet_pressure_time<-getValue(sub_data_batch,"Ext 2 MP Outlet Pressure")$time
      Ext2_screw_speed_time <- getValue(sub_data_batch,"Ext 2 Screw Speed")$time
      
      
      # Ext3
      Ext3_feedthroat_temp_time <- getValue(sub_data_batch,"Ext 3 Feedthroat Temp")$time
      Ext3_barrelzone1_temp_time <- getValue(sub_data_batch,"Ext 3 Barrel 1 Temp")$time
      Ext3_barrelzone2_temp_time <- getValue(sub_data_batch,"Ext 3 Barrel 2 Temp")$time
      Ext3_barrelzone3_temp_time <- getValue(sub_data_batch,"Ext 3 Barrel 3 Temp")$time
      Ext3_clamp_temp_time <- getValue(sub_data_batch,"Ext 3 Clamp Temp")$time
      Ext3_filter_temp_time<-getValue(sub_data_batch,"Ext 3 Filter Temp")$time
      Ext3_adapter_temp_time <- getValue(sub_data_batch,"Ext 3 Adapter Temp")$time
      Ext3_meltpump_temp_time<-getValue(sub_data_batch,"Ext 3 Melt Pump Temp")$time
      Ext3_meltpumpclamp_temp_time<-getValue(sub_data_batch,"Ext 3 Melt Pump Clamp Temp")$time
      Ext3_barrel_pressure_time <- getValue(sub_data_batch,"Ext 3 Barrel Pressure")$time
      Ext3_adapter_pressure_time<-getValue(sub_data_batch,"Ext 3 Adapter Pressure")$time
      Ext3_mpoutlet_pressure_time<-getValue(sub_data_batch,"Ext 3 MP Outlet Pressure")$time
      Ext3_screw_speed_time <- getValue(sub_data_batch,"Ext 3 Screw Speed")$time
      
      #same for three layers
      die1_temp_time <- getValue(sub_data_batch,"Die 1 Temp")$time
      die2_temp_time <- getValue(sub_data_batch,"Die 2 Temp")$time
      puller_speed_time <- getValue(sub_data_batch,"Puller Speed")$time
      
      
      
      
      
      time_data_vector<-c(current_material,sap_batch,swr_number,operator_id,line,start_time,drying_temperature_time,drying_time_time,
                          Ext1_feedthroat_temp_time,Ext1_barrelzone1_temp_time,Ext1_barrelzone2_temp_time,Ext1_barrelzone3_temp_time,
                          Ext1_clamp_temp_time,Ext1_filter_temp_time,Ext1_adapter_temp_time,Ext1_meltpump_temp_time,Ext1_meltpumpclamp_temp_time,
                          Ext2_feedthroat_temp_time,Ext2_barrelzone1_temp_time,Ext2_barrelzone2_temp_time,Ext2_barrelzone3_temp_time,
                          Ext2_clamp_temp_time,Ext2_filter_temp_time,Ext2_adapter_temp_time,Ext2_meltpump_temp_time,Ext2_meltpumpclamp_temp_time,
                          Ext3_feedthroat_temp_time,Ext3_barrelzone1_temp_time,Ext3_barrelzone2_temp_time ,Ext3_barrelzone3_temp_time ,
                          Ext3_clamp_temp_time, Ext3_filter_temp_time,Ext3_adapter_temp_time ,Ext3_meltpump_temp_time,Ext3_meltpumpclamp_temp_time,
                          die1_temp_time,die2_temp_time,
                          Ext1_screw_speed_time,Ext2_screw_speed_time,Ext3_screw_speed_time,
                          Ext1_barrel_pressure_time ,Ext1_adapter_pressure_time,Ext1_mpoutlet_pressure_time,
                          Ext2_barrel_pressure_time ,Ext2_adapter_pressure_time,Ext2_mpoutlet_pressure_time,
                          Ext3_barrel_pressure_time ,Ext3_adapter_pressure_time,Ext3_mpoutlet_pressure_time,
                          puller_speed_time)
      
      time_df[overall_batch_count, ] <- time_data_vector
      
      
      #Submitter#
      drying_temperature_submit <- getDryingTemperature(sub_data_batch)$submit
      drying_time_submit <- getDryingTime(sub_data_batch)$submit
      # Ext1_
      Ext1_feedthroat_temp_submit <- getValue(sub_data_batch,"Ext 1 Feedthroat Temp")$submit
      Ext1_barrelzone1_temp_submit <- getValue(sub_data_batch,"Ext 1 Barrel 1 Temp")$submit
      Ext1_barrelzone2_temp_submit <- getValue(sub_data_batch,"Ext 1 Barrel 2 Temp")$submit
      Ext1_barrelzone3_temp_submit<- getValue(sub_data_batch,"Ext 1 Barrel 3 Temp")$submit
      Ext1_clamp_temp_submit <- getValue(sub_data_batch,"Ext 1 Clamp Temp")$submit
      Ext1_filter_temp_submit <-getValue(sub_data_batch,"Ext 1 Filter Temp")$submit
      Ext1_adapter_temp_submit <- getValue(sub_data_batch,"Ext 1 Adapter Temp")$submit
      Ext1_meltpump_temp_submit <-getValue(sub_data_batch,"Ext 1 Melt Pump Temp")$submit
      Ext1_meltpumpclamp_temp_submit<-getValue(sub_data_batch,"Ext 1 Melt Pump Clamp Temp")$submit
      Ext1_barrel_pressure_submit <- getValue(sub_data_batch,"Ext 1 Barrel Pressure")$submit
      Ext1_adapter_pressure_submit<-getValue(sub_data_batch,"Ext 1 Adapter Pressure")$submit
      Ext1_mpoutlet_pressure_submit<-getValue(sub_data_batch,"Ext 1 MP Outlet Pressure")$submit
      Ext1_screw_speed_submit <- getValue(sub_data_batch,"Ext 1 Screw Speed")$submit
      
      
      
      # Ext2
      Ext2_feedthroat_temp_submit <- getValue(sub_data_batch,"Ext 2 Feedthroat Temp")$submit
      Ext2_barrelzone1_temp_submit <- getValue(sub_data_batch,"Ext 2 Barrel 1 Temp")$submit
      Ext2_barrelzone2_temp_submit <- getValue(sub_data_batch,"Ext 2 Barrel 2 Temp")$submit
      Ext2_barrelzone3_temp_submit <- getValue(sub_data_batch,"Ext 2 Barrel 3 Temp")$submit
      Ext2_clamp_temp_submit <- getValue(sub_data_batch,"Ext 2 Clamp Temp")$submit
      Ext2_filter_temp_submit<-getValue(sub_data_batch,"Ext 2 Filter Temp")$submit
      Ext2_adapter_temp_submit <- getValue(sub_data_batch,"Ext 2 Adapter Temp")$submit
      Ext2_meltpump_temp_submit<-getValue(sub_data_batch,"Ext 2 Melt Pump Temp")$submit
      Ext2_meltpumpclamp_temp_submit<-getValue(sub_data_batch,"Ext 2 Melt Pump Clamp Temp")$submit
      Ext2_barrel_pressure_submit <- getValue(sub_data_batch,"Ext 2 Barrel Pressure")$submit
      Ext2_adapter_pressure_submit<-getValue(sub_data_batch,"Ext 2 Adapter Pressure")$submit
      Ext2_mpoutlet_pressure_submit<-getValue(sub_data_batch,"Ext 2 MP Outlet Pressure")$submit
      Ext2_screw_speed_submit <- getValue(sub_data_batch,"Ext 2 Screw Speed")$submit
      
      
      # Ext3
      Ext3_feedthroat_temp_submit <- getValue(sub_data_batch,"Ext 3 Feedthroat Temp")$submit
      Ext3_barrelzone1_temp_submit <- getValue(sub_data_batch,"Ext 3 Barrel 1 Temp")$submit
      Ext3_barrelzone2_temp_submit <- getValue(sub_data_batch,"Ext 3 Barrel 2 Temp")$submit
      Ext3_barrelzone3_temp_submit <- getValue(sub_data_batch,"Ext 3 Barrel 3 Temp")$submit
      Ext3_clamp_temp_submit <- getValue(sub_data_batch,"Ext 3 Clamp Temp")$submit
      Ext3_filter_temp_submit<-getValue(sub_data_batch,"Ext 3 Filter Temp")$submit
      Ext3_adapter_temp_submit <- getValue(sub_data_batch,"Ext 3 Adapter Temp")$submit
      Ext3_meltpump_temp_submit<-getValue(sub_data_batch,"Ext 3 Melt Pump Temp")$submit
      Ext3_meltpumpclamp_temp_submit<-getValue(sub_data_batch,"Ext 3 Melt Pump Clamp Temp")$submit
      Ext3_barrel_pressure_submit <- getValue(sub_data_batch,"Ext 3 Barrel Pressure")$submit
      Ext3_adapter_pressure_submit<-getValue(sub_data_batch,"Ext 3 Adapter Pressure")$submit
      Ext3_mpoutlet_pressure_submit<-getValue(sub_data_batch,"Ext 3 MP Outlet Pressure")$submit
      Ext3_screw_speed_submit <- getValue(sub_data_batch,"Ext 3 Screw Speed")$submit
      
      #same for three layers
      die1_temp_submit <- getValue(sub_data_batch,"Die 1 Temp")$submit
      die2_temp_submit <- getValue(sub_data_batch,"Die 2 Temp")$submit
      puller_speed_submit <- getValue(sub_data_batch,"Puller Speed")$submit
      
      
      
      
      submit_data_vector<-c(current_material,sap_batch,swr_number,operator_id,line,start_time,drying_temperature_submit,drying_time_submit,
                            Ext1_feedthroat_temp_submit,Ext1_barrelzone1_temp_submit,Ext1_barrelzone2_temp_submit,Ext1_barrelzone3_temp_submit,
                            Ext1_clamp_temp_submit,Ext1_filter_temp_submit,Ext1_adapter_temp_submit,Ext1_meltpump_temp_submit,Ext1_meltpumpclamp_temp_submit,
                            Ext2_feedthroat_temp_submit,Ext2_barrelzone1_temp_submit,Ext2_barrelzone2_temp_submit,Ext2_barrelzone3_temp_submit,
                            Ext2_clamp_temp_submit,Ext2_filter_temp_submit,Ext2_adapter_temp_submit,Ext2_meltpump_temp_submit,Ext2_meltpumpclamp_temp_submit,
                            Ext3_feedthroat_temp_submit,Ext3_barrelzone1_temp_submit,Ext3_barrelzone2_temp_submit ,Ext3_barrelzone3_temp_submit ,
                            Ext3_clamp_temp_submit, Ext3_filter_temp_submit,Ext3_adapter_temp_submit ,Ext3_meltpump_temp_submit,Ext3_meltpumpclamp_temp_submit,
                            die1_temp_submit,die2_temp_submit,
                            Ext1_screw_speed_submit,Ext2_screw_speed_submit,Ext3_screw_speed_submit,
                            Ext1_barrel_pressure_submit ,Ext1_adapter_pressure_submit,Ext1_mpoutlet_pressure_submit,
                            Ext2_barrel_pressure_submit ,Ext2_adapter_pressure_submit,Ext2_mpoutlet_pressure_submit,
                            Ext3_barrel_pressure_submit ,Ext3_adapter_pressure_submit,Ext3_mpoutlet_pressure_submit,
                            puller_speed_submit)

      
      submit_df[overall_batch_count, ] <- submit_data_vector
      
      
      
      #total data
      total_df[(overall_batch_count*3),] <- parameter_data_vector
      total_df[(overall_batch_count*3) + 1,] <- time_data_vector
      total_df[(overall_batch_count*3) + 2,] <- submit_data_vector
      
      batch_count <- batch_count + 1 #update batch_count
      overall_batch_count <- overall_batch_count + 1
    }#end while that goes through the batches
    
    material_count <- material_count + 1 #update batch_count
  }#end while that loops through the materials
  
  
  total_df <- total_df[3:nrow(total_df),] #remove first row that is blank due to *2 multiplier
  
  converttime_count <- 6
  while (converttime_count < 21){
    time_df[,converttime_count] <- as.POSIXct(time_df[,converttime_count], format = '%m/%d/%Y %H:%M:%S %p')
    converttime_count <- converttime_count + 1
  }
  
  test$parameters <- output_df
  test$time <- time_df
  test$submit <- submit_df
  test$total <- total_df
  
  
  return(test)
  
  
}# end generateTariDataFrame


getSAPBatch <- function(data){
  #parses the batch data to get the SWR number
  #because the batch number will be in every row, it just grabs the first one
  
  sap_batch <- data[1,"SAP Batch"]
  return(sap_batch)
  
} #end getSAPBatch


getSWRNumber <- function(data){
  #parses the batch data to get the SWR number
  #because the batch number will be in every row, it just grabs the first one
  
  swr_number <- data[1,"SWR"]
  return(swr_number)
  
} #end getSWRNumber


getOperatorID <- function(data){
  #parses the batch data to get the Operator ID
  #because the operator ID will be in every row, it just grabs the first one
  
  operator_id <- data[1,"Submitter"]
  return(operator_id)
  
}#end getOperatorID


getLine <- function(data){
  #parses the batch data to get the line
  #because the line will be in every row, it just grabs the first one; however, it assumes that the 
  #line will be in the first task IT may have offline or other work cells as the first task
  
  line <- data[1,"Work Cell"]
  return(line)
  
}#end getLine


getStartTime <- function(data){
  #parses the batch data to get the start time for the first task
  
  start_time <- data[1,"Data Collection Date & Time"]
  return(start_time)
  
}#end getStartTime

getDryingTemperature <- function(data){
  #parses the batch data to get the drying temperature by searching for the task and then the
  #data point value. It is "Data Point  Value" with two spaces between point and value.
  
  
  row_index <- grep("Drying Temp Setpoint", data[, "Data Point Name"], ignore.case = TRUE)
  
  
  if (length(row_index) == 0){
    #if no match was found, return NA
    return (list(parameter = NA, time = NA, submit = NA))
  }
  else if (length(row_index) > 1){
    #if multiple matches, return the first
    row_index <- row_index[1]
  }
  
  
  parameter <- data[row_index, "Data Point  Value"]
  time <- data[row_index, "Data Collection Date & Time"]
  submit <- data[row_index, "Submitter"]
  
  
  
  output <- list() #to store the parameter and the time
  
  output$parameter <- parameter
  output$time <- time
  output$submit <- submit
  
  
  return(output)
  
}#end getDryingTemperature

getDryingTime <- function(data){
  #parses the batch data to get the drying time by searching for the task and then the
  #data point value. It is "Data Point  Value" with two spaces between point and value.
  
  row_index <- grep("Drying Time", data[, "Data Point Name"], ignore.case = TRUE)
  
  if (length(row_index) == 0){
    #if no match was found, check for another name for the task
    
    row_index <- grep("Verify Resin is Dryed", data[, "Data Point Name"], ignore.case = TRUE)
    
    if (length(row_index) == 0){
      #if still no match was found, return NA
      return (list(parameter = NA, time = NA, submit = NA))
    }
    else if (length(row_index) > 1){
      #if multiple matches, return the first
      row_index <- row_index[1]
    }
    
  }
  else if (length(row_index) > 1){
    #if multiple matches, return the first
    row_index <- row_index[1]
  }
  
  parameter <- data[row_index, "Data Point  Value"]
  time <- data[row_index, "Data Collection Date & Time"]
  submit <- data[row_index, "Submitter"]
  
  
  
  output <- list() #to store the parameter and the time
  output$parameter <- parameter
  output$time <- time
  output$submit <- submit
  
  
  return(output)
  
}#end getDryingTime

getValue <- function(data,ColName){
  
  row_index <- grep(ColName, data[, "Data Point Name"], ignore.case = TRUE)
  
  
  if (length(row_index) == 0){
    #if no match was found, return NA
    return (list(parameter = NA, time = NA, submit = NA))
  }
  else if (length(row_index) > 1){
    #if multiple matches, return the first
    row_index <- row_index[1]
  }
  
  
  parameter <- data[row_index, "Data Point  Value"]
  time <- data[row_index, "Data Collection Date & Time"]
  submit <- data[row_index, "Submitter"]
  
  
  
  output <- list() #to store the parameter and the time
  
  output$parameter <- parameter
  output$time <- time
  output$submit <- submit
  
  
  return(output)
  

}#end getValue Function


