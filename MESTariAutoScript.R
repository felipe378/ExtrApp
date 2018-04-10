#load the master files
#write master files to a backup location with a date stamp
#analyze the auto download files
#concatenate to the master files
#write the master files to the UI location
#delete files after eight weeks. Keep one file from that period.
#close out the script 


#### Loading the Current Master Files ####
ui_folder <- "//Mgrovef1/shared/Operations/EXTRUSIO/Felipe Correa Netto/Extrusion Application/Data/UI Data"

single_parametersandyields <- read.csv(paste0(ui_folder, "/", "Single Tari Parameters and Yield.csv"), 
                                       header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
multi_parametersandyields <- read.csv(paste0(ui_folder, "/", "Multi Tari Parameters and Yield.csv"), 
                                       header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
tapered_parametersandyields <- read.csv(paste0(ui_folder, "/", "Tapered Tari Parameters and Yield.csv"), 
                                       header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)


single_parameters <- read.csv(paste0(ui_folder, "/", "Single Tari Parameters.csv"), 
                   header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
multi_parameters <- read.csv(paste0(ui_folder, "/", "Multi Tari Parameters.csv"), 
                  header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
tapered_parameters <- read.csv(paste0(ui_folder, "/", "Tapered Tari Parameters.csv"), 
                    header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)

single_submitter <- read.csv(paste0(ui_folder, "/", "Single Tari Submitter.csv"), 
                   header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
multi_submitter <- read.csv(paste0(ui_folder, "/", "Multi Tari Submitter.csv"), 
                  header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
tapered_submitter <- read.csv(paste0(ui_folder, "/", "Tapered Tari Submitter.csv"), 
                    header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)

single_time <- read.csv(paste0(ui_folder, "/", "Single Tari Time.csv"), 
                   header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
multi_time <- read.csv(paste0(ui_folder, "/", "Multi Tari Time.csv"), 
                  header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
tapered_time <- read.csv(paste0(ui_folder, "/", "Tapered Tari Time.csv"), 
                    header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)

single_total <- read.csv(paste0(ui_folder, "/", "Single Tari Total.csv"), 
                   header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
multi_total <- read.csv(paste0(ui_folder, "/", "Multi Tari Total.csv"), 
                  header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
tapered_total <- read.csv(paste0(ui_folder, "/", "Tapered Tari Total.csv"), 
                    header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)


#### Re-writting the Master Files to a Backup ####
backup_folder <- "//Mgrovef1/shared/Operations/EXTRUSIO/Felipe Correa Netto/Test/Backup UI Data"

current_date <- Sys.Date() #gets current date

backup_single_parametersandyield_name <- paste0("Backup Single Tari Parameters And Yield ", current_date, ".csv")
backup_multi_parametersandyield_name <- paste0("Backup Multi Tari Parameters And Yield ", current_date, ".csv")
backup_tapered_parametersandyield_name <- paste0("Backup Tapered Tari Parameters And Yield ", current_date, ".csv")

backup_single_parameters_name <- paste0("Backup Single Tari Parameters ", current_date, ".csv")
backup_multi_parameters_name <- paste0("Backup Multi Tari Parameters ", current_date, ".csv")
backup_tapered_parameters_name <- paste0("Backup Tapered Tari Parameters ", current_date, ".csv")

backup_single_submitter_name <- paste0("Backup Single Tari Submitter ", current_date, ".csv")
backup_multi_submitter_name <- paste0("Backup Multi Tari Submitter ", current_date, ".csv")
backup_tapered_submitter_name <- paste0("Backup Tapered Tari Submitter ", current_date, ".csv")

backup_single_time_name <- paste0("Backup Single Tari Time ", current_date, ".csv")
backup_multi_time_name <- paste0("Backup Multi Tari Time ", current_date, ".csv")
backup_tapered_time_name <- paste0("Backup Tapered Tari Time ", current_date, ".csv")

backup_single_total_name <- paste0("Backup Single Tari Total ", current_date, ".csv")
backup_multi_total_name <- paste0("Backup Multi Tari Total ", current_date, ".csv")
backup_tapered_total_name <- paste0("Backup Tapered Tari Total ", current_date, ".csv")


#writing the files

fwrite(single_parametersandyield, paste0(backup_folder, "/", backup_single_parametersandyield_name))
fwrite(multi_parametersandyield, paste0(backup_folder, "/", backup_multi_parametersandyield_name))
fwrite(tapered_parametersandyield, paste0(backup_folder, "/", backup_tapered_parametersandyield_name))

fwrite(single_parameters, paste0(backup_folder, "/", backup_single_parameters_name))
fwrite(multi_parameters, paste0(backup_folder, "/", backup_multi_parameters_name))
fwrite(tapered_parameters, paste0(backup_folder, "/", backup_tapered_parameters_name))

fwrite(single_submitter, paste0(backup_folder, "/", backup_single_submitter_name))
fwrite(multi_submitter, paste0(backup_folder, "/", backup_multi_submitter_name))
fwrite(tapered_submitter, paste0(backup_folder, "/", backup_tapered_submitter_name))

fwrite(single_time, paste0(backup_folder, "/", backup_single_time_name))
fwrite(multi_time, paste0(backup_folder, "/", backup_multi_time_name))
fwrite(tapered_time, paste0(backup_folder, "/", backup_tapered_time_name))

fwrite(single_total, paste0(backup_folder, "/", backup_single_total_name))
fwrite(multi_total, paste0(backup_folder, "/", backup_multi_total_name))
fwrite(tapered_total, paste0(backup_folder, "/", backup_tapered_total_name))



#### Analyzing the Auto Download Files ####
auto_download_data <- read.csv("//Mgrovef1/shared/Operations/EXTRUSIO/Felipe Correa Netto/Extrusion Application/Data/AutoDownload/MES/CTP - Data Points - Previous Day.csv",
                               header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
colnames(auto_download_data) <- c("Container", "Container Status", "Material", "Material Desc",
                                  "Material Rev", "Model", "SAP Batch", "Current Qty", "Production Order",
                                  "Production Order Type", "SWR", "Data Point  Value", "High Limit",
                                  "Low Limit", "Result", "Parent Container", "Final Confirmed Quantity",
                                  "Data Point Desc", "Sequence", "Duration Delta", "Data Point Name",
                                  "Original Qty", "Task List", "Task List Description",
                                  "Task List Rev", "Task Item", "Work Cell", "Serial #",
                                  "Product Family", "Submitter", "Data Collection Date & Time")

auto_download_data <- auto_download_data[1:which(grepl("CON_CONTAINER", auto_download_data[,1]))[1],]

single_pps_data <- read.csv("//Mgrovef1/shared/Operations/EXTRUSIO/Felipe Correa Netto/Extrusion Application/Data/UI Data/Single PPS Data_UI_30 August 2017.csv",
                             header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
multi_pps_data <- read.csv("//Mgrovef1/shared/Operations/EXTRUSIO/Felipe Correa Netto/Extrusion Application/Data/UI Data/Multi-Layered PPS Data_UI_30 August 2017.csv",
                            header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
tapered_pps_data <- read.csv("//Mgrovef1/shared/Operations/EXTRUSIO/Felipe Correa Netto/Extrusion Application/Data/UI Data/Tapered PPS Data_UI_30 August 2017.csv",
                              header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)

single_part_numbers <- unique(single_pps_data$`Part Number`)
multi_part_numbers <- unique(multi_pps_data$`Part Number`)
tapered_part_numbers <- unique(tapered_pps_data$`Part Number`)

single_aggregate_mes_data <- auto_download_data[which(auto_download_data$Material %in% single_part_numbers),]
multi_aggregate_mes_data <- auto_download_data[which(auto_download_data$Material %in% multi_part_numbers),]
tapered_aggregate_mes_data <- auto_download_data[which(auto_download_data$Material %in% tapered_part_numbers),]

source("C:/Users/correaf/Documents/GitHub/ExtrusionApp/MESTariAnalyer.R")

single_mes_list <- generateTariList(single_aggregate_mes_data)
multi_mes_list <- generateTariList(multi_aggregate_mes_data)
tapered_mes_list <- generateTariList(tapered_aggregate_mes_data)


 #### Rbiding to the Master Data Frames and then Writing Them ####
single_parameters_new <- rbind(single_parametersandyield, single_mes_list$parameters)
multi_parameters_new <- rbind(multi_parametersandyield, multi_mes_list$parameters)
tapered_parameters_new <- rbind(tapered_parametersandyield, tapered_mes_list$parameters)

single_submitter_new <- rbind(single_submitter, single_mes_list$submitter)
multi_submitter_new <- rbind(multi_submitter, multi_mes_list$submitter)
tapered_submitter_new <- rbind(tapered_submitter, tapered_mes_list$submitter)

single_time_new <- rbind(single_time, single_mes_list$time)
multi_time_new <- rbind(multi_time, multi_mes_list$time)
tapered_time_new <- rbind(tapered_time, tapered_mes_list$time)

single_total_new <- rbind(single_total, single_mes_list$total)
multi_total_new <- rbind(multi_total, multi_mes_list$total)
tapered_total_new <- rbind(tapered_total, tapered_mes_list$total)

fwrite(single_parameters_new, paste0(ui_folder, "/", "Single Tari Parameters .csv"))
fwrite(multi_parameters_new, paste0(ui_folder, "/", "Multi Tari Parameters .csv"))
fwrite(tapered_parameters_new, paste0(ui_folder, "/", "Tapered Tari Parameters .csv"))


fwrite(single_submitter_new, paste0(ui_folder, "/", "Single Tari Submitter .csv"))
fwrite(multi_submitter_new, paste0(ui_folder, "/", "Multi Tari Submitter .csv"))
fwrite(tapered_submitter_new, paste0(ui_folder, "/", "Tapered Tari Submitter .csv"))

fwrite(single_time_new, paste0(ui_folder, "/", "Single Tari Time .csv"))
fwrite(multi_time_new, paste0(ui_folder, "/", "Multi Tari Time .csv"))
fwrite(tapered_time_new, paste0(ui_folder, "/", "Tapered Tari Time .csv"))

fwrite(single_parameters_new, paste0(ui_folder, "/", "Single Tari Total .csv"))
fwrite(multi_parameters_new, paste0(ui_folder, "/", "Multi Tari Total .csv"))
fwrite(tapered_parameters_new, paste0(ui_folder, "/", "Tapered Tari Total .csv"))


#### I need to analyze the OPMR Scrap Data ####



# single_parametersandyields_day <-
# multi_parametersandyields_day <- 
# tapered_parametersandyields_day <- 
# 
# single_parametersandyield_new <- 
# multi_parametersandyield_new <- 
# tapered_parametersandyield_new <- 






