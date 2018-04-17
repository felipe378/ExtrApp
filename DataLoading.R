library(shiny)
library(bootstrap)
library(jpeg)
library(ggplot2)
library(DT)
library(stringr)
library(gsubfn)
library(proto)
library(sqldf)
library(plyr)
library(dplyr)
library(data.table)


#LOADING DATA

source("./Resin_Cleaning.R")
source("./DataLoading_MiscFunctions.R")


#### Loading the Files into the Global Environment ####

#Creating variables across all sessions
#getting path for the data files
path <- "//Mgrovef1/shared/Operations/EXTRUSIO/Felipe Correa Netto/Extrusion Application/Data/UI Data/Dev"


### Loading the data


single_pps_data <- fread(paste(path, "Single PPS Data_UI_30 August 2017 Dev.csv", sep = "/"), 
                         header = TRUE, 
                         na.strings = c("NA", ""), 
                         stringsAsFactors = FALSE,
                         check.names = T)
single_pps_data[,Extrusion.Type := "Single"]
setcolorder(single_pps_data, c("Extrusion.Type", 
                               colnames(single_pps_data)[1:(ncol(single_pps_data)-1)]))
multi_pps_data <- fread(paste(path, "Multi-Layered PPS Data_UI_30 August 2017 Dev.csv", sep = "/"), 
                         header = TRUE, 
                         na.strings = c("NA", ""), 
                         stringsAsFactors = FALSE,
                        check.names = T)
tapered_pps_data <- fread(paste(path, "Tapered PPS Data_UI_30 August 2017 Dev.csv", sep = "/"), 
                         header = TRUE, 
                         na.strings = c("NA", ""), 
                         stringsAsFactors = FALSE,
                         check.names = T)
tapered_pps_data[,Extrusion.Type := "Tapered"]
setcolorder(tapered_pps_data, c("Extrusion.Type", 
                                colnames(tapered_pps_data)[1:(ncol(tapered_pps_data)-1)]))

total_pps_data <- rbind.fill(single_pps_data, multi_pps_data, tapered_pps_data)

#Load Sampling Data
all_sampling_data <- fread(paste(path, "All Sampling.csv", sep = "/"), 
                           header = TRUE, 
                           na.strings = c("NA", ""),
                           stringsAsFactors = FALSE,
                           check.names = T)


# Load MES Data
single_tari_parameter_and_yield_data <- fread(paste(path, 
                                                    "Single Tari Parameters and Yield.csv", 
                                                    sep = "/"), 
                                              header = TRUE, 
                                              na.strings = c("NA", ""),
                                              stringsAsFactors = FALSE,
                                              check.names = T)

multi_tari_parameter_and_yield_data <- fread(paste(path, 
                                                   "Multi Tari Parameters and Yield.csv", 
                                                   sep = "/"), 
                                             header = TRUE, 
                                             na.strings = c("NA", ""),
                                             stringsAsFactors = FALSE,
                                             check.names = T)

tapered_tari_parameter_and_yield_data <- fread(paste(path, 
                                                     "Tapered Tari Parameters and Yield.csv", 
                                                     sep = "/"), 
                                               header = TRUE, 
                                               na.strings = c("NA", ""),
                                               stringsAsFactors = FALSE,
                                               check.names = T)

total_tari_parameter_and_yield_data <- rbind.fill(single_tari_parameter_and_yield_data,
                                                  multi_tari_parameter_and_yield_data,
                                                  tapered_tari_parameter_and_yield_data)

#Load Scrap Code Data
scrapcodes_data <- fread(paste(path, "Scrap Codes.csv", sep = "/"), 
                         header = TRUE, 
                         na.strings = c("NA", ""),
                         stringsAsFactors = FALSE,
                         check.names = T)

#Load Resin Data
resin_data <- fread(paste(path, "Total Resin Information.csv", sep = "/"), 
                    header = TRUE, 
                    na.strings = c("NA", ""),
                    stringsAsFactors = FALSE,
                    check.names = T)

#Load Screw Data
screw_data <- fread(paste(path, "Screw Properties.csv", sep = "/"), 
                    header = TRUE, 
                    na.strings = c("NA", ""),
                    stringsAsFactors = FALSE,
                    check.names = T)



#' This will get the columns  that will be used to filter the MES data.
#' The use will select which columns they want to display and analyze
#' Because there are many columns, they will be split by categories
total_tari_columns <- lapply(list("temp", "press", "speed"), #search for these names
                              FUN = function(x){
                                grep(x, colnames(total_tari_parameter_and_yield_data),
                                     ignore.case = T, value = T)
                              })
names(total_tari_columns) <- c("temp", "press", "speed")
#this will get everything else that did not match (note: invert = T)
total_tari_columns$extra <- grep(paste(c("temp", "press", "speed"), collapse = "|"),
                                  colnames(total_tari_parameter_and_yield_data),
                                  ignore.case = T, value = T, invert = T)



#### Adding the Resin Information ####
#this will merge the resin data with the pps data by the issued resins
resindt <- getResinDataTable(total_pps_data, resin_data)
total_pps_data <- as.data.table(total_pps_data)
total_pps_data[,colnames(resindt) := resindt]
setcolorder(total_pps_data, c(colnames(total_pps_data)[1:3], 
                              colnames(resindt),
                              colnames(total_pps_data)[-(1:3)][colnames(total_pps_data)[-(1:3)] %out% colnames(resindt)]))



#### DATA CLEANING For all Data Tables ####
#obtain values for the filters
total_pps_data[,Part.Number.Search := Part.Number]
total_pps_data[,PPS.Number.Search := PPS.Number]



#### Button vectors for PPS documents ####

#imports the key that relates a part number to a print
partsandprints <- fread(paste(path, "Parts and Prints.csv", sep = "/"), 
                        header = TRUE, 
                        check.names = T,
                        na.strings = c("NA", ""),
                        stringsAsFactors = FALSE)



#getting the total buttons
#adding the shopping cart add button
total_pps_data[,Shopping.Cart := lapply(Part.Number, FUN = function(x){
  as.character(actionButton(inputId = paste0("button_", x),
               label = "Add Part",
               onclick = 'Shiny.onInputChange(\"totaladd_button\", this.id)'))
})]
#rearrange to be the first column
setcolorder(total_pps_data, c("Shopping.Cart", colnames(total_pps_data)[-ncol(total_pps_data)]))


#These two functions are for the server
total_pps_data[,Part.Number := lapply(Part.Number.Search, FUN = function(x){
  as.character(actionButton(inputId = paste0("print_button_", x),
               label = x,
               onclick = paste0("window.open(\"https://plm.bsci.bossci.com:1443/Windchill/netmarkets/jsp/bsci/plm/viewable/LatestEffectiveReleased.jsp?number=%11",
                                partsandprints[Part == x, Print], #get the print from the key
                                "\")")))
})]

total_pps_data[,PPS.Number := lapply(PPS.Number.Search, FUN = function(x){
  as.character(actionButton(inputId = paste0("print_button_", x),
               label = x,
               onclick = paste0("window.open(\"https://plm.bsci.bossci.com:1443/Windchill/netmarkets/jsp/bsci/plm/viewable/LatestEffectiveReleased.jsp?number=%11",
                                x,
                                "\")")))
})]


#### Read the FilterKey ####
filterkey <- fread(paste(path, "FilterKey Dev.csv", sep = "/"), 
                   header = TRUE, 
                   na.strings = c("NA", ""), 
                   stringsAsFactors = FALSE,
                   check.names = T)

filterkey_list <- lapply(split(filterkey, seq(nrow(filterkey))), as.list)
names(filterkey_list) <- filterkey[["Filter"]]

filter_options <- lapply(filterkey_list, filterOptions)



#### Removing Unneeded Things ####
rm(single_pps_data, multi_pps_data, tapered_pps_data)
rm(single_tari_parameter_and_yield_data, multi_tari_parameter_and_yield_data,
   tapered_tari_parameter_and_yield_data)
# rm(path)
# rm(resindt, partsandprints)