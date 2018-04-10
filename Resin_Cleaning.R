clean_data <- function(data){
  
  rows <- nrow(data)
  count <- 1
  total_count <- 1
  
  blank <- data.frame(matrix(nrow = 1000, ncol = 2))
  
  while (count < rows + 1){
    ps <- data[count,1]
    ds <- data[count,2]
    
    inner_count <- 1
    numbers <- length(strsplit(ps, ";")[[1]])
    psplit <- strsplit(ps, "; ")[[1]]
    dsplit <- strsplit(ds, "; ")[[1]]
    
    
    while (inner_count < numbers + 1){
      blank[total_count, 1] <- psplit[inner_count]
      blank[total_count, 2] <- dsplit[inner_count]
      
      inner_count <- inner_count + 1
      total_count <- total_count + 1
    }
    
    
    count <- count + 1
  }#end while
  
  return(blank)
}


addResinFamilies <-function(data){
  
  
  pebax_indices <- grep("pebax", data$'Resin Description', ignore.case = TRUE)
  data$'Resin Families'[pebax_indices] <- paste0(data$'Resin Families'[pebax_indices], "Polyether Block Polyamide/Nylon; ")
  
  PBX_indices <- grep("PBX", data$'Resin Description', ignore.case = FALSE)
  data$'Resin Families'[PBX_indices] <- paste0(data$'Resin Families'[PBX_indices], "Polyether Block Polyamide/Nylon; ")
  
  vestamid_indices <- grep("vestamid", data$'Resin Description', ignore.case = TRUE)
  data$'Resin Families'[vestamid_indices] <- paste0(data$'Resin Families'[vestamid_indices], "Polyamide/Nylon; ")
  vest_indices <- grep("\\bvest\\b", data$'Resin Description', ignore.case = TRUE)
  data$'Resin Families'[vest_indices] <- paste0(data$'Resin Families'[vest_indices], "Polyamide/Nylon; ")
  
  grilamid_indices <- grep("grilamid", data$'Resin Description', ignore.case = TRUE)
  data$'Resin Families'[grilamid_indices] <- paste0(data$'Resin Families'[grilamid_indices], "Polyamide/Nylon; ")
  TR_indices <- grep("TR", data$'Resin Description', ignore.case = FALSE)
  data$'Resin Families'[TR_indices] <- paste0(data$'Resin Families'[TR_indices], "Polyamide/Nylon; ")
  
  hytrel_indices <- grep("hytrel", data$'Resin Description', ignore.case = TRUE)
  data$'Resin Families'[hytrel_indices] <- paste0(data$'Resin Families'[hytrel_indices], "Copolyester; ")
  
  polyurethane_indices <- grep("polyurethane", data$'Resin Description', ignore.case = TRUE)
  data$'Resin Families'[polyurethane_indices] <- paste0(data$'Resin Families'[polyurethane_indices], "Polyurethane; ")
  TPU_indices <- grep("TPU", data$'Resin Description', ignore.case = FALSE)
  data$'Resin Families'[TPU_indices] <- paste0(data$'Resin Families'[TPU_indices], "Polyurethane; ")
  
  polypropylene_indices <- grep("polypropylene", data$'Resin Description', ignore.case = TRUE)
  data$'Resin Families'[polypropylene_indices] <- paste0(data$'Resin Families'[polypropylene_indices], "Polypropylene; ")
  
  EM_indices <- grep("EM", data$'Resin Description', ignore.case = FALSE)
  data$'Resin Families'[EM_indices] <- paste0(data$'Resin Families'[EM_indices], "Copolyester; ")
  
  grilflex_indices <- grep("grilflex", data$'Resin Description', ignore.case = TRUE)
  data$'Resin Families'[grilflex_indices] <- paste0(data$'Resin Families'[grilflex_indices], "Polyamide/Nylon; ")
  
  HDPE_indices <- grep("HDPE", data$'Resin Description', ignore.case = FALSE)
  data$'Resin Families'[HDPE_indices] <- paste0(data$'Resin Families'[HDPE_indices], "High-Density Polyethylene; ")
  
  LDPE_indices <- grep("LDPE", data$'Resin Description', ignore.case = FALSE)
  data$'Resin Families'[LDPE_indices] <- paste0(data$'Resin Families'[LDPE_indices], "Low-Density Polyethylene; ")
  
  MDPE_indices <- grep("MDPE", data$'Resin Description', ignore.case = FALSE)
  data$'Resin Families'[MDPE_indices] <- paste0(data$'Resin Families'[MDPE_indices], "Medium-Density Polyethylene; ")
  
  elasthane_indices <- grep("elasthane", data$'Resin Description', ignore.case = TRUE)
  data$'Resin Families'[elasthane_indices] <- paste0(data$'Resin Families'[elasthane_indices], "Polyether Polyurethane/Polyurethane; ")
  
  peek_indices <- grep("peek", data$'Resin Description', ignore.case = TRUE)
  data$'Resin Families'[peek_indices] <- paste0(data$'Resin Families'[peek_indices], "Polyether Ketone; ")
  
  pellethane_indices <- grep("pellethane", data$'Resin Description', ignore.case = TRUE)
  data$'Resin Families'[pellethane_indices] <- paste0(data$'Resin Families'[pellethane_indices], "Polyurethane; ")
  
  TCFX_indices <- grep("TCFX", data$'Resin Description', ignore.case = FALSE)
  data$'Resin Families'[TCFX_indices] <- paste0(data$'Resin Families'[TCFX_indices], "Aliphatic Polyether Polyurethane; ")
  TFX_indices <- grep("TFX", data$'Resin Description', ignore.case = FALSE)
  data$'Resin Families'[TFX_indices] <- paste0(data$'Resin Families'[TFX_indices], "Aliphatic Polyether Polyurethane; ")
  tecoflex_indices <- grep("tecoflex", data$'Resin Description', ignore.case = TRUE)
  data$'Resin Families'[tecoflex_indices] <- paste0(data$'Resin Families'[tecoflex_indices], "Aliphatic Polyether Polyurethane; ")
  
  cristamid_indices <- grep("cristamid", data$'Resin Description', ignore.case = TRUE)
  data$'Resin Families'[cristamid_indices] <- paste0(data$'Resin Families'[cristamid_indices], "Semi-Aromatic Polyamide/Nylon; ")
  
  rilsamid_indices <- grep("rilsamid", data$'Resin Description', ignore.case = TRUE)
  data$'Resin Families'[rilsamid_indices] <- paste0(data$'Resin Families'[rilsamid_indices], "Polyamide/Nylon; ")
  
  vestodur_indices <- grep("vestodur", data$'Resin Description', ignore.case = TRUE)
  data$'Resin Families'[vestodur_indices] <- paste0(data$'Resin Families'[vestodur_indices], "Polybutylene Terephthalate; ")
  
  marlex_indices <- grep("marlex", data$'Resin Description', ignore.case = TRUE)
  data$'Resin Families'[marlex_indices] <- paste0(data$'Resin Families'[marlex_indices], "Polypropylene or High-Density Polyethylene; ")
  
  FEP_indices <- grep("FEP", data$'Resin Description', ignore.case = FALSE)
  data$'Resin Families'[FEP_indices] <- paste0(data$'Resin Families'[FEP_indices], "Fluorinated Ethylene Propylene; ")
  
  alathon_indices <- grep("alathon", data$'Resin Description', ignore.case = TRUE)
  data$'Resin Families'[alathon_indices] <- paste0(data$'Resin Families'[alathon_indices], "High-Density Polyethylene; ")
  
  estane_indices <- grep("\\bestane", data$'Resin Description', ignore.case = TRUE)
  data$'Resin Families'[estane_indices] <- paste0(data$'Resin Families'[estane_indices], "Polyurethane; ")
  
  PLLA_indices <- grep("PLLA", data$'Resin Description', ignore.case = FALSE)
  data$'Resin Families'[PLLA_indices] <- paste0(data$'Resin Families'[PLLA_indices], "Poly (L-Lactide); ")
  
  profax_indices <- grep("pro-fax", data$'Resin Description', ignore.case = TRUE)
  data$'Resin Families'[profax_indices] <- paste0(data$'Resin Families'[profax_indices], "Polypropylene; ")
  
  data$'Resin Families' <- gsub("; $", "", data$'Resin Families')
  
  
  return(data)
}


addResinsToParts <- function(pps, resin_data){
  rows <- nrow(pps)
  columns <- ncol(pps)
  new_pps <- pps
  new_pps$'Resin Families' <- c(rep("", nrow(new_pps)))
  new_pps$'Is Resin Blended with Anything?' <- c(rep("", nrow(new_pps)))
  new_pps$'Is Resin a Polymer Blend?' <- c(rep("", nrow(new_pps)))
  new_pps$'Is Resin Filled?' <- c(rep("", nrow(new_pps)))
  new_pps$'Resin Fillers' <- c(rep("", nrow(new_pps)))
  new_pps$'Is Resin Colored?' <- c(rep("", nrow(new_pps)))
  new_pps$'Resin Color' <- c(rep("", nrow(new_pps)))
  new_pps$'Is Resin Radiopaque?' <- c(rep("", nrow(new_pps)))
  new_pps$'Resin Durometer (D)' <- c(rep("", nrow(new_pps)))
  new_pps$'Average Resin Durometer (D)' <- c(rep("", nrow(new_pps)))
  
  resin_rows <- nrow(resin_data)
  resin_count <- 1
  
  while (resin_count < resin_rows + 1){
    current_resin <- resin_data$'Resin Number'[resin_count]
    current_resin_family <-  resin_data$'Resin Families'[resin_count]
    current_isresinblend <-  resin_data$'Is Resin a General Blend?'[resin_count]
    current_isresinpblend <-  resin_data$'Is Resin a Polymer Blend?'[resin_count]
    current_isresinfilled <-  resin_data$'Is Resin Filled?'[resin_count]
    current_resinfillers <-  resin_data$'Resin Fillers'[resin_count]
    current_isresincolored <-  resin_data$'Is Resin Colored?'[resin_count]
    current_resincolor <-  resin_data$'Resin Color'[resin_count]
    current_isresinradiopaque <-  resin_data$'Is Resin Radiopaque?'[resin_count]
    current_resindurometer <-  resin_data$'Durometer (D)'[resin_count]
    current_resinavedurometer <-  resin_data$'Averaged Durometer (D)'[resin_count]
    
    resin_matches <- grep(current_resin, pps$'Resin Number', ignore.case = TRUE)
    match_length <- length(resin_matches)
    
    match_count <- 1
    
    if (length(resin_matches) != 0){
      while (match_count < match_length + 1){
        
        resin_match_index <- resin_matches[match_count]
        
        new_pps$'Resin Families'[resin_match_index] <- paste0(new_pps$'Resin Families'[resin_match_index], 
                                                              "; ",
                                                              current_resin_family)
        new_pps$'Is Resin Blended with Anything?'[resin_match_index] <- paste0(new_pps$'Is Resin a General Blend'[resin_match_index],
                                                                                 "; ",
                                                                                 current_isresinblend)   
        new_pps$'Is Resin a Polymer Blend?'[resin_match_index] <- paste0(new_pps$'Is Resin a Polymer Blend?'[resin_match_index],
                                                                         "; ",
                                                                         current_isresinpblend)   
        new_pps$'Is Resin Filled?'[resin_match_index] <- paste0(new_pps$'Is Resin Filled?'[resin_match_index],
                                                                "; ",
                                                                current_isresinfilled)   
        new_pps$'Resin Fillers'[resin_match_index] <- paste0(new_pps$'Resin Fillers'[resin_match_index],
                                                             "; ",
                                                             current_resinfillers)   
        new_pps$'Is Resin Colored?'[resin_match_index] <- paste0(new_pps$'Is Resin Colored?'[resin_match_index],
                                                                 "; ",
                                                                 current_isresincolored)
        new_pps$'Resin Color'[resin_match_index] <- paste0(new_pps$'Resin Color'[resin_match_index],
                                                                 "; ",
                                                           current_resincolor)
        new_pps$'Is Resin Radiopaque?'[resin_match_index] <- paste0(new_pps$'Is Resin Radiopaque?'[resin_match_index],
                                                                   "; ",
                                                                   current_isresinradiopaque)   
        new_pps$'Resin Durometer (D)'[resin_match_index] <- paste0(new_pps$'Resin Durometer (D)'[resin_match_index],
                                                                   "; ",
                                                                   current_resindurometer)   
        new_pps$'Average Resin Durometer (D)'[resin_match_index] <- paste0(new_pps$'Average Resin Durometer (D)'[resin_match_index],
                                                                           "; ",
                                                                           current_resinavedurometer)   
        
        
        match_count <- match_count + 1
      }#end resin_match while
        
        
    }#end if for resin_match
    
    resin_count <- resin_count + 1
  }#end resin while

  #Cleans it three times for a maximum of 3 resins
  
  new_pps$'Resin Families'  <- gsub("^; ", "", new_pps$'Resin Families')
  new_pps$'Is Resin Blended with Anything?'  <- gsub("^; ", "", new_pps$'Is Resin Blended with Anything?')
  new_pps$'Is Resin a Polymer Blend?'  <- gsub("^; ", "", new_pps$'Is Resin a Polymer Blend?')
  new_pps$'Is Resin Filled?'  <- gsub("^; ", "", new_pps$'Is Resin Filled?')
  new_pps$'Resin Fillers'  <- gsub("^; ", "", new_pps$'Resin Fillers')
  new_pps$'Is Resin Colored?'  <- gsub("^; ", "", new_pps$'Is Resin Colored?')
  new_pps$'Resin Color'  <- gsub("^; ", "", new_pps$'Resin Color')
  new_pps$'Is Resin Radiopaque?'  <- gsub("^; ", "", new_pps$'Is Resin Radiopaque?')
  new_pps$'Resin Durometer (D)'  <- gsub("^; ", "", new_pps$'Resin Durometer (D)')
  new_pps$'Average Resin Durometer (D)'  <- gsub("^; ", "", new_pps$'Average Resin Durometer (D)')
  
  new_pps$'Resin Families'  <- gsub("; $", "", new_pps$'Resin Families')
  new_pps$'Is Resin Blended with Anything?'  <- gsub("; $", "", new_pps$'Is Resin Blended with Anything?')
  new_pps$'Is Resin a Polymer Blend?'  <- gsub("; $", "", new_pps$'Is Resin a Polymer Blend?')
  new_pps$'Is Resin Filled?'  <- gsub("; $", "", new_pps$'Is Resin Filled?')
  new_pps$'Resin Fillers'  <- gsub("; $", "", new_pps$'Resin Fillers')
  new_pps$'Is Resin Colored?'  <- gsub("; $", "", new_pps$'Is Resin Colored?')
  new_pps$'Resin Color'  <- gsub("; $", "", new_pps$'Resin Color')
  new_pps$'Is Resin Radiopaque?'  <- gsub("; $", "", new_pps$'Is Resin Radiopaque?')
  new_pps$'Resin Durometer (D)'  <- gsub("; $", "", new_pps$'Resin Durometer (D)')
  new_pps$'Average Resin Durometer (D)'  <- gsub("; $", "", new_pps$'Average Resin Durometer (D)')
  
  new_pps$'Resin Families'  <- gsub("^; ", "", new_pps$'Resin Families')
  new_pps$'Is Resin Blended with Anything?'  <- gsub("^; ", "", new_pps$'Is Resin Blended with Anything?')
  new_pps$'Is Resin a Polymer Blend?'  <- gsub("^; ", "", new_pps$'Is Resin a Polymer Blend?')
  new_pps$'Is Resin Filled?'  <- gsub("^; ", "", new_pps$'Is Resin Filled?')
  new_pps$'Resin Fillers'  <- gsub("^; ", "", new_pps$'Resin Fillers')
  new_pps$'Is Resin Colored?'  <- gsub("^; ", "", new_pps$'Is Resin Colored?')
  new_pps$'Resin Color'  <- gsub("^; ", "", new_pps$'Resin Color')
  new_pps$'Is Resin Radiopaque?'  <- gsub("^; ", "", new_pps$'Is Resin Radiopaque?')
  new_pps$'Resin Durometer (D)'  <- gsub("^; ", "", new_pps$'Resin Durometer (D)')
  new_pps$'Average Resin Durometer (D)'  <- gsub("^; ", "", new_pps$'Average Resin Durometer (D)')
  
  new_pps$'Resin Families'  <- gsub("^; ", "", new_pps$'Resin Families')
  new_pps$'Is Resin Blended with Anything?'  <- gsub("^; ", "", new_pps$'Is Resin Blended with Anything?')
  new_pps$'Is Resin a Polymer Blend?'  <- gsub("^; ", "", new_pps$'Is Resin a Polymer Blend?')
  new_pps$'Is Resin Filled?'  <- gsub("^; ", "", new_pps$'Is Resin Filled?')
  new_pps$'Resin Fillers'  <- gsub("^; ", "", new_pps$'Resin Fillers')
  new_pps$'Is Resin Colored?'  <- gsub("^; ", "", new_pps$'Is Resin Colored?')
  new_pps$'Resin Color'  <- gsub("^; ", "", new_pps$'Resin Color')
  new_pps$'Is Resin Radiopaque?'  <- gsub("^; ", "", new_pps$'Is Resin Radiopaque?')
  new_pps$'Resin Durometer (D)'  <- gsub("^; ", "", new_pps$'Resin Durometer (D)')
  new_pps$'Average Resin Durometer (D)'  <- gsub("^; ", "", new_pps$'Average Resin Durometer (D)')
  
  new_indices <- c((columns + 1):(columns + 10))
  
  new_pps <- new_pps[,c(1,2,3,4,5,new_indices, 6:columns)]
  
  
  
  return(new_pps)
}

getResinDataTable <- function(pps_data, resin_data){
  #merges the resin data and the pps data based on the resin number issued to the extruded parts
  #the data must be in data.table format
  
  #creates a list of issued resins. Each element in the list corresponds to a part number
  #thus in each element there is a vector whose length is equal to the number of issued resins
  #to that part
  issued_resins <- strsplit(pps_data[["Resin.Number"]], "; ")
  #names the list elements based on the part number
  names(issued_resins) <- pps_data[["Part.Number"]]
  
  issued_resins_data <- lapply(issued_resins, FUN = function(x){
    #goes through each part and gets the information relating to the issued resins
    indices <- which(resin_data[["Resin.Number"]] %in% x)
    
    if (length(indices) != 0){
      #if there was a match
      #paste together the column elements (pasting rows together along a column)
      resins <- as.data.table(lapply(resin_data[indices,], paste, collapse = "; "))
    }
    else{
      resins <- resin_data[1,] #insert an empty data frames
      resins[1,] <- NA
    }
    
    return(resins)
  })
  
  #returns by creating a data table of the list of data tables
  return(rbindlist(issued_resins_data))
  
  
  
} #end AddResinsToParts2




#### Extra ####

# > resins1 <- resins[setdiff(c(1:nrow(resisn)), grep("mandrel", test1[,2], ignore.case = TRUE)), ]
# Error in nrow(resisn) : object 'resisn' not found
# > resins1 <- resins[setdiff(c(1:nrow(resins)), grep("mandrel", test1[,2], ignore.case = TRUE)), ]
# > resins1 <- resins[setdiff(c(1:nrow(resisn)), grep("mandrel", resins1[,2], ignore.case = TRUE)), ]
# Error in nrow(resisn) : object 'resisn' not found
# > resins1 <- resins[setdiff(c(1:nrow(resins)), grep("mandrel", resins1[,2], ignore.case = TRUE)), ]
# > resins1 <- resins[setdiff(c(1:nrow(resins)), grep("mandrel", resins[,2], ignore.case = TRUE)), ]
# > resins2 <- resins1[setdiff(c(1:nrow(resins1)), grep("core", resins1[,2], ignore.case = TRUE)), ]
# > View(resins2)
# > resins2 <- resins2[!is.na(resins[,1]),]
# > resins2 <- resins1[setdiff(c(1:nrow(resins1)), grep("core", resins1[,2], ignore.case = TRUE)), ]
# > resisn3 <- resins2[!is.na(resins2[,1]),]
# > View(resisn3)
# > resins4 <-  resisn3[setdiff(c(1:nrow(resisn3)), grep("wire", resisn3[,2], ignore.case = TRUE)), ]
# > grep("wire", resisn3[,2], ignore.case = TRUE)
# [1]  35 138 140 142 143 150 179 181 187 227
# > resins5 <-  resins4[setdiff(c(1:nrow(resins4)), grep("braid", resins4[,2], ignore.case = TRUE)), ]
# > resins6 <-  resins5[setdiff(c(1:nrow(resins5)), grep("spool", resins5[,2], ignore.case = TRUE)), ]
