#_d:id of the output of checkbox
#_input: the name of the searchbox
#PCS:Part Catalog--Single Extrusion PPS
#PCM: Part Catalog--Multi Extrusion PPS
#PCT: Part Catalog--Tapered Extrusion PPS

server<-function(input,output,session){
  #' Split up into five sections: 
  #' 1) The environments and the setter and getter methods
  #' 2) The observe functions for the input variables
  #' 3) The single extrusion data set cleaning
  #' 4) The multi layered extrusion data set cleaning
  #' 5) The tapered extrusion data set cleaning
  
  
  
  
  #### Single PPS  ####
  
  generateNewDF <- function(df, index_name, value){
    #this determines whether the index is max, min, or other. And then it cleans up the df based
    #on that and generates a new clean df
    
    if (length(grep("min", index_name, ignore.case = TRUE)) != 0){
      #if min was found in the index_name
      
      parameter_name <- gsub(" Min", "", index_name)
      parameter_name <- gsub("\\(", "\\\\(", parameter_name)
      parameter_name <- gsub("\\)", "\\\\)", parameter_name)
      parameter_name <- paste0("^", parameter_name) #must start with it to avoid mis matches
      
      column_index <- grep(paste0("^",parameter_name), names(df), ignore.case = TRUE)
      
      new_df <- df[df[,column_index] >= value,]
      return(new_df)
      
    }
    else if(length(grep("max", index_name, ignore.case = TRUE)) != 0){
      #if max was found in the index_name
      
      parameter_name <- gsub(" Max", "", index_name)
      parameter_name <- gsub("\\(", "\\\\(", parameter_name)
      parameter_name <- gsub("\\)", "\\\\)", parameter_name)
      parameter_name <- paste0("^", parameter_name) #must start with it to avoid mis matches
      
      
      column_index <- grep(paste0("^",parameter_name), names(df), ignore.case = TRUE)
      
      new_df <- df[df[,column_index] <= value,]
      return(new_df)
      
    }
    else if (length(grep(paste(c("number", "print"), collapse = "|"), 
                         index_name, ignore.case = TRUE)) != 0
             && length(grep("Resin Number", index_name)) == 0){
      #this will catch part number, part description, resin description, and
      #pps numbers
      #it ensures that resin number will not be caught
      
      string_to_search <- gsub("\\| ", "|", gsub(";", "|", value))
      #this replaces semi-colons with a or for grep. It also removes spaces for example:
      #"90161; 123124;435 -> 90161|123124|435
      
      string_to_search <- cleanStringSearch(string_to_search) #handles special characters

      
      
      column_index <- grep(paste0("^",index_name), names(df), ignore.case = TRUE)
      
      raw_parts <- unlist(strsplit(unlist(strsplit(df[,column_index], ">")), "<"))
      
      clean_parts <- raw_parts[seq(3,length(raw_parts), 4)]
      
      new_df <- df[grep(string_to_search, clean_parts, ignore.case = TRUE),]
      return(new_df)
      
    }
    else if(length(grep(paste(c("description", "Resin Number"), collapse = "|"), 
                        index_name, ignore.case = TRUE)) != 0){
      #catches description because we cannot string split
      #also catches resin number
      
      string_to_search <- gsub("\\| ", "|", gsub(";", "|", value))
      #this replaces semi-colons with a or for grep. It also removes spaces for example:
      #"90161; 123124;435 -> 90161|123124|435
      
      string_to_search <- cleanStringSearch(string_to_search) #handles special characters
      
      
      column_index <- grep(paste0("^",index_name), names(df), ignore.case = TRUE)
      
      new_df <- df[grep(string_to_search, df[,column_index], ignore.case = TRUE),]
      return(new_df)
      
      
    }
    else{
      #this is not a min or a max, but instead is a select input
      
      parameter_name <- index_name
      parameter_name <- gsub("\\(", "\\\\(", parameter_name)
      parameter_name <- gsub("\\)", "\\\\)", parameter_name)
      parameter_name <- paste0("^", parameter_name) #must start with it to avoid mis matches
      
      column_index <- grep(paste0("^",parameter_name), names(df), ignore.case = TRUE)
      
      if (value == "All"){
        #if 'All' is selected
        new_df <- df
      }
      else if (value == "NA"){
        #ashow blanks
        new_df <- df[df[,column_index] == "",]
      }
      else{
        #a specific value was selected
        new_df <- df[df[,column_index] == value,]
      }
      
      return(new_df)
      
    }#end if-else on the types of parameters
    
  }#end generateNewDF
  
  e1 <- new.env(
    #This environment will store variable of inputs and stack that are used for comparison
    #of the input parameters that have been selected and changed
    #' variables to contain:
    #' sivector - a vector that will store the inputs of the single extrusion parameters
    #' sistack - the stack for the single inputs
  ) #creates a new environment to store instance variables
  
  #the assign will initialize the siidvector
  assign("siidvector", 
         c("Placeholder", "PCSPN", "PCSPD", "PCSRN", "PCSRD", "PCSPPSN",
           "Placeholder", "Placeholder", "Placeholder", "Placeholder", "Placeholder",
           "Placeholder", "Placeholder", "Placeholder", "Placeholder", "Placeholder",
           "PCSDS_min", "PCSDS_max", "PCSDLL", "PCSTS_min", "PCSTS_max",
           "PCSTLL", "PCSSP", 
           "PCSFT_min", "PCSFT_max", "PCSBZT1_min", "PCSBZT1_max",
           "PCSBZT2_min", "PCSBZT2_max", "PCSBZT3_min", "PCSBZT3_max",
           "PCSCT_min", "PCSCT_max", "PCSAT_min", "PCSAT_max",
           "PCSDT1_min", "PCSDT1_max", "PCSDT2_min", "PCSDT2_max",
           "PCSIDI_min", "PCSIDI_max", "PCSODI_min", "PCSODI_max",
           "PCSWT_min", "PCSWT_max", "PCSOR_min", "PCSOR_max", 
           "PCSCCT_min", "PCSCCT_max", "PCSLength_min", "PCSLength_max",
           "PCSPPD",
           "PCSNEXIV", "PCSAnnealed", "PCSCaliper", "PCSOS",
           "PCSMP", "PCSHT", "PCSSPD", "PCSSLD", "PCSDLN", "PCSULT",
           "PCSVC", "PCSIRD"), 
         envir = e1)
  
  
  ## These are the setter and getter function ##
  
  setSIIDVector <- function(vector){
    #set the vector for the input ids
    
    assign("siidvector", vector, env = e1)
    
  } #end setSIIDVector
  
  getSIIDVector <- function(){
    #returns the IDs of the inputs
    
    if(exists("siidvector", e1)){
      return(get("siidvector", e1))
    }
    else{
      return(NA)
    }
  } #end SIIDVector
  
  setSCBVector <- function(vector){
    #this sets the values for the single input vector
    assign("scbvector", vector, env = e1)
  } #end setSCVector
  
  getSCBVector <- function(vector){
    #this gets the values for the single checkbox vector
    
    if(exists("scbvector", e1)){
      return(get("scbvector", e1))
    }
    else{
      return(NA)
    }
    
  } #end getSCBVector
  
  setOriginalSIVector <- function(vector){
    #this sets the values for the original min and max input values
    assign("originalscbvector", vector, env = e1)
  } #end setOriginalSIVector
  
  getOriginalSIVector <- function(vector){
    #this gets the values for the original min and max input values
    
    if(exists("originalscbvector", e1)){
      return(get("originalscbvector", e1))
    }
    else{
      return(NA)
    }
    
  } #end getOriginalSIVector
  
  setSIVector <- function(vector){
    #this sets the values for the single input vector
    assign("sivector", vector, env = e1)
  } #end setSIVector
  
  getSIVector <- function(){
    #returns sivector
    
    if(exists("sivector", e1)){
      return(get("sivector", e1))
    }
    else{
      return(NA)
    }
    
  } #end getSIVector
  
  setSIStack <- function(stack){
    #this sets the stack for the single inputs
    assign("sistack", stack, env = e1)
  } #end setSIStack
  
  getSIStack <- function(){
    #returns sivector
    if(exists("sistack", e1)){
      return(get("sistack", e1))
    }
    else{
      return(NA)
    }
    
  } #end getSIStack
  
  getSIStack.length <- function(){
    #returns the length of the SIStack
    
    if(exists("sistack", e1)){
      #if it exists, return the length
      return(length(getSIStack()))
    }
    else{
      #else return NA
      return(NA)
    }
    
  }#end getSIStack.length
  
  addSIStack <- function(name){
    #this function adds a new parameter to the stack
    #it will only add to the stack if it is a unique name
    
    stack <- get("sistack", e1)
    
    
    #edit name
    rep_name <- gsub("\\(", "\\\\(", name)
    rep_name <- gsub("\\)", "\\\\)", rep_name)
    rep_name <- paste0("^", rep_name) #must start with it to avoid mis matches
    
    if (length(grep(rep_name, stack)) == 0){
      #the name is not currently in the stack
      stack <- c(stack, name) #add the name to the top of the stack (the right most)
      setSIStack(stack) #set the stack
    }
    else{
      #do nothing because the name is already present
    }
    
  } #end  addSIStack
  
  removeSIStack <- function(name){
    #this function removes the inputs associated with the checkbox name from the stack
    
    print("removeSIStack: We are in the removeSIStack")
    
    stack <- get("sistack", e1)
    
    
    #edit name
    rep_name <- gsub("\\(", "\\\\(", name)
    rep_name <- gsub("\\)", "\\\\)", rep_name)
    rep_name <- paste0("^", rep_name) #must start with it to avoid mis matches
    
    if (length(grep(rep_name, stack)) != 0){
      #the name is not currently in the stack
      indices <- grep(rep_name, stack)
      
      if (length(indices) == length(stack)){
        #if the length of the indices are equal to the stack
        print("removeSIStack: The length of the indices are equal to the stack")
        stack <- c()
      }
      else if (length(indices) == 1){
        print("removeSIStack: There was only one match in the stack")
        #if there is only one match
        if (indices == 1){
          #if the first matches is 1 or the only match is one
          stack <- stack[c(2:length(stack))] # removes the first element
        }
        else if (indices == length(stack)){
          stack <- stack[c(1:(indices - 1))]
        }
        else{
          #removes the index from the stack
          stack <- stack[c(1:(indices-1), (indices + 1):length(stack))]
        }#end if-else for the vallue of indices
      }
      else if (length(indices) == 2){
        print("removeSIStack: There were multiple matches to the stack")
        #if there are multiple indices
        if (indices[1] == 1){
          #if the first matches is 1 or the only match is one
          
          if (indices[2] == 2){
            stack <- stack[c(3:length(stack))] # removes the first and second element
          }
          else if (indices[2] == length(stack)){
            stack <- stack[c(2:(indices[2] - 1))]
          }
          else{
            stack <- stack[c(2:(indices[2] - 1), (indices[2] + 1):length(stack))] # the indices
          }#end if-else for the value of the second index
          
        }
        else if(indices[1] == (length(stack) - 1)){
          #if the first index is length(stack)-1, then the second index is the length of the stack
          stack <- stack[c(1:(indices[1] - 1))]
        }
        else if (indices[1] == (indices[2] + 1)){
          #if the indices are right next to each other but are not at the ends of the stack
          stack <- stack[c(1:(indices[1]-1), (indices[2] + 1):length(stack))]
        }
        else{
          #removes the indices from the stack
          stack <- stack[c(1:(indices[1]-1), (indices[1] + 1):(indices[2] - 1), (indices[2] + 1):length(stack))]
        }#end if-else for the value of indices
        
      }#end if-else for the length of indices
      else{
        #if it was neither, there was an error and the app shoudl stop
        print("The indices in the removeSIStack were more than 2")
        print(paste0("The indices are: ", indices))
        stop()
      }
      
      
      setSIStack(stack) #set the stack
    }
    else{
      #do nothing because the name is not in the stack
      print("removeSIStack: Nothing was done because the name was not in the stack")
    }
    
  } #end  removeSIStack
  
  setSDFList <- function(list){
    #sets the DFList
    assign("df_list", list, env = e1)
  }#end setSDFList
  
  getSDFList <- function(){
    #returns the df_list
    if(exists("df_list", e1)){
      #if it exists, return the length
      return((get("df_list", e1)))
    }
    else{
      #else return NA
      return(NA)
    }
    
  }#end getSDFList
  
  addSDFSeries <- function(index, index_name, value){
    #this creates the series of successively more narrow data frames based on the number of parameters
    #the user has searched by. 3 parameters means there is the original data frame and three ones
    #that have been successively narrowed.
    
    if (exists("df_list", e1)){
      #if the list already exists
      
      if(index == getSIStack.length() + 1){
        #if the index is larger than the list length, we have a new parameter that will be added
        df_list <- getSDFList() #gets latest df_list
        latest_df <- df_list[[index-1]] #the index should only be 1 greater than the list
        
        new_df <- generateNewDF(latest_df, index_name, value)
        df_list[[index]] <- new_df #add the new data frame
        setSDFList(df_list) #set the new list
        
        single_df_output$data <- new_df #set the data table to this
        
      }
      else if(index > getSIStack.length() + 1){
        #if there was an error in the index and it was too large
        print("The index for the stack was too large and greater than the allowable limit")
        print(paste0("The stack index is: ", index, ". The stack length is: ", getSIStack.length(), "."))
        stopApp()
      }
      else if(index < 1){
        #error in the index
        print("The index for the stack was zero or negative")
        print(paste0("The stack index is: ", index, ". The stack length is: ", getSIStack.length(), "."))
        stopApp()
      }
      else{
        #the index is within the stack which means we means a previous parameter is being changed
        stack <- getSIStack()
        stack_length <- getSIStack.length()
        
        input_values <- getSIVector()
        
        df_list <- getSDFList() #gets latest df_list
        
        if (index == 1){
          #if it is starting from the beginning
          
          new_df <- generateNewDF(single_pps_data, index_name, value) #starts from this initial one
          df_list[[index]] <- new_df #add the new data frame
          
          count <- index + 1
          
          while (count < stack_length + 1){
            
            previous_df <- df_list[[count-1]]
            current_parameter <- stack[count]
            current_parameter_value <- input_values[current_parameter]
            
            new_df <- generateNewDF(previous_df, current_parameter, current_parameter_value)
            count <- count + 1
            df_list[[count]] <- new_df #add the new data frame
          }#end while creating new DFs
          
          setSDFList(df_list) #set the new list
          single_df_output$data <- new_df #set the data table to this
          
        }
        else{
          
          count <- index
          
          while (count < stack_length + 1){
            
            previous_df <- df_list[[count-1]] #gets the previous df
            current_parameter <- stack[count]
            current_parameter_value <- input_values[current_parameter]
            
            new_df <- generateNewDF(previous_df, current_parameter, current_parameter_value)
            
            df_list[[count]] <- new_df #add the new data frame
            
            count <- count + 1
          }#end while creating new DFs
          
          setSDFList(df_list) #set the new list
          single_df_output$data <- new_df #set the data table to this
          
        }#end if-else for index value
        
      }#end if else for the index length
      
    }
    else{
      assign("df_list", list("hello", "hi"), env = e1) #if the list does not yet exist, it creates it
      #creates two dummy variables otherwise the list does not initialize correctly
      
      new_df <- generateNewDF(single_pps_data, index_name, value) #starts from this initial one
      df_list <- getSDFList() #gets latest df_list
      df_list[[index]] <- new_df #add the new data frame
      df_list[[2]] <- NULL #removes the previous dummy variable
      
      setSDFList(df_list) #set the new list
      single_df_output$data <- new_df #set the data table to this
      
    }#end if-else for the df_list existing
    
  } #end addSDFSeries
  
  removeSDFSeries <- function(){
    #this removes the data frames that were in the df_list because of the parameters. It is called
    #when a checkbox is unchecked and one of the parameters were present
    
    print("removeSDFSeries: We are in removeSDFSeries")
    
    if (exists("df_list", e1)){
      #if the list already exists
      
      print("removeSDFSeries: df_list exists")
      
      stack <- getSIStack() #gets the stack
      stack_length <- getSIStack.length()
      
      if(stack_length == 0){
        #if when the input parameters were removed from the stack, all the parameters were removed
        #because the removed ones were the only parameters present
        setSDFList(NULL) #sets the list to NULL so it can be reinitialized
        single_df_output$data <- single_pps_data
        
        print("removeSDFSeries: The stack_length was zero")
        
      }
      else{
        #if there is still a stack present
        
        print("removeSDFSeries: The stack_length was NOT zero")
        
        input_values <- getSIVector() #gets the input_values
        df_list <- getSDFList() #gets latest df_list
        
        current_parameter <- stack[1] #gets the first parameter in the stack
        current_parameter_value <- input_values[current_parameter] #gets the parameters value
        
        new_df <- generateNewDF(single_pps_data, current_parameter, current_parameter_value) #starts from this initial one
        df_list[[1]] <- new_df #add the new data frame
        
        count <- 2
        
        while (count < stack_length + 1){
          
          previous_df <- df_list[[count-1]]
          current_parameter <- stack[count]
          current_parameter_value <- input_values[current_parameter]
          
          new_df <- generateNewDF(previous_df, current_parameter, current_parameter_value)
          count <- count + 1
          df_list[[count]] <- new_df #add the new data frame
        }#end while creating new DFs
        
        if (count < (length(df_list) + 1)){
          #'because we recreate the df_list from the original df_list and th new df_list will not
          #'have the dataframes from the removed parameters, this new df_list will be shorter than
          #'the original before the parameters were removed. Thus we resize the df_list
          
          while ((length(df_list) + 1) > count){
            #the df_list will continually be resized as the inputs are made null, thus the condition
            #is met when all the element greater than an index of count are removed
            
            df_list[[count]] <- NULL
            
          } #end while for making the elements NULL
          
          
        }#end if for the count compared to the df_list length
        
        setSDFList(df_list) #set the new list
        single_df_output$data <- new_df #set the data table to this
        
      }#end if-else for the stack length
        
    }
    else{
      #there was an error and the list did not exist when it should have because it observed a
      #unchecked checkbox and the stack length was not zero
      print("The df_list does not exist but you attempted to remove something from it")
      stopApp()
    }#end if else for the list existing
      
    
  } #end removeSDFSeries
  
  resetSI <- function(checkbox_name){
    #this resets the inputs for a checkbox
    
    print("resetSI: Resetting the Input Values")
    print(paste0("resetSI: checkbox_name is - ", checkbox_name))
    
    grepname <- gsub("\\(", "\\\\(", checkbox_name)
    grepname <- gsub("\\)", "\\\\)", grepname)
    grepname <- paste0("^", grepname) #must start with it to avoid mis matches
    
    inputs <- names(isolate(single_inputs()))
    input_ids <- getSIIDVector()
    original_inputs <- getOriginalSIVector()
    print(original_inputs)
    
    input_indices <- grep(grepname, inputs)
    
    print(paste0("resetSI: input_indices is: ", input_indices))
    
    count <- 1
    
    while (count < length(input_indices) + 1){
      #goes through all the input_indices that matches
      
      print("resetSI: In the while loop.")
      
      index <- input_indices[count]
      input_name <- inputs[index]
      id <- input_ids[index]
      value <- original_inputs[[index]]
      
      print(paste0("resetSI: index is: ", index))
      print(paste0("resetSI: input_name is: ", input_name))
      print(paste0("resetSI: id is: ", id))
      print(paste0("resetSI: value is: ", value))
      
      if (length(grep(" min", input_name, ignore.case = TRUE)) > 0){
        print("resetSI: In the min")
        updateNumericInput(session,
                          inputId = id,
                          label = NULL,
                          value = value
                          )
      }
      else if (length(grep(" max", input_name, ignore.case = TRUE)) > 0){
        print("resetSI: In the max")
        updateNumericInput(session,
                          inputId = id,
                          label = NULL,
                          value = ""
                          )
      }
      else if (length(grep(paste(c("number", "description", "print"), collapse = "|"), 
                           input_name, ignore.case = TRUE)) > 0){
        updateTextInput(session,
                        inputId = id,
                        label = NULL,
                        value = value
        )
      }
      else{
        print("resetSI: In the else")
        updateSelectInput(session, 
                        id,
                        label = NULL,
                        choices = c("All",unique(as.character(single_pps_data[,checkbox_name]))),
                        selected =  "All"
                        )
      }#end if-else for grep the input_name
      
      count <- count + 1
    }#end while for length of input_indices
    
    
  }#end resetSI
  
  single_df_output <- reactiveValues(data = single_pps_data) #end reactive for single_df_output
  
  clean_single_pps_data <- reactiveValues(data = NA) #the clean data to be downloaded
  
  observeEvent(single_inputs(),{
    #'This will observe if any of the inputs of the parameters for single extrusion have changed
    #'It does not check to see if the input has been selected, but rather, if the user has changed
    #'the search input.'
    
    print("observeEvent(single_inputs()): Input Observed")
    
    
    if (exists("sivector", e1)){
      #checks to see if the vector has been created yet. This is to prevent the initialization
      #of the program
      
      old_sivector <- getSIVector() #get the old sivector before inputs were changed
      current_sivector <- single_inputs()
      setSIVector(current_sivector) #updates the sivector with the new inputs
      
      #produces a vector fo TRUE and FALSE. There should be one element that is different and
      #grep 'FALSE' will find that index.
      index_differ <- grep("FALSE", (current_sivector == old_sivector))
      
      print(paste0("observeEvent(single_inputs()): The index differ is ", index_differ))
      
      if (length(index_differ) == 0){
        #Nothing will be analyzed
        print("observeEvent(single_inputs()): A parameter was changed but the value was changed to what the previous value was")
      }
      else if (length(index_differ) > 1){
        #this means the values are being reset because a checkbox was unchecked for a max and min value
        print("observeEvent(single_inputs()): The checkbox was unchecked")
        #then do nothing because the values should not go in the stack
        
      }
      else{
        
        index_name <- names(current_sivector[index_differ])
        value <- current_sivector[index_differ] #gets the value of the new parameter
        
        if (is.null(value) || is.na(value)){
          #Nothing will be analyzed
          print("The value is null or na")
        }
        else{
          addSIStack(index_name) #adds the name to the stack
          current_sistack <- getSIStack() #gets the newstack
          
          #This needs to be updated because of the parenthese
          updated_index_name <- gsub("\\(", "\\\\(", index_name)
          updated_index_name <- gsub("\\)", "\\\\)", updated_index_name)
          updated_index_name <- paste0("^", updated_index_name) #must start with it to avoid mis matches
          
          stack_index <- grep(updated_index_name, current_sistack) #gets the index in the stack
          
          if (length(stack_index) != 1){
            print("The Stack Index has a length != 0")
            print(paste0("The Stack Index is: ", stack_index))
            stopApp() #terminate the program
          }
          else{
            addSDFSeries(stack_index, index_name, value) #updates the df_list and sets the datatable output df
          } #end if-else for the length of the stack index
          
        }#end if-else for the value being na or null
        
      } #end if-else for the length of index_differ
      
    }
    else{
      #if it has not been created, it sets the vector and stack
      #this is mainly to initialize data. The df_list does not need to be initialize as that is done
      #in the addSDFSeries()
      
      setSIVector(single_inputs())
      setSIStack(c()) #creates an empty stack since no parameters have been changed yet
    }
  })#end observeEvent for the user inputs

  observeEvent(show_vars1(),{
    #' this function will observe when a user checks or unchecks an input. If the input is unchecked,
    #' it removes any of the data cleaning it did in the stack and also resets the value of the
    #' input to the initialized value from the start of the session

    print("observeEvent(show_vars1): Checkbox Observed")

    if (exists("scbvector", e1)){
      
      print("observeEvent(show_vars1): The scbvector Exists")

      old_scbvector <- getSCBVector() #get the old scbvector before inputs were changed
      current_scbvector <- show_vars1()
      setSCBVector(current_scbvector) #updates the scbvector with the new inputs

      #produces a vector fo TRUE and FALSE. There should be one element that is different and
      #grep 'FALSE' will find that index.
      index_differ <- grep("FALSE", (current_scbvector == old_scbvector))

      if (length(index_differ) == 0){
        #Nothing will be analyzed since the value was not changed
      }
      else if (length(index_differ) > 1){
        #multiple selected or deselected, so do nothing
      }
      else{
        
        print("observeEvent(show_vars1): The Checkbox Index_Differ Worked")

        index_name <- names(current_scbvector[index_differ])
        value <- current_scbvector[index_differ] #gets the value of the new parameter

        if (is.null(value) || is.na(value)){
          #Nothing will be analyzed
          print("observeEvent(show_vars1): The checkbox value is null or na")
          stopApp()
        }
        else if (value == FALSE){
          #this ensures that the checkbox was unchecked instead of checked
          
          print("observeEvent(show_vars1): The Value of the Checkbox was FALSE")
          
          current_sistack <- getSIStack() #gets the newstack

          #This needs to be updated because of the parenthese
          updated_index_name <- gsub("\\(", "\\\\(", index_name)
          updated_index_name <- gsub("\\)", "\\\\)", updated_index_name)
          updated_index_name <- paste0("^", updated_index_name)

          stack_index <- grep(updated_index_name, current_sistack) #gets the index in the stack

          if (length(stack_index) == 0){
            print("observeEvent(show_vars1): The Input was not in the Stack")
            #the input that was unchecked is not in the stack, so do nothing
          }
          else if (length(stack_index) == 1 || length(stack_index) == 2){
            #the parameter that was unchecked is in the stack
            #it can be of length 1 or 2 depending of the input parameter had a min and max value
            #if it is a selectize input, there will only be one match (length of 1)
            
            print("observeEvent(show_vars1): The Input was in the Stack")
            print(paste0("observeEvent(show_vars1): The Checkboxname is: ", index_name))
            
            resetSI(index_name) #resets the values of the inputs of the checkbox
            removeSIStack(index_name)
            removeSDFSeries() #removes the inputs associated with the checkbox
          }
          else{
            #none of the conditions were met, most likely multiple matches greater than 2
            print("observeEvent(show_vars1): The Stack Index for the Checkboxes ObserveEvent has a length > 2 or negative")
            stopApp()
          } #end if-else for the length of the stack index

        }
        else{
          #if it is not false, do nothing
          print("observeEvent(show_vars1): The Checkbox value was TRUE")
        }#end if-else for the value being na or null

      } #end if-else for the length of index_differ

    }
    else{
      #if it has not been created, it sets the vector
      #this is mainly to initialize data.

      setOriginalSIVector(single_inputs()) #initialize the original values
      setSCBVector(show_vars1()) #initialize the input values that will be changing with the app

    } #end if-else for scbvector existing


  })#end observeEvent for the checkboxes



  # obtain the output of checkbox from functions and make a list to store them----Single Extrusion PPS Data
  show_vars1 <- reactive({
    #'Placholder' is for the action buttons. It is fiven a value of true so it is always displayed
    checkboxes <- as.numeric(c(TRUE, input$PCSPN_d,input$PCSPD_d,input$PCSRN_d,input$PCSRD_d,input$PCSPPSN_d,
                               input$PCSRF_d, input$PCSRBQ_d, input$PCSRPBQ_d, input$PCSRFQ_d, 
                               input$PCSRFi_d, input$PCSRCQ_d, input$PCSRC_d, input$PCSRRQ_d,
                               input$PCSRDu_d, input$PCSRADu_d,
                               input$PCSDS_d,input$PCSDLL_d,input$PCSTS_d,input$PCSTLL_d,input$PCSSP_d,
                               input$PCSFT_d,input$PCSBZT1_d,input$PCSBZT2_d,input$PCSBZT3_d,
                               input$PCSCT_d,input$PCSAT_d,input$PCSDT1_d,input$PCSDT2_d,
                               input$PCSIDI_d,input$PCSODI_d,input$PCSWT_d,input$PCSOR_d,
                               input$PCSCCT_d,input$PCSLength_d,input$PCSPPD_d,input$PCSNEXIV_d,
                               input$PCSAnnealed_d,input$PCSCaliper_d,input$PCSOS_d,input$PCSMP_d,
                               input$PCSHT_d,input$PCSSPD_d,input$PCSSLD_d,input$PCSDLN_d,
                               input$PCSULT_d,input$PCSVC_d,input$PCSIRD_d))

    names(checkboxes) <- c("Placeholder", "Part Number", "Part Description", "Resin Number", "Resin Description",
                           "PPS Number",
                           "Is Resin Filled?", "Resin Fillers", "Is Resin Colored?", 
                           "Resin Color", "Is Resin Radiopaque?", "Resin Durometer (D)", 
                           "Average Resin Durometer (D)",
                           "Die Size (in)", "Die Land Length (in)",
                           "Tip Size (in)", "Tip Land Length (in)", "Screw Print",
                           "Feedthroat Temperature  F",
                           "Barrel Zone 1 Temperature  F", "Barrel Zone 2 Temperature  F",
                           "Barrel Zone 3 Temperature  F","Clamp Temperature  F",
                           "Adapter Temperature  F","Die 1 Temperature  F", "Die 2 Temperature  F",
                           "Inner Diameter (in)", "Outer Diameter (in)",
                           "Wall Thickness (in)", "Out of Roundness (in)",
                           "Concentricity (in)", "Length (in)", "Perpendicularity (in)",
                           "Nexiv", "Annealed", "Caliper", "OD Sort", "Melt Pump", "Hypo Tip",
                           "Sparker Die", "Slicking Die", "Delamination", "Ultrasonic",
                           "Vacuum Calibration", "Irradiated")

    return(checkboxes)

  })
  
  #this variable will store all the inputs of the single extrusions
  single_inputs <- reactive({
    #this variable will store all the inputs of of the single extrusions
    #'Placholder' is for the action buttons and resin information
    inputs <- c("Placeholder", input$PCSPN, input$PCSPD, input$PCSRN, input$PCSRD, input$PCSPPSN,
                "Placeholder", "Placeholder", "Placeholder", "Placeholder", "Placeholder",
                "Placeholder", "Placeholder", "Placeholder", "Placeholder", "Placeholder",
                   input$PCSDS_min, input$PCSDS_max, input$PCSDLL, input$PCSTS_min, input$PCSTS_max,
                   input$PCSTLL, input$PCSSP, 
                   input$PCSFT_min, input$PCSFT_max, input$PCSBZT1_min, input$PCSBZT1_max,
                   input$PCSBZT2_min, input$PCSBZT2_max, input$PCSBZT3_min, input$PCSBZT3_max,
                   input$PCSCT_min, input$PCSCT_max, input$PCSAT_min, input$PCSAT_max,
                   input$PCSDT1_min, input$PCSDT1_max, input$PCSDT2_min, input$PCSDT2_max,
                   input$PCSIDI_min, input$PCSIDI_max, input$PCSODI_min, input$PCSODI_max,
                   input$PCSWT_min, input$PCSWT_max, input$PCSOR_min, input$PCSOR_max, 
                   input$PCSCCT_min, input$PCSCCT_max, input$PCSLength_min, input$PCSLength_max,
                   input$PCSPPD,
                   input$PCSNEXIV, input$PCSAnnealed, input$PCSCaliper, input$PCSOS,
                   input$PCSMP, input$PCSHT, input$PCSSPD, input$PCSSLD, input$PCSDLN, input$PCSULT,
                   input$PCSVC, input$PCSIRD
                   )
    names(inputs) <- c("Placeholder", "Part Number", "Part Description", "Resin Number", "Resin Description",
                       "PPS Number",
                       "Placeholder", "Placeholder", "Placeholder", "Placeholder", "Placeholder",
                       "Placeholder", "Placeholder", "Placeholder", "Placeholder", "Placeholder",
                       "Die Size (in) Min", "Die Size (in) Max", "Die Land Length (in)", 
                       "Tip Size (in) Min","Tip Size (in) Max", "Tip Land Length (in)", "Screw Print",
                       "Feedthroat Temperature  F Min", "Feedthroat Temperature  F Max",
                       "Barrel Zone 1 Temperature  F Min", "Barrel Zone 1 Temperature  F Max",
                       "Barrel Zone 2 Temperature  F Min", "Barrel Zone 2 Temperature  F Max",
                       "Barrel Zone 3 Temperature  F Min", "Barrel Zone 3 Temperature  F Max",
                       "Clamp Temperature  F Min", "Clamp Temperature  F Max",
                       "Adapter Temperature  F Min", "Adapter Temperature  F Max",
                       "Die 1 Temperature  F Min", "Die 1 Temperature  F Max",
                       "Die 2 Temperature  F Min", "Die 2 Temperature  F Max",
                       "Inner Diameter (in) Min", "Inner Diameter (in) Max", "Outer Diameter (in) Min",
                       "Outer Diameter (in) Max", "Wall Thickness (in) Min", "Wall Thickness (in) Max",
                       "Out of Roudness (in) Min", "Out of Roundness (in) Max", 
                       "Concentricity (in) Min", "Concentricity (in) Max",
                       "Length (in) Min", "Length (in) Max", "Perpendicularity (in", 
                       "Nexiv", "Annealed", "Caliper", "OD Sort", "Melt Pump", "Hypo Tip",
                       "Sparker Die", "Slicking Die", "Delamination", "Ultrasonic",
                       "Vacuum Calibration", "Irradiated")
    return(inputs)
  })
  
  # use all the input values from UI to modify table 1 and show the modified table
  
  
  output$mytable1 <- DT::renderDataTable({
      
      Col_PCS <- which (1 == show_vars1())
      
      data_PCS <- single_df_output$data #the data frame is set
      data_PCS <- data_PCS[,Col_PCS] #only get the columns that have been checked
      
      clean_single_pps_data$data <- data_PCS #assign the clean table to the data that is available
      #for downloading
      
      return(data_PCS)
  },
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',
                                        targets = "_all"
                 )
                 ),
                 scrollX=TRUE,
                 scrollY=600,
                 autoWidth=TRUE),
  rownames = FALSE, 
  escape = FALSE, #escape allows for html elements to be rendered in the table
  server = FALSE) #end Single Extrusion PPS Data
  
  

  
  
  ###
  
  ### The single shopping cart section ###
  
  ###
  
  
  singleshoppingcart <- reactiveValues(
    #this is a shopping cart to hold all the singl extrusion parts and SAP batches that a user wants.
    #this is linked to the output data, so only the output data located of the associated batches 
    #in the shopping cart is displayed
    data = data.frame("Part" = numeric(0), "Delete Part" = numeric(0),
                      "SAP Batch" = numeric(0), "Delete Batch" = numeric(0),
                      stringsAsFactors = FALSE,
                      check.names = FALSE)
  ) #end singleshoppingcart
  
  singleshoppingcartparts <- reactiveValues(
    #'this will hold only a list of parts, this way it is easier for users to look at all the parts
    #'when there are two many batches in the shopping cart.
    data = data.frame("Part" = numeric(0), "Batches?" = numeric(0), "Delete Part" = numeric(0),
                      stringsAsFactors = FALSE,
                      check.names = FALSE)
  )
  
  observeEvent(input$singleadd_button,{
    #this observes whether the user clicked a button to add a part to the shopping cart
    #get the part
    part <- strsplit(input$singleadd_button, "_")[[1]][2]
    print(paste0("observeEvent(input$singleadd_button): ", part))
    
    #Action button to delete part
    deletepart <- as.character(
      actionButton(inputId = paste0("button_", part),
                   label = "Delete Part",
                   onclick = 'Shiny.onInputChange(\"singledelete_part_button\",  this.id)'))
    
    #Get the SAP batches
    SAP_batches <- single_tari_parameter_data$`SAP Batch Number`[single_tari_parameter_data$`Material Number` == part]
    numberofbatches <- length(SAP_batches)
    
    #Action button to delete batch
    #' if there are not batches the part will not be added to this shopping cart table but will
    #' be added to the part
    batch_count <- 1
    vectorofbuttons <- c(rep(0, length(SAP_batches)))
    
    while(batch_count < length(SAP_batches) + 1){
      vectorofbuttons[batch_count] <- as.character(
        actionButton(inputId = paste0("button_", SAP_batches[batch_count]),
                     label = "Delete Batch",
                     onclick = 'Shiny.onInputChange(\"singledelete_batch_button\",  this.id)'))
      batch_count <- batch_count + 1
    }
    
    #Vectors of parts and buttons
    partvector <- rep(part, numberofbatches)
    deletepartvector <- rep(deletepart, numberofbatches)
    
    new_data <- cbind(partvector, deletepartvector, SAP_batches, vectorofbuttons)
    
    colnames(new_data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
    
    if (length(grep(part, singleshoppingcart$data$"Part")) == 0){
      singleshoppingcart$data <- rbind(singleshoppingcart$data, new_data, stringsAsFactors = FALSE)
      colnames(singleshoppingcart$data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
    }
    else{
      #Do nothing if the part is already there
      showModal(modalDialog(
        title = "Add Part Number",
        "The part is already in the shopping cart",
        easyClose = T
      ))
    }
    
    
  })
  
  observeEvent(input$singleadd_button,{
    #this observes whether the user clicked a button to add a part to the part only shopping cart
    part <- strsplit(input$singleadd_button, "_")[[1]][2]
    
    #Action button to delete part
    deletepart <- as.character(
      actionButton(inputId = paste0("button_", part),
                   label = "Delete Part",
                   onclick = 'Shiny.onInputChange(\"singledelete_part_button\",  this.id)'))
    
    
    #This determines if there are batches for the part
    SAP_batches <- single_tari_parameter_data$`SAP Batch Number`[single_tari_parameter_data$`Material Number` == part]
    numberofbatches <- length(SAP_batches)
    
    if(numberofbatches > 0){
      #if there are batches
      batches <- "Yes"
    }
    else{
      batches <- "No"
    }
    
    new_data <- cbind(part, batches, deletepart)
    
    colnames(new_data) <- c("Part", "Batches?", "Delete Part")
    
    
    if (length(grep(part, singleshoppingcartparts$data$"Part")) == 0){
      singleshoppingcartparts$data <- rbind(singleshoppingcartparts$data, new_data, stringsAsFactors = FALSE)
      colnames(singleshoppingcartparts$data) <- c("Part", "Batches?", "Delete Part")
    }
    else{
      #Do nothing if the part is already there
      showModal(modalDialog(
        title = "Add Part Number",
        "The part is already in the shopping cart",
        easyClose = T
      ))
    }
    
  })
  
  observeEvent(input$singleaddtable,{
    #this observes for the button singleaddtable which will add all the parts in the table to the
    #shopping cart.
    
    raw_parts <- unlist(strsplit(unlist(strsplit(single_df_output$data$`Part Number`, ">")), "<"))
    
    clean_parts <- raw_parts[seq(3,length(raw_parts), 4)]
    
    count <- 1
    
    while (count < (length(clean_parts) + 1)){
      #iterates through the parts
      
      part <- clean_parts[count]
      
      #Action button to delete part
      deletepart <- as.character(
        actionButton(inputId = paste0("button_", part),
                     label = "Delete Part",
                     onclick = 'Shiny.onInputChange(\"singledelete_part_button\",  this.id)'))
      
      #Get the SAP batches
      SAP_batches <- single_tari_parameter_data$`SAP Batch Number`[single_tari_parameter_data$`Material Number` == part]
      numberofbatches <- length(SAP_batches)
      
      if(numberofbatches > 0){
        #if there are batches
        batches <- "Yes"
        
        #Action button to delete batch
        #' if there are not batches the part will not be added to this shopping cart table but will
        #' be added to the part
        batch_count <- 1
        vectorofbuttons <- c(rep(0, length(SAP_batches)))
        
        while(batch_count < length(SAP_batches) + 1){
          vectorofbuttons[batch_count] <- as.character(
            actionButton(inputId = paste0("button_", SAP_batches[batch_count]),
                         label = "Delete Batch",
                         onclick = 'Shiny.onInputChange(\"singledelete_batch_button\",  this.id)'))
          batch_count <- batch_count + 1
        }
        
        #Vectors of parts and buttons
        partvector <- rep(part, numberofbatches)
        deletepartvector <- rep(deletepart, numberofbatches)
        
        new_data <- cbind(partvector, deletepartvector, SAP_batches, vectorofbuttons)
        
        colnames(new_data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
        
        if (length(grep(part, singleshoppingcart$data$"Part")) == 0){
          singleshoppingcart$data <- rbind(singleshoppingcart$data, new_data, stringsAsFactors = FALSE)
          colnames(singleshoppingcart$data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
        }
        else{
          #Do nothing if the part is already there
        }
        
      }
      else{
        batches <- "No"
      }
      
      new_data <- cbind(part, batches, deletepart)
      
      colnames(new_data) <- c("Part", "Batches?", "Delete Part")
      
      
      if (length(grep(part, singleshoppingcartparts$data$"Part")) == 0){
        singleshoppingcartparts$data <- rbind(singleshoppingcartparts$data, new_data, stringsAsFactors = FALSE)
        colnames(singleshoppingcartparts$data) <- c("Part", "Batches?", "Delete Part")
      }
      else{
        #Do nothing if the part is already there
      }
      
      count <- count + 1
    }
    
    
  })
    
  
  
  observeEvent(input$singledelete_part_button,{
    #'this observes whether a person deleted a part from the shopping cart. If the button is clicked
    #'all batches associated to the part are removed
    part <- strsplit(input$singledelete_part_button, "_")[[1]][2]
    singleshoppingcart$data <- singleshoppingcart$data[singleshoppingcart$data$'Part' != part,]
    singleshoppingcartparts$data <- singleshoppingcartparts$data[singleshoppingcartparts$data$'Part' != part,]
  })
  
  observeEvent(input$singledelete_batch_button,{
    #'this observes whether a person deleted a SAP batch from the shopping cart. If the button is
    #'clicked, the batch is removed from the cart
    batch <- strsplit(input$singledelete_batch_button, "_")[[1]][2]
    singleshoppingcart$data <- singleshoppingcart$data[singleshoppingcart$data$'SAP Batch' != batch,]
  })
  
  
  output$singleshoppingcart <- renderDataTable({
    #'this shopping cart allows a user to select parts and batches they want to examine. Once added
    #'to the cart, they can view all the MES, SAP, and AppStats data
    return(singleshoppingcart$data)
    },
    filter = "top",
    rownames = FALSE,
    escape = FALSE,
    server = FALSE,
    options = list(orderClasses = TRUE,
                   columnDefs = list(list(className = 'dt-center',targets = "_all")),
                   scrollX=TRUE,
                   scrollY=250,
                   autoWidth=TRUE,
                   pageLength=5)  # make the shopping cart page shorter
  ) #for the shoppingcart
  
  
  output$singleshoppingcartparts <- renderDataTable({
    #'this is a table that only lists the parts for quick viewing
    return(singleshoppingcartparts$data)
  },
  filter = "top",
  rownames = FALSE,
  escape = FALSE,
  server = FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=250,
                 autoWidth=TRUE,
                 pageLength=5)   # make the shopping cart page shorter
  ) #for the shoppingcart
  
  
  output$singledownloadSPPSData <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Visible Single-Layer PPS Data", '.csv', sep='') },
    content = function(file) {
      #I remove the first column so the HTML is not outputed
      output <- clean_single_pps_data$data[2:ncol(clean_single_pps_data$data)]
      
      if (length(grep("Part Number", colnames(output), ignore.case = TRUE) > 0)){
        #if a user has selected the column
        raw_parts <- unlist(strsplit(unlist(strsplit(single_df_output$data$`Part Number`, ">")), "<"))
        clean_parts <- raw_parts[seq(3,length(raw_parts), 4)]
        output$'Part Number' <- clean_parts
      }
      
      if (length(grep("PPS Number", colnames(output), ignore.case = TRUE) > 0)){
        #if a user has selected the column
        raw_pps <- unlist(strsplit(unlist(strsplit(single_df_output$data$`PPS Number`, ">")), "<"))
        clean_pps <- raw_parts[seq(3,length(raw_pps), 4)]
        output$'PPS Number' <- clean_pps
      }
      
      write.csv(output, file, row.names = FALSE)
    }
  )
  
  output$singledownloadSPPSDataAll <- downloadHandler(
    #downlaod the data
    filename = function() { paste("All Single-Layer PPS Data", '.csv', sep='') },
    content = function(file) {
      #I remove the first column so the HTML is not outputed
      output <- single_df_output$data[2:ncol(single_df_output$data)]
      
      raw_parts <- unlist(strsplit(unlist(strsplit(single_df_output$data$`Part Number`, ">")), "<"))
      clean_parts <- raw_parts[seq(3,length(raw_parts), 4)]
      output$'Part Number' <- clean_parts
      
      raw_pps <- unlist(strsplit(unlist(strsplit(single_df_output$data$`PPS Number`, ">")), "<"))
      clean_pps <- raw_parts[seq(3,length(raw_pps), 4)]
      output$'PPS Number' <- clean_pps
      
      
      write.csv(output, file, row.names = FALSE)
    }
  )
  
  output$singleshoppingcartpps <- renderDataTable({
    #this is to render a datatable that has all the PPS information of parts that have been saved
    #to the shopping cart
    
    raw_parts <- unlist(strsplit(unlist(strsplit(single_pps_data$`Part Number`, ">")), "<"))
    clean_parts <- raw_parts[seq(3,length(raw_parts), 4)]
    
    data <- single_pps_data[which(clean_parts %in% singleshoppingcartparts$data$'Part'),2:ncol(single_pps_data)]
    return(data)
    
  },
  filter = "top",
  rownames = FALSE,
  escape = FALSE,
  server = FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE)
  )
  
  
  
  output$singlecartdownloadpps <- downloadHandler(
    #downlaod the single PPS data from the shopping cart
    filename = function() { paste("Single PPS Shopping Cart Data", '.csv', sep='') },
    content = function(file) {
      
      output <- single_pps_data
      
      if (length(grep("Part Number", colnames(output), ignore.case = TRUE) > 0)){
        #removes the html from the part number
        raw_parts <- unlist(strsplit(unlist(strsplit(output$`Part Number`, ">")), "<"))
        clean_parts <- raw_parts[seq(3,length(raw_parts), 4)]
        output$'Part Number' <- clean_parts
      }
      
      if (length(grep("PPS Number", colnames(output), ignore.case = TRUE) > 0)){
        #removes the html from the PPS number
        raw_pps <- unlist(strsplit(unlist(strsplit(output$`PPS Number`, ">")), "<"))
        clean_pps <- raw_parts[seq(3,length(raw_pps), 4)]
        output$'PPS Number' <- clean_pps
      }
      write.csv(output[which(output$`Part Number` %in% singleshoppingcartparts$data$'Part'),2:ncol(single_pps_data)], 
                file, row.names = FALSE)
    }
  )
  
  observeEvent(input$checksingleresininfo,{
    #this checks all the checkboxes associated with single tooling inputs
    updateCheckboxInput(session, inputId = "PCSRF_d", label = "Resin Families",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSRBQ_d", label = "Is Resin Blended with Anything?",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSRPBQ_d", label = "Is Resin a Polymer Blend?",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSRFQ_d", label = "Is Resin Filled?",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSRFi_d", label = "Resin Fillers",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSRCQ_d", label = "Is Resin Colored?",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSRC_d", label = "Resin Color",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSRRQ_d", label = "Is Resin Radiopaque?",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSRDu_d", label = "Resin Durometer (D)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSRADu_d", label = "Average Durometer (D)",value = TRUE)
    
  }) #end obserEvent for input$checksingletooling
  
  observeEvent(input$unchecksingleresininfo,{
    #this unchecks all the checkboxes associated with single tooling inputs
    updateCheckboxInput(session, inputId = "PCSRF_d", label = "Resin Families",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSRBQ_d", label = "Is Resin Blended with Anything?",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSRPBQ_d", label = "Is Resin a Polymer Blend?",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSRFQ_d", label = "Is Resin Filled?",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSRFi_d", label = "Resin Fillers",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSRCQ_d", label = "Is Resin Colored?",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSRC_d", label = "Resin Color",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSRRQ_d", label = "Is Resin Radiopaque?",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSRDu_d", label = "Resin Durometer (D)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSRADu_d", label = "Average Durometer (D)",value = FALSE)
    
  }) #end obserEvent for input$unchecksingletooling
  
  observeEvent(input$checksingletooling,{
    #this checks all the checkboxes associated with single tooling inputs
    updateCheckboxInput(session, inputId = "PCSDS_d", label = "Die Size (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSDLL_d", label = "Die Land Length (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSTS_d", label = "Tip Size (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSTLL_d", label = "Tip Land Length (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSSP_d", label = "Screw Print",value = TRUE)
    
  }) #end obserEvent for input$checksingletooling
  
  
  observeEvent(input$unchecksingletooling,{
    #this unchecks all the checkboxes associated with single tooling inputs
    updateCheckboxInput(session, inputId = "PCSDS_d", label = "Die Size (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSDLL_d", label = "Die Land Length (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSTS_d", label = "Tip Size (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSTLL_d", label = "Tip Land Length (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSSP_d", label = "Screw Print",value = FALSE)
    
  }) #end obserEvent for input$unchecksingletooling
  
  
  observeEvent(input$checksingleparameters,{
    #this checks all the checkboxes associated with single processing parameters inputs
    updateCheckboxInput(session, inputId = "PCSFT_d", label = "Feedthroat Temperature F",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSBZT1_d", label = "Barrel Zone 1 Temperature F",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSBZT2_d", label = "Barrel Zone 2 Temperature F",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSBZT3_d", label = "Barrel Zone 3 Temperature F",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSCT_d", label = "Clamp Temperature F",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSAT_d", label = "Adapter Temperature F",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSDT1_d", label = "Die 1 Temperature F",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSDT2_d", label = "Die 2 Temperature F",value = TRUE)
    
  }) #end obserEvent for input$checksingleparameters
  
  
  observeEvent(input$unchecksingleparameters,{
    #this unchecks all the checkboxes associated with single processing parameters inputs
    updateCheckboxInput(session, inputId = "PCSFT_d", label = "Feedthroat Temperature F",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSBZT1_d", label = "Barrel Zone 1 Temperature F",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSBZT2_d", label = "Barrel Zone 2 Temperature F",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSBZT3_d", label = "Barrel Zone 3 Temperature F",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSCT_d", label = "Clamp Temperature F",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSAT_d", label = "Adapter Temperature F",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSDT1_d", label = "Die 1 Temperature F",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSDT2_d", label = "Die 2 Temperature F",value = FALSE)
    
  }) #end obserEvent for input$unchecksingleparameters
  
  observeEvent(input$checksingledimensions,{
    #this checks all the checkboxes associated with single dimensional attribute inputs
    updateCheckboxInput(session, inputId = "PCSIDI_d", label = "Inner Diameter (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSODI_d", label = "Outer Diameter (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSWT_d", label = "Wall Thickness (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSOR_d", label = "Out of Roundness (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSCCT_d", label = "Concentricity",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSLength_d", label = "Length (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSPPD_d", label = "Perpendicularity (in)",value = TRUE)
    
  }) #end obserEvent for input$checksingledimensions
  
  
  observeEvent(input$unchecksingledimensions,{
    #this unchecks all the checkboxes associated with single dimensional attribute inputs
    updateCheckboxInput(session, inputId = "PCSIDI_d", label = "Inner Diameter (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSODI_d", label = "Outer Diameter (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSWT_d", label = "Wall Thickness (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSOR_d", label = "Out of Roundness (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSCCT_d", label = "Concentricity",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSLength_d", label = "Length (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSPPD_d", label = "Perpendicularity (in)",value = FALSE)
    
  }) #end obserEvent for input$unchecksingledimensions
  
  observeEvent(input$checksinglespecial,{
    #this checks all the checkboxes associated with single special operation inputs
    updateCheckboxInput(session, inputId = "PCSNEXIV_d", label = "NEXIV",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSAnnealed_d", label = "Annealed",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSCaliper_d", label = "Caliper",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSOS_d", label = "OD Sort",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSMP_d", label = "Melt Pump",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSHT_d", label = "Hypo Tip",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSSPD_d", label = "Sparker Die",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSSLD_d", label = "Slicking Die",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSDLN_d", label = "Delamination",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSULT_d", label = "Ultrasonic",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSVC_d", label = "Vacuum Calibration",value = TRUE)
    updateCheckboxInput(session, inputId = "PCSIRD_d", label = "Irradiated",value = TRUE)
    
  }) #end obserEvent for input$checksinglespecial
  
  
  observeEvent(input$unchecksinglespecial,{
    #this unchecks all the checkboxes associated with single special operation inputs
    updateCheckboxInput(session, inputId = "PCSNEXIV_d", label = "NEXIV",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSAnnealed_d", label = "Annealed",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSCaliper_d", label = "Caliper",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSOS_d", label = "OD Sort",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSMP_d", label = "Melt Pump",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSHT_d", label = "Hypo Tip",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSSPD_d", label = "Sparker Die",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSSLD_d", label = "Slicking Die",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSDLN_d", label = "Delamination",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSULT_d", label = "Ultrasonic",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSVC_d", label = "Vacuum Calibration",value = FALSE)
    updateCheckboxInput(session, inputId = "PCSIRD_d", label = "Irradiated",value = FALSE)
    
  }) #end obserEvent for input$unchecksinglespecial
  
  observeEvent(input$resetsingleinputs,{
    #this will reset the single inputs when clicked
    #It first removes all the dataframes that have been created, resets the input values,
    #and then it resets the stack
    setSDFList(NULL) #resetst the DF list
    
    current_inputs <- isolate(single_inputs())
    original_inputs <- getOriginalSIVector()
    
    differ_indices <- which(current_inputs != original_inputs)
    value_names <- names(current_inputs[differ_indices])
    
    print(paste0("observeEvent(input$resetsingleinputs): ",value_names))
    
    count <- 1
    #this while loop iterates through all the values that differed
    while (count < length(value_names) + 1){
      current_name <- value_names[count]
      resetSI(current_name)
      count <- count + 1
    }#end while
    
    setSIStack(c()) #creates an empty stack
    
    #sets the data table to the original data set
    single_df_output$data <- single_pps_data
    
  }) #end obserEvent for input$resetsingleinputs
     
  
  

  #### Multi-Layer PPS ####
  
  generateNewMDF <- function(df, index_name, value){
    #this determines whether the index is max, min, or other. And then it cleans up the df based
    #on that and generates a new clean df
    
    if (length(grep("min", index_name, ignore.case = TRUE)) != 0){
      #if min was found in the index_name
      
      parameter_name <- gsub(" Min", "", index_name)
      parameter_name <- gsub("\\(", "\\\\(", parameter_name)
      parameter_name <- gsub("\\)", "\\\\)", parameter_name)
      parameter_name <- paste0("^", parameter_name) #must start with it to avoid mis matches
      
      column_index <- grep(parameter_name, names(df), ignore.case = TRUE)
      
      middle_df <- df[df[,column_index] >= value,]
      
      part_numbers <- middle_df[,"Part Number"]#then it gets the unique part numbers
      new_df <- df[df[,"Part Number"] %in% part_numbers,]#then it cleans up the original df
      
      return(new_df)
      
    }
    else if(length(grep("max", index_name, ignore.case = TRUE)) != 0){
      #if max was found in the index_name
      
      parameter_name <- gsub(" Max", "", index_name)
      parameter_name <- gsub("\\(", "\\\\(", parameter_name)
      parameter_name <- gsub("\\)", "\\\\)", parameter_name)
      parameter_name <- paste0("^", parameter_name) #must start with it to avoid mis matches
      
      column_index <- grep(parameter_name, names(df), ignore.case = TRUE)
      
      clean_df <- df[df[,column_index] != "", ] #this removes blanks
      
      middle_df <- clean_df[clean_df[,column_index] <= value,]
      
      part_numbers <- middle_df[,"Part Number"]#then it gets the unique part numbers
      new_df <- df[df[,"Part Number"] %in% part_numbers,]#then it cleans up the original df
      
      return(new_df)
      
    }
    else if (length(grep(paste(c("number", "print"), collapse = "|"), 
                         index_name, ignore.case = TRUE)) != 0
             && length(grep("Resin Number", index_name)) == 0){
      #this will catch part number, resin number, resin description, and
      #pps numbers
      
      string_to_search <- gsub("\\| ", "|", gsub(";", "|", value))
      #this replaces semi-colons with a or for grep. It also removes spaces for example:
      #"90161; 123124;435 -> 90161|123124|435
      
      string_to_search <- cleanStringSearch(string_to_search) #handles special characters
      
      column_index <- grep(paste0("^",index_name), names(df), ignore.case = TRUE)
      
      raw_parts <- unlist(strsplit(unlist(strsplit(df[,column_index], ">")), "<"))
      
      clean_parts <- raw_parts[seq(3,length(raw_parts), 4)]
      
      middle_df <- df[grep(string_to_search, clean_parts, ignore.case = TRUE),]
      
      part_numbers <- middle_df[,"Part Number"]#then it gets the unique part numbers
      new_df <- df[df[,"Part Number"] %in% part_numbers,]#then it cleans up the original df
      
      return(new_df)
      
    }
    else if(length(grep(paste(c("description", "Resin Number"), collapse = "|"), 
                        index_name, ignore.case = TRUE)) != 0){
      #catches description because we cannot string split
      
      string_to_search <- gsub("\\| ", "|", gsub(";", "|", value))
      #this replaces semi-colons with a or for grep. It also removes spaces for example:
      #"90161; 123124;435 -> 90161|123124|435
      
      string_to_search <- cleanStringSearch(string_to_search) #handles special characters
      
      
      column_index <- grep(paste0("^",index_name), names(df), ignore.case = TRUE)
      
      middle_df <- df[grep(string_to_search, df[,column_index], ignore.case = TRUE),]
      
      part_numbers <- middle_df[,"Part Number"]#then it gets the unique part numbers
      new_df <- df[df[,"Part Number"] %in% part_numbers,]#then it cleans up the original df
      
      return(new_df)
      
      
    }
    else{
      #this is not a min or a max, but instead is a select input
      
      parameter_name <- index_name
      parameter_name <- gsub("\\(", "\\\\(", parameter_name)
      parameter_name <- gsub("\\)", "\\\\)", parameter_name)
      parameter_name <- paste0("^", parameter_name) #must start with it to avoid mis matches
      
      column_index <- grep(parameter_name, names(df), ignore.case = TRUE)
      
      print(parameter_name)
      print(column_index)
      
      if (value == "All"){
        #if 'All' is selected
        new_df <- df
      }
      else{
        #a specific value was selected
        middle_df <- df[df[,column_index] == value,] #first it searches by the parameters
        part_numbers <- middle_df[,"Part Number"]#then it gets the unique part numbers
        new_df <- df[df[,"Part Number"] %in% part_numbers,]#then it cleans up the original df
      }
      
      return(new_df)
      
    }#end if-else on the types of parameters
    
  }#end generateNewMDf
  
  e2 <- new.env(
    #This environment will store variable of inputs and stack that are used for comparison
    #of the input parameters that have been selected and changed
    #' variables to contain:
    #' mivector - a vector that will store the inputs of the multi extrusion parameters
    #' mistack - the stack for the multi inputs
  ) #creates a new environment to store instance variables
  
  #the assign will initialize the miidvector
  assign("miidvector", 
         c("Placeholder", "PCMPN", "PCMPD", "PCMRN", "PCMRD", "PCMPPSN", "Placeholder",
           "Placeholder", "Placeholder", "Placeholder", "Placeholder", "Placeholder",
           "Placeholder", "Placeholder", "Placeholder", "Placeholder", "Placeholder",
           "Placeholder",
           "PCMDS_min", "PCMDS_max", "PCMDLL", "PCMTS_min", "PCMTS_max",
           "PCMTLL", "PCMSP", 
           "PCMFT_min", "PCMFT_max", "PCMBZT1_min", "PCMBZT1_max",
           "PCMBZT2_min", "PCMBZT2_max", "PCMBZT3_min", "PCMBZT3_max",
           "PCMCT_min", "PCMCT_max", "PCMAT_min", "PCMAT_max",
           "PCMDT1_min", "PCMDT1_max", "PCMDT2_min", "PCMDT2_max",
           "PCMIDI_min", "PCMIDI_max", "PCMODI_min", "PCMODI_max",
           "PCMIWT_min", "PCMIWT_max", "PCMMWT_min", "PCMMWT_max", 
           "PCMOWT_min", "PCMOWT_max", "PCMTWT_min", "PCMTWT_max", 
           "PCMOR_min", "PCMOR_max", 
           "PCMCCT_min", "PCMCCT_max", "PCMLength_min", "PCMLength_max",
           "PCMToLength_min", "PCMToLength_max",
           "PCMPPD",
           "PCMNEXIV", "PCMAnnealed", "PCMCaliper", "PCMOS",
           "PCMMP", "PCMHT", "PCMSPD", "PCMSLD", "PCMDLN", "PCMULT",
           "PCMVC", "PCMIRD"), 
         envir = e2)
  
  
  ## These are the setter and getter function ##
  
  setMIIDVector <- function(vector){
    #set the vector for the input ids
    
    assign("miidvector", vector, env = e2)
    
  } #end setMIIDVector
  
  getMIIDVector <- function(){
    #returns the IDs of the inputs
    
    if(exists("miidvector", e2)){
      return(get("miidvector", e2))
    }
    else{
      return(NA)
    }
  } #end getMIIDVector
  
  setMCBVector <- function(vector){
    #this sets the values for the multi input vector
    assign("mcbvector", vector, env = e2)
  } #end setMCVector
  
  getMCBVector <- function(vector){
    #this gets the values for the multi checkbox vector
    
    if(exists("mcbvector", e2)){
      return(get("mcbvector", e2))
    }
    else{
      return(NA)
    }
    
  } #end getMCBVector
  
  setOriginalMIVector <- function(vector){
    #this sets the values for the original min and max input values
    assign("originalmivector", vector, env = e2)
  } #end setOriginalMIVector
  
  getOriginalMIVector <- function(vector){
    #this gets the values for the original min and max input values
    
    if(exists("originalmivector", e2)){
      return(get("originalmivector", e2))
    }
    else{
      return(NA)
    }
    
  } #end getOriginalMIVector
  
  setMIVector <- function(vector){
    #this sets the values for the multi input vector
    assign("mivector", vector, env = e2)
  } #end setMIVector
  
  getMIVector <- function(){
    #returns mivector
    
    if(exists("mivector", e2)){
      return(get("mivector", e2))
    }
    else{
      return(NA)
    }
    
  } #end getMIVector
  
  setMIStack <- function(stack){
    #this sets the stack for the multi inputs
    assign("mistack", stack, env = e2)
  } #end setMIStack
  
  getMIStack <- function(){
    #returns mivector
    if(exists("mistack", e2)){
      return(get("mistack", e2))
    }
    else{
      return(NA)
    }
    
  } #end getMIStack
  
  getMIStack.length <- function(){
    #returns the length of the SIStack
    
    if(exists("mistack", e2)){
      #if it exists, return the length
      return(length(getMIStack()))
    }
    else{
      #else return NA
      return(NA)
    }
    
  }#end getMIStack.length
  
  addMIStack <- function(name){
    #this function adds a new parameter to the stack
    #it will only add to the stack if it is a unique name
    
    stack <- get("mistack", e2)
    
    
    #edit name
    rep_name <- gsub("\\(", "\\\\(", name)
    rep_name <- gsub("\\)", "\\\\)", rep_name)
    rep_name <- paste0("^", rep_name) #must start with it to avoid mis matches
    
    if (length(grep(rep_name, stack)) == 0){
      #the name is not currently in the stack
      stack <- c(stack, name) #add the name to the top of the stack (the right most)
      setMIStack(stack) #set the stack
    }
    else{
      #do nothing because the name is already present
    }
    
  } #end  addMIStack
  
  removeMIStack <- function(name){
    #this function removes the inputs associated with the checkbox name from the stack
    
    print("removeMIStack: We are in the removeMIStack")
    
    stack <- get("mistack", e2)
    
    
    #edit name
    rep_name <- gsub("\\(", "\\\\(", name)
    rep_name <- gsub("\\)", "\\\\)", rep_name)
    rep_name <- paste0("^", rep_name) #must start with it to avoid mis matches
    
    if (length(grep(rep_name, stack)) != 0){
      #the name is not currently in the stack
      indices <- grep(rep_name, stack)
      
      if (length(indices) == length(stack)){
        #if the length of the indices are equal to the stack
        print("removeMIStack: The length of the indices are equal to the stack")
        stack <- c()
      }
      else if (length(indices) == 1){
        print("removeMIStack: There was only one match in the stack")
        #if there is only one match
        if (indices == 1){
          #if the first matches is 1 or the only match is one
          stack <- stack[c(2:length(stack))] # removes the first element
        }
        else if (indices == length(stack)){
          stack <- stack[c(1:(indices - 1))]
        }
        else{
          #removes the index from the stack
          stack <- stack[c(1:(indices-1), (indices + 1):length(stack))]
        }#end if-else for the vallue of indices
      }
      else if (length(indices) == 2){
        print("removeMIStack: There were multiple matches to the stack")
        #if there are multiple indices
        if (indices[1] == 1){
          #if the first matches is 1 or the only match is one
          
          if (indices[2] == 2){
            stack <- stack[c(3:length(stack))] # removes the first and second element
          }
          else if (indices[2] == length(stack)){
            stack <- stack[c(2:(indices[2] - 1))]
          }
          else{
            stack <- stack[c(2:(indices[2] - 1), (indices[2] + 1):length(stack))] # the indices
          }#end if-else for the value of the second index
          
        }
        else if(indices[1] == (length(stack) - 1)){
          #if the first index is length(stack)-1, then the second index is the length of the stack
          stack <- stack[c(1:(indices[1] - 1))]
        }
        else if (indices[1] == (indices[2] + 1)){
          #if the indices are right next to each other but are not at the ends of the stack
          stack <- stack[c(1:(indices[1]-1), (indices[2] + 1):length(stack))]
        }
        else{
          #removes the indices from the stack
          stack <- stack[c(1:(indices[1]-1), (indices[1] + 1):(indices[2] - 1), (indices[2] + 1):length(stack))]
        }#end if-else for the value of indices
        
      }#end if-else for the length of indices
      else{
        #if it was neither, there was an error and the app shoudl stop
        print("The indices in the removeMIStack were more than 2")
        print(paste0("The indices are: ", indices))
        stop()
      }
      
      
      setMIStack(stack) #set the stack
    }
    else{
      #do nothing because the name is not in the stack
      print("removeMIStack: Nothing was done because the name was not in the stack")
    }
    
  } #end  removeMIStack
  
  setMDFList <- function(list){
    #sets the DFList
    assign("df_list", list, env = e2)
  }#end setMDFList
  
  getMDFList <- function(){
    #returns the df_list
    if(exists("df_list", e2)){
      #if it exists, return the length
      return((get("df_list", e2)))
    }
    else{
      #else return NA
      return(NA)
    }
    
  }#end getMDFList
  
  addMDFSeries <- function(index, index_name, value){
    #this creates the series of successively more narrow data frames based on the number of parameters
    #the user has searched by. 3 parameters means there is the original data frame and three ones
    #that have been successively narrowed.
    
    if (exists("df_list", e2)){
      #if the list already exists
      
      if(index == getMIStack.length() + 1){
        #if the index is larger than the list length, we have a new parameter that will be added
        df_list <- getMDFList() #gets latest df_list
        latest_df <- df_list[[index-1]] #the index should only be 1 greater than the list
        
        new_df <- generateNewMDF(latest_df, index_name, value)
        df_list[[index]] <- new_df #add the new data frame
        setMDFList(df_list) #set the new list
        
        multi_df_output$data <- new_df #set the data table to this
        
      }
      else if(index > getMIStack.length() + 1){
        #if there was an error in the index and it was too large
        print("The index for the stack was too large and greater than the allowable limit")
        print(paste0("The stack index is: ", index, ". The stack length is: ", getMIStack.length(), "."))
        stopApp()
      }
      else if(index < 1){
        #error in the index
        print("The index for the stack was zero or negative")
        print(paste0("The stack index is: ", index, ". The stack length is: ", getMIStack.length(), "."))
        stopApp()
      }
      else{
        #the index is within the stack which means we means a previous parameter is being changed
        stack <- getMIStack()
        stack_length <- getMIStack.length()
        
        input_values <- getMIVector()
        
        df_list <- getMDFList() #gets latest df_list
        
        if (index == 1){
          #if it is starting from the beginning
          
          new_df <- generateNewMDF(multi_pps_data, index_name, value) #starts from this initial one
          df_list[[index]] <- new_df #add the new data frame
          
          count <- index + 1
          
          while (count < stack_length + 1){
            
            previous_df <- df_list[[count-1]]
            current_parameter <- stack[count]
            current_parameter_value <- input_values[current_parameter]
            
            new_df <- generateNewMDF(previous_df, current_parameter, current_parameter_value)
            count <- count + 1
            df_list[[count]] <- new_df #add the new data frame
          }#end while creating new DFs
          
          setMDFList(df_list) #set the new list
          multi_df_output$data <- new_df #set the data table to this
          
        }
        else{
          
          count <- index
          
          while (count < stack_length + 1){
            
            previous_df <- df_list[[count-1]] #gets the previous df
            current_parameter <- stack[count]
            current_parameter_value <- input_values[current_parameter]
            
            new_df <- generateNewMDF(previous_df, current_parameter, current_parameter_value)
            
            df_list[[count]] <- new_df #add the new data frame
            
            count <- count + 1
          }#end while creating new DFs
          
          setMDFList(df_list) #set the new list
          multi_df_output$data <- new_df #set the data table to this
          
        }#end if-else for index value
        
      }#end if else for the index length
      
    }
    else{
      assign("df_list", list("hello", "hi"), env = e2) #if the list does not yet exist, it creates it
      #creates two dummy variables otherwise the list does not initialize correctly
      
      new_df <- generateNewMDF(multi_pps_data, index_name, value) #starts from this initial one
      df_list <- getMDFList() #gets latest df_list
      df_list[[index]] <- new_df #add the new data frame
      df_list[[2]] <- NULL #removes the previous dummy variable
      
      setMDFList(df_list) #set the new list
      multi_df_output$data <- new_df #set the data table to this
      
    }#end if-else for the df_list existing
    
  } #end addMDFSeries
  
  removeMDFSeries <- function(){
    #this removes the data frames that were in the df_list because of the parameters. It is called
    #when a checkbox is unchecked and one of the parameters were present
    
    print("removeMDFSeries: We are in removeMDFSeries")
    
    if (exists("df_list", e2)){
      #if the list already exists
      
      print("removeMDFSeries: df_list exists")
      
      stack <- getMIStack() #gets the stack
      stack_length <- getMIStack.length()
      
      if(stack_length == 0){
        #if when the input parameters were removed from the stack, all the parameters were removed
        #because the removed ones were the only parameters present
        setMDFList(NULL) #sets the list to NULL so it can be reinitialized
        multi_df_output$data <- multi_pps_data
        
        print("removeMDFSeries: The stack_length was zero")
        
      }
      else{
        #if there is still a stack present
        
        print("removeMDFSeries: The stack_length was NOT zero")
        
        input_values <- getMIVector() #gets the input_values
        df_list <- getMDFList() #gets latest df_list
        
        current_parameter <- stack[1] #gets the first parameter in the stack
        current_parameter_value <- input_values[current_parameter] #gets the parameters value
        
        new_df <- generateNewMDF(multi_pps_data, current_parameter, current_parameter_value) #starts from this initial one
        df_list[[1]] <- new_df #add the new data frame
        
        count <- 2
        
        while (count < stack_length + 1){
          
          previous_df <- df_list[[count-1]]
          current_parameter <- stack[count]
          current_parameter_value <- input_values[current_parameter]
          
          new_df <- generateNewMDF(previous_df, current_parameter, current_parameter_value)
          count <- count + 1
          df_list[[count]] <- new_df #add the new data frame
        }#end while creating new DFs
        
        if (count < (length(df_list) + 1)){
          #'because we recreate the df_list from the original df_list and th new df_list will not
          #'have the dataframes from the removed parameters, this new df_list will be shorter than
          #'the original before the parameters were removed. Thus we resize the df_list
          
          while ((length(df_list) + 1) > count){
            #the df_list will continually be resized as the inputs are made null, thus the condition
            #is met when all the element greater than an index of count are removed
            
            df_list[[count]] <- NULL
            
          } #end while for making the elements NULL
          
          
        }#end if for the count compared to the df_list length
        
        
        setMDFList(df_list) #set the new list
        multi_df_output$data <- new_df #set the data table to this
        
      }#end if-else for the stack length
      
    }
    else{
      #there was an error and the list did not exist when it should have because it observed a
      #unchecked checkbox and the stack length was not zero
      print("The df_list does not exist but you attempted to remove something from it")
      stopApp()
    }#end if else for the list existing
    
    
  } #end removeMDFSeries
  
  resetMI <- function(checkbox_name){
    #this resets the inputs for a checkbox
    
    print("resetMI: Resetting the Input Values")
    print(paste0("resetMI: checkbox_name is - ", checkbox_name))
    
    grepname <- gsub("\\(", "\\\\(", checkbox_name)
    grepname <- gsub("\\)", "\\\\)", grepname)
    grepname <- paste0("^", grepname) #must start with it to avoid mis matches
    
    inputs <- names(isolate(multi_inputs()))
    input_ids <- getMIIDVector()
    original_inputs <- getOriginalMIVector()
    print(original_inputs)
    
    input_indices <- grep(grepname, inputs)
    
    print(paste0("resetMI: input_indices is: ", input_indices))
    
    count <- 1
    
    while (count < length(input_indices) + 1){
      #goes through all the input_indices that matches
      
      print("resetMI: In the while loop.")
      
      index <- input_indices[count]
      input_name <- inputs[index]
      id <- input_ids[index]
      value <- original_inputs[[index]]
      
      print(paste0("resetMI: index is: ", index))
      print(paste0("resetMI: input_name is: ", input_name))
      print(paste0("resetMI: id is: ", id))
      print(paste0("resetMI: value is: ", value))
      
      if (length(grep(" min", input_name, ignore.case = TRUE)) > 0){
        print("resetMI: In the min")
        updateNumericInput(session,
                           inputId = id,
                           label = NULL,
                           value = value
        )
      }
      else if (length(grep(" max", input_name, ignore.case = TRUE)) > 0){
        print("resetMI: In the max")
        updateNumericInput(session,
                           inputId = id,
                           label = NULL,
                           value = value
        )
      }
      else if (length(grep(" max", input_name, ignore.case = TRUE)) > 0){
        print("resetSI: In the max")
        updateNumericInput(session,
                           inputId = id,
                           label = NULL,
                           value = ""
        )
      }
      else{
        print("resetMI: In the else")
        updateSelectInput(session, 
                          id,
                          label = NULL,
                          choices = c("All",unique(as.character(multi_pps_data[,checkbox_name]))),
                          selected =  "All"
        )
      }#end if-else for grep the input_name
      
      count <- count + 1
    }#end while for length of input_indices
    
    
  }#end resetMI
  
  multi_df_output <- reactiveValues(data = multi_pps_data) #end reactive for multi_df_output
  
  clean_multi_pps_data <- reactiveValues(data = NA) #this is the data to be downloaded
  
  observeEvent(multi_inputs(),{
    #'This will observe if any of the inputs of the parameters for multi extrusion have changed
    #'It does not check to see if the input has been selected, but rather, if the user has changed
    #'the search input.'
    
    print("observeEvent(multi_inputs()): Input Observed")
    
    if (exists("mivector", e2)){
      #checks to see if the vector has been created yet. This is to prevent the initialization
      #of the program
      
      old_mivector <- getMIVector() #get the old mivector before inputs were changed
      current_mivector <- multi_inputs()
      setMIVector(current_mivector) #updates the mivector with the new inputs
      
      #produces a vector fo TRUE and FALSE. There should be one element that is different and
      #grep 'FALSE' will find that index.
      index_differ <- grep("FALSE", (current_mivector == old_mivector))
      
      if (length(index_differ) == 0){
        #Nothing will be analyzed
        print("A parameter was changed but the value was changed to what the previous value was")
        
      }
      else if (length(index_differ) > 1){
        #this means the values are being reset because a checkbox was unchecked for a max and min value
        print("observeEvent(multi_inputs()): The checkbox was unchecked")
        #then do nothing because the values should not go in the stack
        
      }
      else{
        
        index_name <- names(current_mivector[index_differ])
        value <- current_mivector[index_differ] #gets the value of the new parameter
        
        if (is.null(value) || is.na(value)){
          #Nothing will be analyzed
          print("The value is null or na")
        }
        else{
          addMIStack(index_name) #adds the name to the stack
          current_mistack <- getMIStack() #gets the newstack
          
          #This needs to be updated because of the parenthese
          updated_index_name <- gsub("\\(", "\\\\(", index_name)
          updated_index_name <- gsub("\\)", "\\\\)", updated_index_name)
          updated_index_name <- paste0("^", updated_index_name) #must start with it to avoid mis matches
          
          stack_index <- grep(updated_index_name, current_mistack) #gets the index in the stack
          
          if (length(stack_index) != 1){
            print("The Stack Index has a length != 0")
            print(paste0("The Stack Index is: ", stack_index))
            stopApp() #terminate the program
          }
          else{
            addMDFSeries(stack_index, index_name, value) #updates the df_list and sets the datatable output df
          } #end if-else for the length of the stack index
          
        }#end if-else for the value being na or null
        
      } #end if-else for the length of index_differ
      
    }
    else{
      #if it has not been created, it sets the vector and stack
      #this is mainly to initialize data. The df_list does not need to be initialize as that is done
      #in the addMDFSeries()
      
      setMIVector(multi_inputs())
      setMIStack(c()) #creates an empty stack since no parameters have been changed yet
    }
  })#end observeEvent for the user inputs
  
  observeEvent(show_vars2(),{
    #' this function will observe when a user checks or unchecks an input. If the input is unchecked,
    #' it removes any of the data cleaning it did in the stack and also resets the value of the
    #' input to the initialized value from the start of the session
    
    print("observeEvent(show_vars2): Checkbox Observed")
    
    if (exists("mcbvector", e2)){
      
      print("observeEvent(show_vars2): The mcbvector Exists")
      
      old_mcbvector <- getMCBVector() #get the old scbvector before inputs were changed
      current_mcbvector <- show_vars2()
      setMCBVector(current_mcbvector) #updates the scbvector with the new inputs
      
      #produces a vector fo TRUE and FALSE. There should be one element that is different and
      #grep 'FALSE' will find that index.
      index_differ <- grep("FALSE", (current_mcbvector == old_mcbvector))
      
      if (length(index_differ) == 0){
        #Nothing will be analyzed since the value was changed to TRUE
      }
      else if (length(index_differ) > 1){
        #multiple selected or deselected, so do nothing
      }
      else{
        
        print("observeEvent(show_vars2): The Checkbox Index_Differ Worked")
        
        index_name <- names(current_mcbvector[index_differ])
        value <- current_mcbvector[index_differ] #gets the value of the new parameter
        
        if (is.null(value) || is.na(value)){
          #Nothing will be analyzed
          print("observeEvent(show_vars2): The checkbox value is null or na")
          stopApp()
        }
        else if (value == FALSE){
          #this ensures that the checkbox was unchecked instead of checked
          
          print("observeEvent(show_vars2): The Value of the Checkbox was FALSE")
          
          current_mistack <- getMIStack() #gets the newstack
          
          #This needs to be updated because of the parenthese
          updated_index_name <- gsub("\\(", "\\\\(", index_name)
          updated_index_name <- gsub("\\)", "\\\\)", updated_index_name)
          updated_index_name <- paste0("^", updated_index_name) #must start with it to avoid mis matches
          
          stack_index <- grep(updated_index_name, current_mistack) #gets the index in the stack
          
          if (length(stack_index) == 0){
            print("observeEvent(show_vars2): The Input was not in the Stack")
            #the input that was unchecked is not in the stack, so do nothing
          }
          else if (length(stack_index) == 1 || length(stack_index) == 2){
            #the parameter that was unchecked is in the stack
            #it can be of length 1 or 2 depending of the input parameter had a min and max value
            #if it is a selectize input, there will only be one match (length of 1)
            
            print("observeEvent(show_vars2): The Input was in the Stack")
            print(paste0("observeEvent(show_vars2): The Checkboxname is: ", index_name))
            
            resetMI(index_name) #resets the values of the inputs of the checkbox
            removeMIStack(index_name)
            removeMDFSeries() #removes the inputs associated with the checkbox
          }
          else{
            #none of the conditions were met, most likely multiple matches greater than 2
            print("observeEvent(show_vars2): The Stack Index for the Checkboxes ObserveEvent has a length > 2 or negative")
            stopApp()
          } #end if-else for the length of the stack index
          
        }
        else{
          #if it is not false, do nothing
          print("observeEvent(show_vars2): The Checkbox value was TRUE")
        }#end if-else for the value being na or null
        
      } #end if-else for the length of index_differ
      
    }
    else{
      #if it has not been created, it sets the vector
      #this is mainly to initialize data.
      
      setOriginalMIVector(multi_inputs()) #initialize the original values
      setMCBVector(show_vars2()) #initialize the input values that will be changing with the app
      
    } #end if-else for scbvector existing
    
    
  })#end observeEvent for the checkboxes
  

  
  # obtain the output of checkbox from functions and make a list to store them
  #this variable will store all the inputs of the multi-layer extrusions

  show_vars2<-reactive({
    
    checkboxes2 <- as.numeric(c(TRUE, input$PCMPN_d,input$PCMPD_d,input$PCMRN_d,input$PCMRD_d,
                               input$PCMPPSN_d,
                               input$PCMRF_d, input$PCMRBQ_d, input$PCMRPBQ_d, input$PCMRFQ_d, 
                               input$PCMRFi_d, input$PCMRCQ_d, input$PCMRC_d, input$PCMRRQ_d,
                               input$PCMRDu_d, input$PCMRADu_d,
                               input$PCMET_d, input$PCMB_d,
                               input$PCMDS_d,input$PCMDLL_d,input$PCMTS_d,
                               input$PCMTLL_d,input$PCMSP_d,input$PCMFT_d,
                               input$PCMBZT1_d,input$PCMBZT2_d,input$PCMBZT3_d,
                               input$PCMCT_d,input$PCMAT_d,input$PCMDT1_d,input$PCMDT2_d, input$PCMTE_d,
                               input$PCMIDI_d,input$PCMODI_d,input$PCMIWT_d,input$PCMMWT_d,
                               input$PCMOWT_d,input$PCMTWT_d,input$PCMOR_d,input$PCMCCT_d,
                               input$PCMLength_d,input$PCMToLength_d,input$PCMPPD_d,input$PCMNEXIV_d,
                               input$PCMAnnealed_d,input$PCMCaliper_d,input$PCMOS_d,input$PCMMP_d,
                               input$PCMHT_d,input$PCMSPD_d,input$PCMSLD_d,input$PCMDLN_d,
                               input$PCMULT_d,input$PCMVC_d,input$PCMIRD_d))
    
    names(checkboxes2) <- c("Placeholder", "Part Number", "Part Description", "Resin Number", "Resin Description",
                           "PPS Number", "Extrusion Type", 
                           "Resin Families", "Is Resin Blended with Anything?", "Is Resin a Polymer Blend?",
                           "Is Resin Filled?", "Resin Fillers", "Is Resin Colored?", 
                           "Resin Color", "Is Resin Radiopaque?", "Resin Durometer (D)", 
                           "Average Resin Durometer (D)",
                           "Barrel",
                           "Die Size (in)", "Die Land Length (in)",
                           "Tip Size (in)", "Tip Land Length (in)", "Screw Print",
                           "Feedthroat Temperature  F",
                           "Barrel Zone 1 Temperature  F", "Barrel Zone 2 Temperature  F",
                           "Barrel Zone 3 Temperature  F","Clamp Temperature  F",
                           "Adapter Temperature  F","Die 1 Temperature  F", "Die 2 Temperature  F",
                           "Tapered End",
                           "Inner Diameter (in)", "Outer Diameter (in)",
                           "Inner Wall Thickness (in)", "Middle Wall Thickness (in)",
                           "Outer Wall Thickness (in)", "Total Wall Thickness (in)",
                           "Out of Roundness (in)",
                           "Concentricity (in)", "Length (in)", "Total Length",
                           "Perpendicularity (in)",
                           "Nexiv", "Annealed", "Caliper", "OD Sort", "Melt Pump", "Hypo Tip",
                           "Sparker Die", "Slicking Die", "Delamination", "Ultrasonic",
                           "Vacuum Calibration", "Irradiated")
    
    return(checkboxes2)
    
    })
  
  #this variable will store all the inputs of the multi extrusions
  multi_inputs <- reactive({
    #this variable will store all the inputs of of the multi extrusions
    inputs2 <- c("Placeholder", input$PCMPN, input$PCMPD, input$PCMRN, input$PCMRD, input$PCMPPSN,
                 "Placeholder",
                 "Placeholder", "Placeholder", "Placeholder", "Placeholder", "Placeholder",
                 "Placeholder", "Placeholder", "Placeholder", "Placeholder", "Placeholder",
                 "Placeholder",
                input$PCMDS_min, input$PCMDS_max, input$PCMDLL, input$PCMTS_min, input$PCMTS_max,
                input$PCMTLL, input$PCMSP, 
                input$PCMFT_min, input$PCMFT_max, input$PCMBZT1_min, input$PCMBZT1_max,
                input$PCMBZT2_min, input$PCMBZT2_max, input$PCMBZT3_min, input$PCMBZT3_max,
                input$PCMCT_min, input$PCMCT_max, input$PCMAT_min, input$PCMAT_max,
                input$PCMDT1_min, input$PCMDT1_max, input$PCMDT2_min, input$PCMDT2_max,
                input$PCMIDI_min, input$PCMIDI_max, input$PCMODI_min, input$PCMODI_max,
                input$PCMIWT_min, input$PCMIWT_max, 
                input$PCMMWT_min, input$PCMMWT_max,
                input$PCMOWT_min, input$PCMOWT_max,
                input$PCMTWT_min, input$PCMTWT_max,
                input$PCMOR_min, input$PCMOR_max, 
                input$PCMCCT_min, input$PCMCCT_max, input$PCMLength_min, input$PCMLength_max,
                input$PCMPPD,
                input$PCMNEXIV, input$PCMAnnealed, input$PCMCaliper, input$PCMOS,
                input$PCMMP, input$PCMHT, input$PCMSPD, input$PCMSLD, input$PCMDLN, input$PCMULT,
                input$PCMVC, input$PCMIRD)
    
    names(inputs2) <- c("Placeholder", "Part Number", "Part Description", "Resin Number", "Resin Description",
                       "PPS Number", "Placeholder",
                       "Placeholder", "Placeholder", "Placeholder", "Placeholder", "Placeholder",
                       "Placeholder", "Placeholder", "Placeholder", "Placeholder", "Placeholder",
                       "Placeholder",
                       "Die Size (in) Min", "Die Size (in) Max", "Die Land Length (in)", 
                       "Tip Size (in) Min","Tip Size (in) Max", "Tip Land Length(in)", "Screw Print",
                       "Feedthroat Temperature  F Min", "Feedthroat Temperature  F Max",
                       "Barrel Zone 1 Temperature  F Min", "Barrel Zone 1 Temperature  F Max",
                       "Barrel Zone 2 Temperature  F Min", "Barrel Zone 2 Temperature  F Max",
                       "Barrel Zone 3 Temperature  F Min", "Barrel Zone 3 Temperature  F Max",
                       "Clamp Temperature  F Min", "Clamp Temperature  F Max",
                       "Adapter Temperature  F Min", "Adapter Temperature  F Max",
                       "Die 1 Temperature  F Min", "Die 1 Temperature  F Max",
                       "Die 2 Temperature  F Min", "Die 2 Temperature  F Max",
                       "Inner Diameter (in) Min", "Inner Diameter (in) Max", "Outer Diameter (in) Min",
                       "Outer Diameter (in) Max", 
                       "Inner Wall Thickness (in) Min", "Inner Wall Thickness (in) Max",
                       "Middle Wall Thickness (in) Min", "Middle Wall Thickness (in) Max",
                       "Outer Wall Thickness (in) Min", "Outer Wall Thickness (in) Max",
                       "Total Wall Thickness (in) Min", "Total Wall Thickness (in) Max",
                       "Out of Roudness (in) Min", "Out of Roundness (in) Max", 
                       "Concentricity (in) Min", "Concentricity (in) Max",
                       "Length (in) Min", "Length (in) Max", "Perpendicularity (in", 
                       "Nexiv", "Annealed", "Caliper", "OD Sort", "Melt Pump", "Hypo Tip",
                       "Sparker Die", "Slicking Die", "Delamination", "Ultrasonic",
                       "Vacuum Calibration", "Irradiated")
    return(inputs2)
  })
  
  
  
  output$mytable2 <- DT::renderDataTable({
    DT::datatable({
      
      Col_PCM <- which(1 == show_vars2())
  
      data_PCM <- multi_df_output$data #the data frame is set
      data_PCM<-data_PCM[,Col_PCM]
      
      clean_multi_pps_data$data <- data_PCM #assign the clean table to the data that is available
      #for downloading
      
      
      return(data_PCM)

    },
    options = list(orderClasses = TRUE, 
                   columnDefs = list(list(className = 'dt-center', 
                                          targets = "_all"
                   )
                   ),
                   scrollX=TRUE,
                   scrollY=500,
                   autoWidth=TRUE))
  },
  rownames = FALSE, 
  escape = FALSE, #escape allows for html elements to be rendered in the table
  server = FALSE)#END Multi Extrusion PPS Data
  
  
  

  ###
  
  ### The multi-layer shopping cart section ###
  
  ###
  
  
  multishoppingcart <- reactiveValues(
    #this is a shopping cart to hold all the multi extrusion parts and SAP batches that a user wants.
    #this is linked to the output data, so only the output data located of the associated batches 
    #in the shopping cart is displayed
    data = data.frame("Part" = numeric(0), "Delete Part" = numeric(0),
                      "SAP Batch" = numeric(0), "Delete Batch" = numeric(0),
                      stringsAsFactors = FALSE,
                      check.names = FALSE)
  ) #end multishoppingcart
  
  multishoppingcartparts <- reactiveValues(
    #this is a shopping cart to hold all the multi extrusion parts and SAP batches that a user wants.
    #this is linked to the output data, so only the output data located of the associated batches 
    #in the shopping cart is displayed
    data = data.frame("Part" = numeric(0), "Batches?" = numeric(0), "Delete Part" = numeric(0),
                      stringsAsFactors = FALSE,
                      check.names = FALSE)
  ) #end multishoppingcartparts
  
  observeEvent(input$multiadd_button,{
    #this observes whether the user clicked a button to add a part to the shopping cart
    #get the part
    part <- strsplit(input$multiadd_button, "_")[[1]][2]
    print(part)
    
    #Action button to delete part
    deletepart <- as.character(
      actionButton(inputId = paste0("button_", part),
                   label = "Delete Part",
                   onclick = 'Shiny.onInputChange(\"multidelete_part_button\",  this.id)'))
    
    #Get the SAP batches
    SAP_batches <- multi_tari_parameter_data$`SAP Batch Number`[multi_tari_parameter_data$`Material Number` == part]
    numberofbatches <- length(SAP_batches)
    
    #Action button to delete batch
    batch_count <- 1
    vectorofbuttons <- c(rep(0, length(SAP_batches)))
    
    while(batch_count < length(SAP_batches) + 1){
      vectorofbuttons[batch_count] <- as.character(
        actionButton(inputId = paste0("button_", SAP_batches[batch_count]),
                     label = "Delete Batch",
                     onclick = 'Shiny.onInputChange(\"multidelete_batch_button\",  this.id)'))
      batch_count <- batch_count + 1
    }
    
    #Vectors of parts and buttons
    partvector <- rep(part, numberofbatches)
    deletepartvector <- rep(deletepart, numberofbatches)
    
    new_data <- cbind(partvector, deletepartvector, SAP_batches, vectorofbuttons)
    
    colnames(new_data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
    
    
    if (length(grep(part, singleshoppingcart$data$"Part")) == 0){
      multishoppingcart$data <- rbind(multishoppingcart$data, new_data, stringsAsFactors = FALSE)
      colnames(multishoppingcart$data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
    }
    else{
      #Do nothing if the part is already there
    }
  }) #End observeEvent(input$multiadd_button)
  
  observeEvent(input$multiadd_button,{
    #this observes whether the user clicked a button to add a part to the part only shopping cart
    part <- strsplit(input$multiadd_button, "_")[[1]][2]
    
    #Action button to delete part
    deletepart <- as.character(
      actionButton(inputId = paste0("button_", part),
                   label = "Delete Part",
                   onclick = 'Shiny.onInputChange(\"multidelete_part_button\",  this.id)'))
    
    
    #This determines if there are batches for the part
    SAP_batches <- multi_tari_parameter_data$`SAP Batch Number`[multi_tari_parameter_data$`Material Number` == part]
    numberofbatches <- length(SAP_batches)
    
    if(numberofbatches > 0){
      #if there are batches
      batches <- "Yes"
    }
    else{
      batches <- "No"
    }
    
    new_data <- cbind(part, batches, deletepart)
    
    colnames(new_data) <- c("Part", "Batches?", "Delete Part")
    
    
    if (length(grep(part, multishoppingcartparts$data$"Part")) == 0){
      multishoppingcartparts$data <- rbind(multishoppingcartparts$data, new_data, stringsAsFactors = FALSE)
      colnames(multishoppingcartparts$data) <- c("Part", "Batches?", "Delete Part")
    }
    else{
      #Do nothing if the part is already there
    }
    
  }) #end observeEvent(input$multiadd_button)
  
  observeEvent(input$multiaddtable,{
    #this observes for the button multiaddtable which will add all the parts in the table to the
    #shopping cart.
    
    raw_parts <- unlist(strsplit(unlist(strsplit(multi_df_output$data$`Part Number`, ">")), "<"))
    
    clean_parts <- raw_parts[seq(3,length(raw_parts), 4)]
    
    count <- 1
    
    while (count < (length(clean_parts) + 1)){
      #iterates through the parts
      
      part <- clean_parts[count]
      
      #Action button to delete part
      deletepart <- as.character(
        actionButton(inputId = paste0("button_", part),
                     label = "Delete Part",
                     onclick = 'Shiny.onInputChange(\"multidelete_part_button\",  this.id)'))
      
      #Get the SAP batches
      SAP_batches <- multi_tari_parameter_data$`SAP Batch Number`[multi_tari_parameter_data$`Material Number` == part]
      numberofbatches <- length(SAP_batches)
      
      if(numberofbatches > 0){
        #if there are batches
        batches <- "Yes"
        
        #Action button to delete batch
        #' if there are not batches the part will not be added to this shopping cart table but will
        #' be added to the part
        batch_count <- 1
        vectorofbuttons <- c(rep(0, length(SAP_batches)))
        
        while(batch_count < length(SAP_batches) + 1){
          vectorofbuttons[batch_count] <- as.character(
            actionButton(inputId = paste0("button_", SAP_batches[batch_count]),
                         label = "Delete Batch",
                         onclick = 'Shiny.onInputChange(\"multidelete_batch_button\",  this.id)'))
          batch_count <- batch_count + 1
        }
        
        #Vectors of parts and buttons
        partvector <- rep(part, numberofbatches)
        deletepartvector <- rep(deletepart, numberofbatches)
        
        new_data <- cbind(partvector, deletepartvector, SAP_batches, vectorofbuttons)
        
        colnames(new_data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
        
        if (length(grep(part, multishoppingcart$data$"Part")) == 0){
          multishoppingcart$data <- rbind(multishoppingcart$data, new_data, stringsAsFactors = FALSE)
          colnames(multishoppingcart$data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
        }
        else{
          #Do nothing if the part is already there
        }
        
      }
      else{
        batches <- "No"
      }
      
      new_data <- cbind(part, batches, deletepart)
      
      colnames(new_data) <- c("Part", "Batches?", "Delete Part")
      
      
      if (length(grep(part, multishoppingcartparts$data$"Part")) == 0){
        multishoppingcartparts$data <- rbind(multishoppingcartparts$data, new_data, stringsAsFactors = FALSE)
        colnames(multishoppingcartparts$data) <- c("Part", "Batches?", "Delete Part")
      }
      else{
        #Do nothing if the part is already there
      }
      
      count <- count + 1
    }
    
  })
    
    
  
  
  observeEvent(input$multidelete_part_button,{
    #'this observes whether a person deleted a part from the shopping cart. If the button is clicked
    #'all batches associated to the part are removed
    part <- strsplit(input$multidelete_part_button, "_")[[1]][2]
    multishoppingcart$data <- multishoppingcart$data[multishoppingcart$data$'Part' != part,]
    multishoppingcartparts$data <- multishoppingcartparts$data[multishoppingcartparts$data$'Part' != part,]
  })
  
  observeEvent(input$multidelete_batch_button,{
    #'this observes whether a person deleted a SAP batch from the shopping cart. If the button is
    #'clicked, the batch is removed from the cart
    batch <- strsplit(input$multidelete_batch_button, "_")[[1]][2]
    multishoppingcart$data <- multishoppingcart$data[multishoppingcart$data$'SAP Batch' != batch,]
  })
  
  
  output$multishoppingcart <- renderDataTable({
    #'this shopping cart allows a user to select parts and batches they want to examine. Once added
    #'to the cart, they can view all the MES, SAP, and AppStats data
    return(multishoppingcart$data)
    },
    filter = "top",
    rownames = FALSE,
    escape = FALSE,
    server = FALSE,
    options = list(orderClasses = TRUE,
                   columnDefs = list(list(className = 'dt-center',targets = "_all")),
                   scrollX=TRUE,
                   scrollY=250,
                   autoWidth=TRUE,
                   pageLength = 5)
    ) #for the shoppingcart
  
  output$multishoppingcartparts <- renderDataTable({
    #'this is a table that only lists the parts for quick viewing
    return(multishoppingcartparts$data)
  },
  filter = "top",
  rownames = FALSE,
  escape = FALSE,
  server = FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=250,
                 autoWidth=TRUE,
                 pageLength=5) # make the shopping cart page shorter
  ) #for the shoppingcart
  
  
  output$multidownloadSPPSData <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Visible Multi-Layer PPS Data", '.csv', sep='') },
    content = function(file) {
      #I remove the first column so the HTML is not outputed
      output <- clean_multi_pps_data$data[2:ncol(clean_multi_pps_data$data)]
      
      if (length(grep("Part Number", colnames(output), ignore.case = TRUE) > 0)){
        #if a user has selected the column
        raw_parts <- unlist(strsplit(unlist(strsplit(multi_df_output$data$`Part Number`, ">")), "<"))
        clean_parts <- raw_parts[seq(3,length(raw_parts), 4)]
        output$'Part Number' <- clean_parts
      }
      
      if (length(grep("PPS Number", colnames(output), ignore.case = TRUE) > 0)){
        #if a user has selected the column
        raw_pps <- unlist(strsplit(unlist(strsplit(multi_df_output$data$`PPS Number`, ">")), "<"))
        clean_pps <- raw_parts[seq(3,length(raw_pps), 4)]
        output$'PPS Number' <- clean_pps
      }
      
      write.csv(output, file, row.names = FALSE)
    }
  )

  
  output$multidownloadSPPSDataAll <- downloadHandler(
    #downlaod the data
    filename = function() { paste("All Multi-Layer PPS Data", '.csv', sep='') },
    content = function(file) {
      #I remove the first column so the HTML is not outputed
      output <- multi_df_output$data[2:ncol(multi_df_output$data)]
      
      raw_parts <- unlist(strsplit(unlist(strsplit(multi_df_output$data$`Part Number`, ">")), "<"))
      clean_parts <- raw_parts[seq(3,length(raw_parts), 4)]
      output$'Part Number' <- clean_parts
      
      raw_pps <- unlist(strsplit(unlist(strsplit(multi_df_output$data$`PPS Number`, ">")), "<"))
      clean_pps <- raw_parts[seq(3,length(raw_pps), 4)]
      output$'PPS Number' <- clean_pps
      
      
      write.csv(output, file, row.names = FALSE)
    }
  )
  
  output$multishoppingcartpps <- renderDataTable({
    #this is to render a datatable that has all the PPS information of parts that have been saved
    #to the shopping cart
    
    raw_parts <- unlist(strsplit(unlist(strsplit(multi_pps_data$`Part Number`, ">")), "<"))
    clean_parts <- raw_parts[seq(3,length(raw_parts), 4)]
    
    data <- multi_pps_data[which(clean_parts %in% multishoppingcartparts$data$'Part'),2:ncol(multi_pps_data)]
    return(data)
    
  },
  filter = "top",
  rownames = FALSE,
  escape = FALSE,
  server = FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE)
  )
  
  
  
  output$multicartdownloadpps <- downloadHandler(
    #downlaod the multi PPS data from the shopping cart
    filename = function() { paste("Single PPS Shopping Cart Data", '.csv', sep='') },
    content = function(file) {
      
      output <- multi_pps_data
      
      if (length(grep("Part Number", colnames(output), ignore.case = TRUE) > 0)){
        #removes the html from the part number
        raw_parts <- unlist(strsplit(unlist(strsplit(output$`Part Number`, ">")), "<"))
        clean_parts <- raw_parts[seq(3,length(raw_parts), 4)]
        output$'Part Number' <- clean_parts
      }
      
      if (length(grep("PPS Number", colnames(output), ignore.case = TRUE) > 0)){
        #removes the html from the PPS number
        raw_pps <- unlist(strsplit(unlist(strsplit(output$`PPS Number`, ">")), "<"))
        clean_pps <- raw_parts[seq(3,length(raw_pps), 4)]
        output$'PPS Number' <- clean_pps
      }
      
      write.csv(output[which(output$`Part Number` %in% multishoppingcartparts$data$'Part'),2:ncol(multi_pps_data)], 
                file, row.names = FALSE)
    }
  )
  
  observeEvent(input$checkmultitooling,{
    #this checks all the checkboxes associated with multi tooling inputs
    
    updateCheckboxInput(session, inputId = "PCMB_d", label = "Barrel",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMDS_d", label = "Die Size (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMDLL_d", label = "Die Land Length (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMTS_d", label = "Tip Size (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMTLL_d", label = "Tip Land Length (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMSP_d", label = "Screw Print",value = TRUE)
    
  }) #end obserEvent for input$checkmultitooling
  
  observeEvent(input$checkmultiresininfo,{
    #this checks all the checkboxes associated with multi tooling inputs
    updateCheckboxInput(session, inputId = "PCMRF_d", label = "Resin Families",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMRBQ_d", label = "Is Resin Blended with Anything?",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMRPBQ_d", label = "Is Resin a Polymer Blend?",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMRFQ_d", label = "Is Resin Filled?",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMRFi_d", label = "Resin Fillers",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMRCQ_d", label = "Is Resin Colored?",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMRC_d", label = "Resin Color",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMRRQ_d", label = "Is Resin Radiopaque?",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMRDu_d", label = "Resin Durometer (D)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMRADu_d", label = "Average Durometer (D)",value = TRUE)
    
  }) #end obserEvent for input$checkmultitooling
  
  observeEvent(input$uncheckmultiresininfo,{
    #this unchecks all the checkboxes associated with multi tooling inputs
    updateCheckboxInput(session, inputId = "PCMRF_d", label = "Resin Families",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMRBQ_d", label = "Is Resin Blended with Anything?",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMRPBQ_d", label = "Is Resin a Polymer Blend?",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMRFQ_d", label = "Is Resin Filled?",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMRFi_d", label = "Resin Fillers",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMRCQ_d", label = "Is Resin Colored?",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMRC_d", label = "Resin Color",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMRRQ_d", label = "Is Resin Radiopaque?",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMRDu_d", label = "Resin Durometer (D)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMRADu_d", label = "Average Durometer (D)",value = FALSE)
    
  }) #end obserEvent for input$uncheckmultitooling
  
  
  observeEvent(input$uncheckmultitooling,{
    #this unchecks all the checkboxes associated with multi tooling inputs
    updateCheckboxInput(session, inputId = "PCMET_d", label = "Extrusion Type",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMB_d", label = "Barrel",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMDS_d", label = "Die Size (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMDLL_d", label = "Die Land Length (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMTS_d", label = "Tip Size (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMTLL_d", label = "Tip Land Length (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMSP_d", label = "Screw Print",value = FALSE)
    
  }) #end obserEvent for input$uncheckmultitooling
  
  
  observeEvent(input$checkmultiparameters,{
    #this checks all the checkboxes associated with multi processing parameters inputs
    updateCheckboxInput(session, inputId = "PCMFT_d", label = "Feedthroat Temperature F",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMBZT1_d", label = "Barrel Zone 1 Temperature F",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMBZT2_d", label = "Barrel Zone 2 Temperature F",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMBZT3_d", label = "Barrel Zone 3 Temperature F",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMCT_d", label = "Clamp Temperature F",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMAT_d", label = "Adapter Temperature F",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMDT1_d", label = "Die 1 Temperature F",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMDT2_d", label = "Die 2 Temperature F",value = TRUE)
    
  }) #end obserEvent for input$checkmultiparameters
  
  
  observeEvent(input$uncheckmultiparameters,{
    #this unchecks all the checkboxes associated with multi processing parameters inputs
    updateCheckboxInput(session, inputId = "PCMFT_d", label = "Feedthroat Temperature F",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMBZT1_d", label = "Barrel Zone 1 Temperature F",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMBZT2_d", label = "Barrel Zone 2 Temperature F",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMBZT3_d", label = "Barrel Zone 3 Temperature F",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMCT_d", label = "Clamp Temperature F",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMAT_d", label = "Adapter Temperature F",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMDT1_d", label = "Die 1 Temperature F",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMDT2_d", label = "Die 2 Temperature F",value = FALSE)
    
  }) #end obserEvent for input$uncheckmultiparameters
  
  observeEvent(input$checkmultidimensions,{
    #this checks all the checkboxes associated with multi dimensional attribute inputs
    updateCheckboxInput(session, inputId = "PCMTE_d", label = "Tapered End",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMIDI_d", label = "Inner Diameter (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMODI_d", label = "Outer Diameter (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMIWT_d", label = "Inner Wall Thickness (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMMWT_d", label = "Middle Wall Thickness (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMOWT_d", label = "Outer Wall Thickness (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMTWT_d", label = "Total Wall Thickness (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMOR_d", label = "Out of Roundness (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMCCT_d", label = "Concentricity",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMLength_d", label = "Length (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMToLength_d", label = "Total Length (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMPPD_d", label = "Perpendicularity (in)",value = TRUE)
    
  }) #end obserEvent for input$checkmultidimensions
  
  
  observeEvent(input$uncheckmultidimensions,{
    #this unchecks all the checkboxes associated with multi dimensional attribute inputs
    updateCheckboxInput(session, inputId = "PCMTE_d", label = "Tapered End",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMIDI_d", label = "Inner Diameter (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMODI_d", label = "Outer Diameter (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMIWT_d", label = "Inner Wall Thickness (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMMWT_d", label = "Middle Wall Thickness (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMOWT_d", label = "Outer Wall Thickness (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMTWT_d", label = "Total Wall Thickness (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMOR_d", label = "Out of Roundness (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMCCT_d", label = "Concentricity",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMLength_d", label = "Length (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMToLength_d", label = "Total Length (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMPPD_d", label = "Perpendicularity (in)",value = FALSE)
    
  }) #end obserEvent for input$uncheckmultidimensions
  
  observeEvent(input$checkmultispecial,{
    #this checks all the checkboxes associated with multi special operation inputs
    updateCheckboxInput(session, inputId = "PCMNEXIV_d", label = "NEXIV",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMAnnealed_d", label = "Annealed",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMCaliper_d", label = "Caliper",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMOS_d", label = "OD Sort",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMMP_d", label = "Melt Pump",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMHT_d", label = "Hypo Tip",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMSPD_d", label = "Sparker Die",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMSLD_d", label = "Slicking Die",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMDLN_d", label = "Delamination",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMULT_d", label = "Ultrasonic",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMVC_d", label = "Vacuum Calibration",value = TRUE)
    updateCheckboxInput(session, inputId = "PCMIRD_d", label = "Irradiated",value = TRUE)
    
  }) #end obserEvent for input$checkmultispecial
  
  
  observeEvent(input$uncheckmultispecial,{
    #this unchecks all the checkboxes associated with multi special operation inputs
    updateCheckboxInput(session, inputId = "PCMNEXIV_d", label = "NEXIV",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMAnnealed_d", label = "Annealed",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMCaliper_d", label = "Caliper",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMOS_d", label = "OD Sort",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMMP_d", label = "Melt Pump",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMHT_d", label = "Hypo Tip",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMSPD_d", label = "Sparker Die",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMSLD_d", label = "Slicking Die",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMDLN_d", label = "Delamination",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMULT_d", label = "Ultrasonic",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMVC_d", label = "Vacuum Calibration",value = FALSE)
    updateCheckboxInput(session, inputId = "PCMIRD_d", label = "Irradiated",value = FALSE)
    
  }) #end obserEvent for input$uncheckmultispecial
  
  observeEvent(input$resetmultiinputs,{
    #this will reset the multi inputs when clicked
    #It first removes all the dataframes that have been created, resets the input values,
    #and then it resets the stack
    setMDFList(NULL) #resetst the DF list
    
    current_inputs <- isolate(multi_inputs())
    original_inputs <- getOriginalMIVector()
    
    differ_indices <- which(current_inputs != original_inputs)
    value_names <- names(current_inputs[differ_indices])
    
    print(paste0("observeEvent(input$resetmultiinputs): ",value_names))
    
    count <- 1
    #this while loop iterates through all the values that differed
    while (count < length(value_names) + 1){
      current_name <- value_names[count]
      resetMI(current_name)
      count <- count + 1
    }#end while
    
    setMIStack(c()) #creates an empty stack
    
    #sets the data table to the original data set
    multi_df_output$data <- multi_pps_data
    
  }) #end obserEvent for input$resetmultiinputs
  
  
  #### Tapered PPS ####
  
  e3 <- new.env(
    #This environment will store variable of inputs and stack that are used for comparison
    #of the input parameters that have been selected and changed
    #' variables to contain:
    #' tivector - a vector that will store the inputs of the tapered extrusion parameters
    #' tistack - the stack for the tapered inputs
  ) #creates a new environment to store instance variables
  
  #the assign will initialize the tiidvector
  assign("tiidvector", 
         c("Placeholoder", 
           "PCTPN", "PCTPD", "PCTRN", "PCTRD", "PCTPPSN", 
           "Placeholder", "Placeholder", "Placeholder", "Placeholder", "Placeholder",
           "Placeholder", "Placeholder", "Placeholder", "Placeholder", "Placeholder",
           "PCTDS_min", "PCTDS_max", "PCTDLL", "PCTTS_min", "PCTTS_max",
           "PCTTLL", "PCTSP", 
           "PCTFT_min", "PCTFT_max", "PCTBZT1_min", "PCTBZT1_max",
           "PCTBZT2_min", "PCTBZT2_max", "PCTBZT3_min", "PCTBZT3_max",
           "PCTCT_min", "PCTCT_max", "PCTAT_min", "PCTAT_max",
           "PCTDT1_min", "PCTDT1_max", "PCTDT2_min", "PCTDT2_max",
           "PCTPIDI_min", "PCTPIDI_max", "PCTPODI_min", "PCTPODI_max", "PCTPWT_min", 
           "PCTPWT_max","PCTPOR_min", "PCTPOR_max", "PCTPCCT_min", "PCTPCCT_max",
           "PCTDIDI_min", "PCTDIDI_max", "PCTDODI_min", "PCTDODI_max", "PCTDWT_min",
           "PCTDWT_max", "PCTDOR_min", "PCTDOR_max", "PCTDCCT_min", "PCTDCCT_max",
           "PCTPLength_min", "PCTPLength_max", "PCTTLength_min", "PCTTLength_max",
           "PCTDLength_min", "PCTDLength_max", "PCTToLength_min", "PCTToLength_max",
           "PCTPPD",
           "PCTNEXIV", "PCTAnnealed", "PCTCaliper", "PCTOS",
           "PCTMP", "PCTHT", "PCTSPD", "PCTSLD", "PCTDLN", "PCTULT",
           "PCTVC", "PCTIRD"), 
         envir = e3)
  
  
  ## These are the setter and getter function ##
  
  setTIIDVector <- function(vector){
    #set the vector for the input ids
    
    assign("tiidvector", vector, env = e3)
    
  } #end setTIIDVector
  
  getTIIDVector <- function(){
    #returns the IDs of the inputs
    
    if(exists("tiidvector", e3)){
      return(get("tiidvector", e3))
    }
    else{
      return(NA)
    }
  } #end SIIDVector
  
  setTCBVector <- function(vector){
    #this sets the values for the tapered input vector
    assign("tcbvector", vector, env = e3)
  } #end setTCVector
  
  getTCBVector <- function(vector){
    #this gets the values for the tapered checkbox vector
    
    if(exists("tcbvector", e3)){
      return(get("tcbvector", e3))
    }
    else{
      return(NA)
    }
    
  } #end getTCBVector
  
  setOriginalTIVector <- function(vector){
    #this sets the values for the original min and max input values
    assign("originaltcbvector", vector, env = e3)
  } #end setOriginalTIVector
  
  getOriginalTIVector <- function(vector){
    #this gets the values for the original min and max input values
    
    if(exists("originaltcbvector", e3)){
      return(get("originaltcbvector", e3))
    }
    else{
      return(NA)
    }
    
  } #end getOriginalTIVector
  
  setTIVector <- function(vector){
    #this sets the values for the tapered input vector
    assign("tivector", vector, env = e3)
  } #end setTIVector
  
  getTIVector <- function(){
    #returns tivector
    
    if(exists("tivector", e3)){
      return(get("tivector", e3))
    }
    else{
      return(NA)
    }
    
  } #end getTIVector
  
  setTIStack <- function(stack){
    #this sets the stack for the tapered inputs
    assign("tistack", stack, env = e3)
  } #end setTIStack
  
  getTIStack <- function(){
    #returns tivector
    if(exists("tistack", e3)){
      return(get("tistack", e3))
    }
    else{
      return(NA)
    }
    
  } #end getTIStack
  
  getTIStack.length <- function(){
    #returns the length of the SIStack
    
    if(exists("tistack", e3)){
      #if it exists, return the length
      return(length(getTIStack()))
    }
    else{
      #else return NA
      return(NA)
    }
    
  }#end getTIStack.length
  
  addTIStack <- function(name){
    #this function adds a new parameter to the stack
    #it will only add to the stack if it is a unique name
    
    stack <- get("tistack", e3)
    
    
    #edit name
    rep_name <- gsub("\\(", "\\\\(", name)
    rep_name <- gsub("\\)", "\\\\)", rep_name)
    rep_name <- paste0("^", rep_name) #must start with it to avoid mis matches
    
    if (length(grep(rep_name, stack)) == 0){
      #the name is not currently in the stack
      stack <- c(stack, name) #add the name to the top of the stack (the right most)
      setTIStack(stack) #set the stack
    }
    else{
      #do nothing because the name is already present
    }
    
  } #end  addTIStack
  
  removeTIStack <- function(name){
    #this function removes the inputs associated with the checkbox name from the stack
    
    print("removeTIStack: We are in the removeTIStack")
    
    stack <- get("tistack", e3)
    
    
    #edit name
    rep_name <- gsub("\\(", "\\\\(", name)
    rep_name <- gsub("\\)", "\\\\)", rep_name)
    rep_name <- paste0("^", rep_name) #must start with it to avoid mis matches
    
    if (length(grep(rep_name, stack)) != 0){
      #the name is not currently in the stack
      indices <- grep(rep_name, stack)
      
      if (length(indices) == length(stack)){
        #if the length of the indices are equal to the stack
        print("removeTIStack: The length of the indices are equal to the stack")
        stack <- c()
      }
      else if (length(indices) == 1){
        print("removeTIStack: There was only one match in the stack")
        #if there is only one match
        if (indices == 1){
          #if the first matches is 1 or the only match is one
          stack <- stack[c(2:length(stack))] # removes the first element
        }
        else if (indices == length(stack)){
          stack <- stack[c(1:(indices - 1))]
        }
        else{
          #removes the index from the stack
          stack <- stack[c(1:(indices-1), (indices + 1):length(stack))]
        }#end if-else for the vallue of indices
      }
      else if (length(indices) == 2){
        print("removeTIStack: There were multiple matches to the stack")
        #if there are multiple indices
        if (indices[1] == 1){
          #if the first matches is 1 or the only match is one
          
          if (indices[2] == 2){
            stack <- stack[c(3:length(stack))] # removes the first and second element
          }
          else if (indices[2] == length(stack)){
            stack <- stack[c(2:(indices[2] - 1))]
          }
          else{
            stack <- stack[c(2:(indices[2] - 1), (indices[2] + 1):length(stack))] # the indices
          }#end if-else for the value of the second index
          
        }
        else if(indices[1] == (length(stack) - 1)){
          #if the first index is length(stack)-1, then the second index is the length of the stack
          stack <- stack[c(1:(indices[1] - 1))]
        }
        else if (indices[1] == (indices[2] + 1)){
          #if the indices are right next to each other but are not at the ends of the stack
          stack <- stack[c(1:(indices[1]-1), (indices[2] + 1):length(stack))]
        }
        else{
          #removes the indices from the stack
          stack <- stack[c(1:(indices[1]-1), (indices[1] + 1):(indices[2] - 1), (indices[2] + 1):length(stack))]
        }#end if-else for the value of indices
        
      }#end if-else for the length of indices
      else{
        #if it was neither, there was an error and the app shoudl stop
        print("The indices in the removeTIStack were more than 2")
        print(paste0("The indices are: ", indices))
        stop()
      }
      
      
      setTIStack(stack) #set the stack
    }
    else{
      #do nothing because the name is not in the stack
      print("removeTIStack: Nothing was done because the name was not in the stack")
    }
    
  } #end  removeTIStack
  
  setTDFList <- function(list){
    #sets the DFList
    assign("df_list", list, env = e3)
  }#end setTDFList
  
  getTDFList <- function(){
    #returns the df_list
    if(exists("df_list", e3)){
      #if it exists, return the length
      return((get("df_list", e3)))
    }
    else{
      #else return NA
      return(NA)
    }
    
  }#end getTDFList
  
  addTDFSeries <- function(index, index_name, value){
    #this creates the series of successively more narrow data frames based on the number of parameters
    #the user has searched by. 3 parameters means there is the original data frame and three ones
    #that have been successively narrowed.
    
    if (exists("df_list", e3)){
      #if the list already exists
      
      if(index == getTIStack.length() + 1){
        #if the index is larger than the list length, we have a new parameter that will be added
        df_list <- getTDFList() #gets latest df_list
        latest_df <- df_list[[index-1]] #the index should only be 1 greater than the list
        
        new_df <- generateNewDF(latest_df, index_name, value)
        df_list[[index]] <- new_df #add the new data frame
        setTDFList(df_list) #set the new list
        
        tapered_df_output$data <- new_df #set the data table to this
        
      }
      else if(index > getTIStack.length() + 1){
        #if there was an error in the index and it was too large
        print("The index for the stack was too large and greater than the allowable limit")
        print(paste0("The stack index is: ", index, ". The stack length is: ", getTIStack.length(), "."))
        stopApp()
      }
      else if(index < 1){
        #error in the index
        print("The index for the stack was zero or negative")
        print(paste0("The stack index is: ", index, ". The stack length is: ", getTIStack.length(), "."))
        stopApp()
      }
      else{
        #the index is within the stack which means we means a previous parameter is being changed
        stack <- getTIStack()
        stack_length <- getTIStack.length()
        
        input_values <- getTIVector()
        
        df_list <- getTDFList() #gets latest df_list
        
        if (index == 1){
          #if it is starting from the beginning
          
          new_df <- generateNewDF(tapered_pps_data, index_name, value) #starts from this initial one
          df_list[[index]] <- new_df #add the new data frame
          
          count <- index + 1
          
          while (count < stack_length + 1){
            
            previous_df <- df_list[[count-1]]
            current_parameter <- stack[count]
            current_parameter_value <- input_values[current_parameter]
            
            new_df <- generateNewDF(previous_df, current_parameter, current_parameter_value)
            count <- count + 1
            df_list[[count]] <- new_df #add the new data frame
          }#end while creating new DFs
          
          setTDFList(df_list) #set the new list
          tapered_df_output$data <- new_df #set the data table to this
          
        }
        else{
          
          count <- index
          
          while (count < stack_length + 1){
            
            previous_df <- df_list[[count-1]] #gets the previous df
            current_parameter <- stack[count]
            current_parameter_value <- input_values[current_parameter]
            
            new_df <- generateNewDF(previous_df, current_parameter, current_parameter_value)
            
            df_list[[count]] <- new_df #add the new data frame
            
            count <- count + 1
          }#end while creating new DFs
          
          setTDFList(df_list) #set the new list
          tapered_df_output$data <- new_df #set the data table to this
          
        }#end if-else for index value
        
      }#end if else for the index length
      
    }
    else{
      assign("df_list", list("hello", "hi"), env = e3) #if the list does not yet exist, it creates it
      #creates two dummy variables otherwise the list does not initialize correctly
      
      new_df <- generateNewDF(tapered_pps_data, index_name, value) #starts from this initial one
      df_list <- getTDFList() #gets latest df_list
      df_list[[index]] <- new_df #add the new data frame
      df_list[[2]] <- NULL #removes the previous dummy variable
      
      setTDFList(df_list) #set the new list
      tapered_df_output$data <- new_df #set the data table to this
      
    }#end if-else for the df_list existing
    
  } #end addTDFSeries
  
  removeTDFSeries <- function(){
    #this removes the data frames that were in the df_list because of the parameters. It is called
    #when a checkbox is unchecked and one of the parameters were present
    
    print("removeTDFSeries: We are in removeTDFSeries")
    
    if (exists("df_list", e3)){
      #if the list already exists
      
      print("removeTDFSeries: df_list exists")
      
      stack <- getTIStack() #gets the stack
      stack_length <- getTIStack.length()
      
      if(stack_length == 0){
        #if when the input parameters were removed from the stack, all the parameters were removed
        #because the removed ones were the only parameters present
        setTDFList(NULL) #sets the list to NULL so it can be reinitialized
        tapered_df_output$data <- tapered_pps_data
        
        print("removeTDFSeries: The stack_length was zero")
        
      }
      else{
        #if there is still a stack present
        
        print("removeTDFSeries: The stack_length was NOT zero")
        
        input_values <- getTIVector() #gets the input_values
        df_list <- getTDFList() #gets latest df_list
        
        current_parameter <- stack[1] #gets the first parameter in the stack
        current_parameter_value <- input_values[current_parameter] #gets the parameters value
        
        new_df <- generateNewDF(tapered_pps_data, current_parameter, current_parameter_value) #starts from this initial one
        df_list[[1]] <- new_df #add the new data frame
        
        count <- 2
        
        while (count < stack_length + 1){
          
          previous_df <- df_list[[count-1]]
          current_parameter <- stack[count]
          current_parameter_value <- input_values[current_parameter]
          
          new_df <- generateNewDF(previous_df, current_parameter, current_parameter_value)
          count <- count + 1
          df_list[[count]] <- new_df #add the new data frame
        }#end while creating new DFs
        
        if (count < (length(df_list) + 1)){
          #'because we recreate the df_list from the original df_list and th new df_list will not
          #'have the dataframes from the removed parameters, this new df_list will be shorter than
          #'the original before the parameters were removed. Thus we resize the df_list
          
          while ((length(df_list) + 1) > count){
            #the df_list will continually be resized as the inputs are made null, thus the condition
            #is met when all the element greater than an index of count are removed
            
            df_list[[count]] <- NULL
            
          } #end while for making the elements NULL
          
          
        }#end if for the count compared to the df_list length
        
        setTDFList(df_list) #set the new list
        tapered_df_output$data <- new_df #set the data table to this
        
      }#end if-else for the stack length
      
    }
    else{
      #there was an error and the list did not exist when it should have because it observed a
      #unchecked checkbox and the stack length was not zero
      print("The df_list does not exist but you attempted to remove something from it")
      stopApp()
    }#end if else for the list existing
    
    
  } #end removeTDFSeries
  
  resetTI <- function(checkbox_name){
    #this resets the inputs for a checkbox
    
    print("resetTI: Resetting the Input Values")
    print(paste0("resetTI: checkbox_name is - ", checkbox_name))
    
    grepname <- gsub("\\(", "\\\\(", checkbox_name)
    grepname <- gsub("\\)", "\\\\)", grepname)
    grepname <- paste0("^", grepname) #must start with it to avoid mis matches
    
    inputs <- names(isolate(tapered_inputs()))
    input_ids <- getTIIDVector()
    original_inputs <- getOriginalTIVector()
    
    input_indices <- grep(grepname, inputs)
    
    print(paste0("resetTI: input_indices is: ", input_indices))
    
    count <- 1
    
    while (count < length(input_indices) + 1){
      #goes through all the input_indices that matches
      
      print("resetTI: In the while loop.")
      
      index <- input_indices[count]
      input_name <- inputs[index]
      id <- input_ids[index]
      value <- original_inputs[[index]]
      
      print(paste0("resetTI: index is: ", index))
      print(paste0("resetTI: input_name is: ", input_name))
      print(paste0("resetTI: id is: ", id))
      print(paste0("resetTI: value is: ", value))
      
      if (length(grep(" min", input_name, ignore.case = TRUE)) > 0){
        print("resetTI: In the min")
        updateNumericInput(session,
                           inputId = id,
                           label = NULL,
                           value = value
        )
      }
      else if (length(grep(" max", input_name, ignore.case = TRUE)) > 0){
        print("resetTI: In the max")
        updateNumericInput(session,
                           inputId = id,
                           label = NULL,
                           value = value
        )
      }
      else if (length(grep(" max", input_name, ignore.case = TRUE)) > 0){
        print("resetSI: In the max")
        updateNumericInput(session,
                           inputId = id,
                           label = NULL,
                           value = ""
        )
      }
      else{
        print("resetTI: In the else")
        updateSelectInput(session, 
                          id,
                          label = NULL,
                          choices = c("All",unique(as.character(tapered_pps_data[,checkbox_name]))),
                          selected =  "All"
        )
      }#end if-else for grep the input_name
      
      count <- count + 1
    }#end while for length of input_indices
    
    
  }#end resetTI
  
  tapered_df_output <- reactiveValues(data = tapered_pps_data) #end reactive for tapered_df_output
  
  clean_tapered_pps_data <- reactiveValues(data = NA) #the clean data to be downloaded
  
  observeEvent(tapered_inputs(),{
    #'This will observe if any of the inputs of the parameters for tapered extrusion have changed
    #'It does not check to see if the input has been selected, but rather, if the user has changed
    #'the search input.'
    
    print("Input Observed")
    
    if (exists("tivector", e3)){
      #checks to see if the vector has been created yet. This is to prevent the initialization
      #of the program
      
      old_tivector <- getTIVector() #get the old tivector before inputs were changed
      current_tivector <- tapered_inputs()
      setTIVector(current_tivector) #updates the tivector with the new inputs
      
      #produces a vector fo TRUE and FALSE. There should be one element that is different and
      #grep 'FALSE' will find that index.
      index_differ <- grep("FALSE", (current_tivector == old_tivector))
      
      if (length(index_differ) == 0){
        #Nothing will be analyzed
        print("A parameter was changed but the value was changed to what the previous value was")
      }
      else if (length(index_differ) > 1){
        #multiple selected or deselected, so do nothing
      }
      else{
        
        index_name <- names(current_tivector[index_differ])
        value <- current_tivector[index_differ] #gets the value of the new parameter
        
        if (is.null(value) || is.na(value)){
          #Nothing will be analyzed
          print("The value is null or na")
        }
        else{
          addTIStack(index_name) #adds the name to the stack
          current_tistack <- getTIStack() #gets the newstack
          
          #This needs to be updated because of the parenthese
          updated_index_name <- gsub("\\(", "\\\\(", index_name)
          updated_index_name <- gsub("\\)", "\\\\)", updated_index_name)
          updated_index_name <- paste0("^", updated_index_name) #must start with it to avoid mis matches
          
          stack_index <- grep(updated_index_name, current_tistack) #gets the index in the stack
          
          if (length(stack_index) != 1){
            print("The Stack Index has a length != 0")
            print(paste0("The Stack Index is: ", stack_index))
            stopApp() #terminate the program
          }
          else{
            addTDFSeries(stack_index, index_name, value) #updates the df_list and sets the datatable output df
          } #end if-else for the length of the stack index
          
        }#end if-else for the value being na or null
        
      } #end if-else for the length of index_differ
      
    }
    else{
      #if it has not been created, it sets the vector and stack
      #this is mainly to initialize data. The df_list does not need to be initialize as that is done
      #in the addTDFSeries()
      
      setTIVector(tapered_inputs())
      setTIStack(c()) #creates an empty stack since no parameters have been changed yet
    }
  })#end observeEvent for the user inputs
  
  observeEvent(show_vars3(),{
    #' this function will observe when a user checks or unchecks an input. If the input is unchecked,
    #' it removes any of the data cleaning it did in the stack and also resets the value of the
    #' input to the initialized value from the start of the session
    
    print("observeEvent(show_vars3): Checkbox Observed")
    
    if (exists("tcbvector", e3)){
      
      print("observeEvent(show_vars3): The tcbvector Exists")
      
      old_tcbvector <- getTCBVector() #get the old tcbvector before inputs were changed
      current_tcbvector <- show_vars3()
      setTCBVector(current_tcbvector) #updates the tcbvector with the new inputs
      
      #produces a vector fo TRUE and FALSE. There should be one element that is different and
      #grep 'FALSE' will find that index.
      index_differ <- grep("FALSE", (current_tcbvector == old_tcbvector))
      
      if (length(index_differ) == 0){
        #Nothing will be analyzed since the value was changed to TRUE
      }
      else if (length(index_differ) > 1){
        #multiple selected or deselected, so do nothing
      }
      else{
        
        print("observeEvent(show_vars3): The Checkbox Index_Differ Worked")
        
        index_name <- names(current_tcbvector[index_differ])
        value <- current_tcbvector[index_differ] #gets the value of the new parameter
        
        if (is.null(value) || is.na(value)){
          #Nothing will be analyzed
          print("observeEvent(show_vars3): The checkbox value is null or na")
          stopApp()
        }
        else if (value == FALSE){
          #this ensures that the checkbox was unchecked instead of checked
          
          print("observeEvent(show_vars3): The Value of the Checkbox was FALSE")
          
          current_tistack <- getTIStack() #gets the newstack
          
          #This needs to be updated because of the parenthese
          updated_index_name <- gsub("\\(", "\\\\(", index_name)
          updated_index_name <- gsub("\\)", "\\\\)", updated_index_name)
          updated_index_name <- paste0("^", updated_index_name) #must start with it to avoid mis matches
          
          stack_index <- grep(updated_index_name, current_tistack) #gets the index in the stack
          
          if (length(stack_index) == 0){
            print("observeEvent(show_vars3): The Input was not in the Stack")
            #the input that was unchecked is not in the stack, so do nothing
          }
          else if (length(stack_index) == 1 || length(stack_index) == 2){
            #the parameter that was unchecked is in the stack
            #it can be of length 1 or 2 depending of the input parameter had a min and max value
            #if it is a selectize input, there will only be one match (length of 1)
            
            print("observeEvent(show_vars3): The Input was in the Stack")
            print(paste0("observeEvent(show_vars3): The Checkboxname is: ", index_name))
            
            resetTI(index_name) #resets the values of the inputs of the checkbox
            removeTIStack(index_name) #removes the value from the stack
            removeTDFSeries() #removes the inputs associated with the checkbox
          }
          else{
            #none of the conditions were met, most likely multiple matches greater than 2
            print("observeEvent(show_vars3): The Stack Index for the Checkboxes ObserveEvent has a length > 2 or negative")
            stopApp()
          } #end if-else for the length of the stack index
          
        }
        else{
          #if it is not false, do nothing
          print("observeEvent(show_vars3): The Checkbox value was TRUE")
        }#end if-else for the value being na or null
        
      } #end if-else for the length of index_differ
      
    }
    else{
      #if it has not been created, it sets the vector
      #this is mainly to initialize data.
      
      setOriginalTIVector(tapered_inputs()) #initialize the original values
      setTCBVector(show_vars3()) #initialize the input values that will be changing with the app
      
    } #end if-else for tcbvector existing
    
    
  })#end observeEvent for the checkboxes
  

  
  
  # obtain the output of checkbox from functions and make a list to store them----tapered Extrusion PPS Data

  
  show_vars3<-reactive({
    checkboxes <- as.numeric(c(TRUE,input$PCTPN_d,input$PCTPD_d,input$PCTRN_d,input$PCTRD_d, input$PCTPPSN_d,
                               input$PCTRF_d, input$PCTRBQ_d, input$PCTRPBQ_d, input$PCTRFQ_d, 
                               input$PCTRFi_d, input$PCTRCQ_d, input$PCTRC_d, input$PCTRRQ_d,
                               input$PCTRDu_d, input$PCTRADu_d,
                               input$PCTDS_d,input$PCTDLL_d,input$PCTTS_d,
                               input$PCTTLL_d,input$PCTSP_d,input$PCTFT_d,
                 input$PCTBZT1_d,input$PCTBZT2_d,input$PCTBZT3_d,input$PCTCT_d,input$PCTAT_d,
                 input$PCTDT1_d,input$PCTDT2_d,
                 input$PCTPIDI_d, input$PCTPODI_d, input$PCTPWT_d, input$PCTPOR_d, input$PCTPCCT_d,
                 input$PCTDIDI_d, input$PCTDODI_d, input$PCTDWT_d, input$PCTDOR_d, input$PCTDCCT_d,
                 input$PCTPLength_d, input$PCTTLength_d, input$PCTDLength_d, input$PCTToLength_d,
                 input$PCTPPD_d,
                 input$PCTNEXIV_d,input$PCTAnnealed_d,input$PCTCaliper_d,input$PCTOS_d,
                 input$PCTMP_d,input$PCTHT_d,
                 input$PCTSPD_d,input$PCTSLD_d,input$PCTDLN_d,input$PCTULT_d,input$PCTVC_d,
                 input$PCTIRD_d))
    
    names(checkboxes) <- c("Placeholder",
                           "Part Number", "Part Description", "Resin Number", "Resin Description",
                           "PPS Number",
                           "Is Resin Filled?", "Resin Fillers", "Is Resin Colored?", 
                           "Resin Color", "Is Resin Radiopaque?", "Resin Durometer (D)", 
                           "Average Resin Durometer (D)",
                           "Die Size (in)", "Die Land Length (in)",
                           "Tip Size (in)", "Tip Land Length (in)", "Screw Print",
                           "Feedthroat Temperature  F",
                           "Barrel Zone 1 Temperature  F", "Barrel Zone 2 Temperature  F",
                           "Barrel Zone 3 Temperature  F","Clamp Temperature  F",
                           "Adapter Temperature  F","Die 1 Temperature  F", "Die 2 Temperature  F",
                           "Proximal Inner Diameter (in)", "Proximal Outer Diameter (in)",
                           "Proximal Wall Thickness (in)", "Proximal Out of Roundness (in)",
                           "Proximal Concentricity (in)",
                           "Distal Inner Diameter (in)", "Distal Outer Diameter (in)",
                           "Distal Wall Thickness (in)", "Distal Out of Roundness (in)",
                           "Distal Concentricity (in)",
                           "Proximal Length (in)", "Transition Length (in)", "Distal Length (in)",
                           "Total Length (in)", "Perpendicularity (in)",
                           "Nexiv", "Annealed", "Caliper", "OD Sort", "Melt Pump", "Hypo Tip",
                           "Sparker Die", "Slicking Die", "Delamination", "Ultrasonic",
                           "Vacuum Calibration", "Irradiated")
    
    return(checkboxes)
    
    })
  
  #this variable will store all the inputs of the tapered extrusions
  tapered_inputs <- reactive({
    #this variable will store all the inputs of of the tapered extrusions
    inputs3 <- c("Placeholder", input$PCTPN, input$PCTPD, input$PCTRN, input$PCTRD, input$PCTPPSN,
                 "Placeholder", "Placeholder", "Placeholder", "Placeholder", "Placeholder",
                 "Placeholder", "Placeholder", "Placeholder", "Placeholder", "Placeholder",
                input$PCTDS_min, input$PCTDS_max, input$PCTDLL, input$PCTTS_min, input$PCTTS_max,
                input$PCTTLL, input$PCTSP, 
                input$PCTFT_min, input$PCTFT_max, input$PCTBZT1_min, input$PCTBZT1_max,
                input$PCTBZT2_min, input$PCTBZT2_max, input$PCTBZT3_min, input$PCTBZT3_max,
                input$PCTCT_min, input$PCTCT_max, input$PCTAT_min, input$PCTAT_max,
                input$PCTDT1_min, input$PCTDT1_max, input$PCTDT2_min, input$PCTDT2_max,
                input$PCTPIDI_min, input$PCTPIDI_max, input$PCTPODI_min, input$PCTPODI_max, input$PCTPWT_min, 
                input$PCTPWT_max,input$PCTPOR_min, input$PCTPOR_max, input$PCTPCCT_min, input$PCTPCCT_max,
                input$PCTDIDI_min, input$PCTDIDI_max, input$PCTDODI_min, input$PCTDODI_max, input$PCTDWT_min,
                input$PCTDWT_max, input$PCTDOR_min, input$PCTDOR_max, input$PCTDCCT_min, input$PCTDCCT_max,
                input$PCTPLength_min, input$PCTPLength_max, input$PCTTLength_min, input$PCTTLength_max,
                input$PCTDLength_min, input$PCTDLength_max, input$PCTToLength_min, input$PCTToLength_max,
                input$PCTPPD,
                input$PCTNEXIV, input$PCTAnnealed, input$PCTCaliper, input$PCTOS,
                input$PCTMP, input$PCTHT, input$PCTSPD, input$PCTSLD, input$PCTDLN, input$PCTULT,
                input$PCTVC, input$PCTIRD
    )
    names(inputs3) <- c("Placeholder",
                        "Part Number", "Part Description", "Resin Number", "Resin Description",
                       "PPS Number",
                       "Placeholder", "Placeholder", "Placeholder", "Placeholder", "Placeholder",
                       "Placeholder", "Placeholder", "Placeholder", "Placeholder", "Placeholder",
                       "Die Size (in) Min", "Die Size (in) Max", "Die Land Length (in)", 
                       "Tip Size (in) Min","Tip Size (in) Max", "Tip Land Length (in)", "Screw Print",
                       "Feedthroat Temperature  F Min", "Feedthroat Temperature  F Max",
                       "Barrel Zone 1 Temperature  F Min", "Barrel Zone 1 Temperature  F Max",
                       "Barrel Zone 2 Temperature  F Min", "Barrel Zone 2 Temperature  F Max",
                       "Barrel Zone 3 Temperature  F Min", "Barrel Zone 3 Temperature  F Max",
                       "Clamp Temperature  F Min", "Clamp Temperature  F Max",
                       "Adapter Temperature  F Min", "Adapter Temperature  F Max",
                       "Die 1 Temperature  F Min", "Die 1 Temperature  F Max",
                       "Die 2 Temperature  F Min", "Die 2 Temperature  F Max",
                       "Proximal Inner Diameter (in) Min", 
                       "Proximal Inner Diameter (in) Max", 
                       "Proximal Outer Diameter (in) Min",
                       "Proximal Outer Diameter (in) Max",
                       "Proximal Wall Thickness (in) Min", 
                       "Proximal Wall Thickness (in) Max", 
                       "Proximal Out of Roundness (in) Min",
                       "Proximal Out of Roundness (in) Max",
                       "Proximal Concentricity (in) Min",
                       "Proximal Concentricity (in) Max",
                       "Distal Inner Diameter (in) Min", 
                       "Distal Inner Diameter (in) Max",
                       "Distal Outer Diameter (in) Min",
                       "Distal Outer Diameter (in) Max",
                       "Distal Wall Thickness (in) Min", 
                       "Distal Wall Thickness (in) Max", 
                       "Distal Out of Roundness (in) Min",
                       "Distal Out of Roundness (in) Max",
                       "Distal Concentricity (in) Min",
                       "Distal Concentricity (in) Max",
                       "Proximal Length (in) Min", 
                       "Proximal Length (in) Max",
                       "Transition Length (in) Min", 
                       "Transition Length (in) Max", 
                       "Distal Length (in) Min",
                       "Distal Length (in) Max",
                       "Total Length (in) Min",
                       "Total Length (in) Max",
                       "Perpendicularity (in", 
                       "Nexiv", "Annealed", "Caliper", "OD Sort", "Melt Pump", "Hypo Tip",
                       "Sparker Die", "Slicking Die", "Delamination", "Ultrasonic",
                       "Vacuum Calibration", "Irradiated")
    return(inputs3)
  })
  
  
  output$mytable3 <- DT::renderDataTable({
    
    DT::datatable({
      
      Col_PCT <- which(1 == show_vars3())
      
      data_PCT <- tapered_df_output$data #the data frame is set
      data_PCT<-data_PCT[,Col_PCT]
      
      clean_tapered_pps_data$data <- data_PCT #assign the clean table to the data that is available
      #for downloading
      
      return(data_PCT)
    },
    options = list(orderClasses = TRUE,
                   columnDefs = list(list(className = 'dt-center',
                                          targets = "_all"
                   )
                   ),
                   scrollX=TRUE,
                   scrollY=500,
                   autoWidth=TRUE)
    )
  },
  rownames = FALSE, 
  escape = FALSE, #escape allows for html elements to be rendered in the table
  server = FALSE) #end Tapered Extrusion PPS Data
  
  
  
  
  ###
  
  ### The tapered shopping cart section ###
  
  ###
  
  
  taperedshoppingcart <- reactiveValues(
    #this is a shopping cart to hold all the tapered extrusion parts that a user wants.
    #this is linked to the output data, so only the output data located of the associated batches 
    #in the shopping cart is displayed
    data = data.frame("Part" = numeric(0), "Delete Part" = numeric(0),
                      "SAP Batch" = numeric(0), "Delete Batch" = numeric(0),
                      stringsAsFactors = FALSE,
                      check.names = FALSE)
  ) #end taperedshoppingcart
  
  taperedshoppingcartparts <- reactiveValues(
    #this is a shopping cart to hold all the tapered extrusion parts and SAP batches that a user wants.
    #this is linked to the output data, so only the output data located of the associated batches 
    #in the shopping cart is displayed
    data = data.frame("Part" = numeric(0), "Batches?" = numeric(0), "Delete Part" = numeric(0),
                      stringsAsFactors = FALSE,
                      check.names = FALSE)
  ) #end taperedshoppingcart
  
  observeEvent(input$taperedadd_button,{
    #this observes whether the user clicked a button to add a part to the shopping cart
    #get the part
    part <- strsplit(input$taperedadd_button, "_")[[1]][2]
    print(part)
    
    #Action button to delete part
    deletepart <- as.character(
      actionButton(inputId = paste0("button_", part),
                   label = "Delete Part",
                   onclick = 'Shiny.onInputChange(\"tapereddelete_part_button\",  this.id)'))
    
    #Get the SAP batches
    SAP_batches <- tapered_tari_parameter_data$`SAP Batch Number`[tapered_tari_parameter_data$`Material Number` == part]
    numberofbatches <- length(SAP_batches)
    
    #Action button to delete batch
    batch_count <- 1
    vectorofbuttons <- c(rep(0, length(SAP_batches)))
    
    while(batch_count < length(SAP_batches) + 1){
      vectorofbuttons[batch_count] <- as.character(
        actionButton(inputId = paste0("button_", SAP_batches[batch_count]),
                     label = "Delete Batch",
                     onclick = 'Shiny.onInputChange(\"tapereddelete_batch_button\",  this.id)'))
      batch_count <- batch_count + 1
    }
    
    #Vectors of parts and buttons
    partvector <- rep(part, numberofbatches)
    deletepartvector <- rep(deletepart, numberofbatches)
    
    new_data <- cbind(partvector, deletepartvector, SAP_batches, vectorofbuttons)
    
    colnames(new_data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
    taperedshoppingcart$data <- rbind(taperedshoppingcart$data, new_data, stringsAsFactors = FALSE)
    colnames(taperedshoppingcart$data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
  })
  
  observeEvent(input$taperedadd_button,{
    #this observes whether the user clicked a button to add a part to the part only shopping cart
    part <- strsplit(input$taperedadd_button, "_")[[1]][2]
    
    #Action button to delete part
    deletepart <- as.character(
      actionButton(inputId = paste0("button_", part),
                   label = "Delete Part",
                   onclick = 'Shiny.onInputChange(\"tapereddelete_part_button\",  this.id)'))
    
    
    #This determines if there are batches for the part
    SAP_batches <- tapered_tari_parameter_data$`SAP Batch Number`[tapered_tari_parameter_data$`Material Number` == part]
    numberofbatches <- length(SAP_batches)
    
    if(numberofbatches > 0){
      #if there are batches
      batches <- "Yes"
    }
    else{
      batches <- "No"
    }
    
    new_data <- cbind(part, batches, deletepart)
    
    colnames(new_data) <- c("Part", "Batches?", "Delete Part")
    
    
    if (length(grep(part, taperedshoppingcartparts$data$"Part")) == 0){
      taperedshoppingcartparts$data <- rbind(taperedshoppingcartparts$data, new_data, stringsAsFactors = FALSE)
      colnames(taperedshoppingcartparts$data) <- c("Part", "Batches?", "Delete Part")
    }
    else{
      #Do nothing if the part is already there
    }
    
  }) #end observeEvent(input$taperedadd_button)
  
  
  observeEvent(input$taperedaddtable,{
    #this observes for the button taperedaddtable which will add all the parts in the table to the
    #shopping cart.
    
    raw_parts <- unlist(strsplit(unlist(strsplit(tapered_df_output$data$`Part Number`, ">")), "<"))
    
    clean_parts <- raw_parts[seq(3,length(raw_parts), 4)]
    
    count <- 1
    
    while (count < (length(clean_parts) + 1)){
      #iterates through the parts
      
      part <- clean_parts[count]
      
      #Action button to delete part
      deletepart <- as.character(
        actionButton(inputId = paste0("button_", part),
                     label = "Delete Part",
                     onclick = 'Shiny.onInputChange(\"tapereddelete_part_button\",  this.id)'))
      
      #Get the SAP batches
      SAP_batches <- tapered_tari_parameter_data$`SAP Batch Number`[tapered_tari_parameter_data$`Material Number` == part]
      numberofbatches <- length(SAP_batches)
      
      if(numberofbatches > 0){
        #if there are batches
        batches <- "Yes"
        
        #Action button to delete batch
        #' if there are not batches the part will not be added to this shopping cart table but will
        #' be added to the part
        batch_count <- 1
        vectorofbuttons <- c(rep(0, length(SAP_batches)))
        
        while(batch_count < length(SAP_batches) + 1){
          vectorofbuttons[batch_count] <- as.character(
            actionButton(inputId = paste0("button_", SAP_batches[batch_count]),
                         label = "Delete Batch",
                         onclick = 'Shiny.onInputChange(\"tapereddelete_batch_button\",  this.id)'))
          batch_count <- batch_count + 1
        }
        
        #Vectors of parts and buttons
        partvector <- rep(part, numberofbatches)
        deletepartvector <- rep(deletepart, numberofbatches)
        
        new_data <- cbind(partvector, deletepartvector, SAP_batches, vectorofbuttons)
        
        colnames(new_data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
        
        if (length(grep(part, taperedshoppingcart$data$"Part")) == 0){
          taperedshoppingcart$data <- rbind(taperedshoppingcart$data, new_data, stringsAsFactors = FALSE)
          colnames(taperedshoppingcart$data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
        }
        else{
          #Do nothing if the part is already there
        }
        
      }
      else{
        batches <- "No"
      }
      
      new_data <- cbind(part, batches, deletepart)
      
      colnames(new_data) <- c("Part", "Batches?", "Delete Part")
      
      
      if (length(grep(part, taperedshoppingcartparts$data$"Part")) == 0){
        taperedshoppingcartparts$data <- rbind(taperedshoppingcartparts$data, new_data, stringsAsFactors = FALSE)
        colnames(taperedshoppingcartparts$data) <- c("Part", "Batches?", "Delete Part")
      }
      else{
        #Do nothing if the part is already there
      }
      
      count <- count + 1
    }
    
  })
  
  
  observeEvent(input$tapereddelete_part_button,{
    #'this observes whether a person deleted a part from the shopping cart. If the button is clicked
    #'all batches associated to the part are removed
    part <- strsplit(input$tapereddelete_part_button, "_")[[1]][2]
    taperedshoppingcart$data <- taperedshoppingcart$data[taperedshoppingcart$data$'Part' != part,]
    taperedshoppingcartparts$data <- taperedshoppingcartparts$data[taperedshoppingcartparts$data$'Part' != part,]
  })
  
  observeEvent(input$tapereddelete_batch_button,{
    #'this observes whether a person deleted a SAP batch from the shopping cart. If the button is
    #'clicked, the batch is removed from the cart
    batch <- strsplit(input$tapereddelete_batch_button, "_")[[1]][2]
    taperedshoppingcart$data <- taperedshoppingcart$data[taperedshoppingcart$data$'SAP Batch' != batch,]
  })
  
  
  output$taperedshoppingcart <- renderDataTable({
    #'this shopping cart allows a user to select parts and batches they want to examine. Once added
    #'to the cart, they can view all the MES, SAP, and AppStats data
    return(taperedshoppingcart$data)
    },
    filter = "top",
    rownames = FALSE,
    escape = FALSE,
    server = FALSE,
    options = list(orderClasses = TRUE,
                   columnDefs = list(list(className = 'dt-center',targets = "_all")),
                   scrollX=TRUE,
                   scrollY=250,
                   autoWidth=TRUE,
                   pageLength = 5)
    ) #for the shoppingcart
  

  output$taperedshoppingcartparts <- renderDataTable({
    #'this is a table that only lists the parts for quick viewing
    return(taperedshoppingcartparts$data)
  },
  filter = "top",
  rownames = FALSE,
  escape = FALSE,
  server = FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=250,
                 autoWidth=TRUE,
                 pageLength = 5)  # make the shopping cart page shorter
  ) #for the shoppingcart

  

  
  
  output$tapereddownloadSPPSData <- downloadHandler(
    #downloadd the data
    filename = function() { paste("Visible Tapered PPS Data", '.csv', sep='') },
    content = function(file) {
      #I remove the first column so the HTML is not outputed
      output <- clean_tapered_pps_data$data[2:ncol(clean_tapered_pps_data$data)]
      
      if (length(grep("Part Number", colnames(output), ignore.case = TRUE) > 0)){
        #if a user has selected the column
        raw_parts <- unlist(strsplit(unlist(strsplit(tapered_df_output$data$`Part Number`, ">")), "<"))
        clean_parts <- raw_parts[seq(3,length(raw_parts), 4)]
        output$'Part Number' <- clean_parts
      }
      
      if (length(grep("PPS Number", colnames(output), ignore.case = TRUE) > 0)){
        #if a user has selected the column
        raw_pps <- unlist(strsplit(unlist(strsplit(tapered_df_output$data$`PPS Number`, ">")), "<"))
        clean_pps <- raw_parts[seq(3,length(raw_pps), 4)]
        output$'PPS Number' <- clean_pps
      }
      
      write.csv(output, file, row.names = FALSE)
    }
  )
  
  
  output$tapereddownloadSPPSDataAll <- downloadHandler(
    #downlaod the data
    filename = function() { paste("All Tapered PPS Data", '.csv', sep='') },
    content = function(file) {
      #I remove the first column so the HTML is not outputed
      output <- tapered_df_output$data[2:ncol(tapered_df_output$data)]
      
      raw_parts <- unlist(strsplit(unlist(strsplit(tapered_df_output$data$`Part Number`, ">")), "<"))
      clean_parts <- raw_parts[seq(3,length(raw_parts), 4)]
      output$'Part Number' <- clean_parts
      
      raw_pps <- unlist(strsplit(unlist(strsplit(tapered_df_output$data$`PPS Number`, ">")), "<"))
      clean_pps <- raw_parts[seq(3,length(raw_pps), 4)]
      output$'PPS Number' <- clean_pps
      
      write.csv(output, file, row.names = FALSE)
    }
  )
  

  output$taperedshoppingcartpps <- renderDataTable({
    #this is to render a datatable that has all the PPS information of parts that have been saved
    #to the shopping cart
    
    raw_parts <- unlist(strsplit(unlist(strsplit(tapered_pps_data$`Part Number`, ">")), "<"))
    clean_parts <- raw_parts[seq(3,length(raw_parts), 4)]
    
    data <- tapered_pps_data[which(clean_parts %in% taperedshoppingcartparts$data$'Part'),2:ncol(tapered_pps_data)]
    return(data)
    
  },
  filter = "top",
  rownames = FALSE,
  escape = FALSE,
  server = FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE)
  )

  
  
  
  output$taperedcartdownloadpps <- downloadHandler(
    #downlaod the tapered PPS data from the shopping cart
    filename = function() { paste("Tapered PPS Shopping Cart Data", '.csv', sep='') },
    content = function(file) {
      
      output <- tapered_pps_data
      
      if (length(grep("Part Number", colnames(output), ignore.case = TRUE) > 0)){
        #removes the html from the part number
        raw_parts <- unlist(strsplit(unlist(strsplit(output$`Part Number`, ">")), "<"))
        clean_parts <- raw_parts[seq(3,length(raw_parts), 4)]
        output$'Part Number' <- clean_parts
      }
      
      if (length(grep("PPS Number", colnames(output), ignore.case = TRUE) > 0)){
        #removes the html from the PPS number
        raw_pps <- unlist(strsplit(unlist(strsplit(output$`PPS Number`, ">")), "<"))
        clean_pps <- raw_parts[seq(3,length(raw_pps), 4)]
        output$'PPS Number' <- clean_pps
      }
      
      write.csv(output[which(output$`Part Number` %in% taperedshoppingcartparts$data$'Part'),2:ncol(tapered_pps_data)], 
                file, row.names = FALSE)
    }
  )
  
  observeEvent(input$checktaperedresininfo,{
    #this checks all the checkboxes associated with tapered tooling inputs
    updateCheckboxInput(session, inputId = "PCTRF_d", label = "Resin Families",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTRBQ_d", label = "Is Resin Blended with Anything?",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTRPBQ_d", label = "Is Resin a Polymer Blend?",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTRFQ_d", label = "Is Resin Filled?",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTRFi_d", label = "Resin Fillers",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTRCQ_d", label = "Is Resin Colored?",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTRC_d", label = "Resin Color",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTRRQ_d", label = "Is Resin Radiopaque?",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTRDu_d", label = "Resin Durometer (D)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTRADu_d", label = "Average Durometer (D)",value = TRUE)
    
  }) #end obserEvent for input$checktaperedtooling
  
  observeEvent(input$unchecktaperedresininfo,{
    #this unchecks all the checkboxes associated with tapered tooling inputs
    updateCheckboxInput(session, inputId = "PCTRF_d", label = "Resin Families",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTRBQ_d", label = "Is Resin Blended with Anything?",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTRPBQ_d", label = "Is Resin a Polymer Blend?",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTRFQ_d", label = "Is Resin Filled?",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTRFi_d", label = "Resin Fillers",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTRCQ_d", label = "Is Resin Colored?",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTRC_d", label = "Resin Color",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTRRQ_d", label = "Is Resin Radiopaque?",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTRDu_d", label = "Resin Durometer (D)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTRADu_d", label = "Average Durometer (D)",value = FALSE)
    
  }) #end obserEvent for input$unchecktaperedtooling
  
  observeEvent(input$checktaperedtooling,{
    #this checks all the checkboxes associated with tapered tooling inputs
    updateCheckboxInput(session, inputId = "PCTB_d", label = "Barrel",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTDS_d", label = "Die Size (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTDLL_d", label = "Die Land Length (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTTS_d", label = "Tip Size (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTTLL_d", label = "Tip Land Length (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTSP_d", label = "Screw Print",value = TRUE)
    
  }) #end obserEvent for input$checktaperedtooling
  
  
  observeEvent(input$unchecktaperedtooling,{
    #this unchecks all the checkboxes associated with tapered tooling inputs
    updateCheckboxInput(session, inputId = "PCTB_d", label = "Barrel",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTDS_d", label = "Die Size (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTDLL_d", label = "Die Land Length (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTTS_d", label = "Tip Size (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTTLL_d", label = "Tip Land Length (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTSP_d", label = "Screw Print",value = FALSE)
    
  }) #end obserEvent for input$unchecktaperedtooling
  
  
  observeEvent(input$checktaperedparameters,{
    #this checks all the checkboxes associated with tapered processing parameters inputs
    updateCheckboxInput(session, inputId = "PCTFT_d", label = "Feedthroat Temperature F",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTBZT1_d", label = "Barrel Zone 1 Temperature F",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTBZT2_d", label = "Barrel Zone 2 Temperature F",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTBZT3_d", label = "Barrel Zone 3 Temperature F",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTCT_d", label = "Clamp Temperature F",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTAT_d", label = "Adapter Temperature F",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTDT1_d", label = "Die 1 Temperature F",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTDT2_d", label = "Die 2 Temperature F",value = TRUE)
    
  }) #end obserEvent for input$checktaperedparameters
  
  
  observeEvent(input$unchecktaperedparameters,{
    #this unchecks all the checkboxes associated with tapered processing parameters inputs
    updateCheckboxInput(session, inputId = "PCTFT_d", label = "Feedthroat Temperature F",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTBZT1_d", label = "Barrel Zone 1 Temperature F",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTBZT2_d", label = "Barrel Zone 2 Temperature F",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTBZT3_d", label = "Barrel Zone 3 Temperature F",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTCT_d", label = "Clamp Temperature F",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTAT_d", label = "Adapter Temperature F",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTDT1_d", label = "Die 1 Temperature F",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTDT2_d", label = "Die 2 Temperature F",value = FALSE)
    
  }) #end obserEvent for input$unchecktaperedparameters
  
  observeEvent(input$checktapereddimensions,{
    #this checks all the checkboxes associated with tapered dimensional attribute inputs
    updateCheckboxInput(session, inputId = "PCTPIDI_d", label = "Proximal Inner Diameter (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTPODI_d", label = "Proximal Outer Diameter (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTPWT_d", label = "Proximal Wall Thickness (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTPOR_d", label = "Proximal Out of Roundness (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTPCCT_d", label = "Proximal Concentricity (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTDIDI_d", label = "Distal Inner Diameter (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTDODI_d", label = "Distal Outer Diameter (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTDWT_d", label = "Distal Wall Thickness (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTDOR_d", label = "Distal Out of Roundness (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTDCCT_d", label = "Distal Concentricity (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTPLength_d", label = "Proximal Length (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTTLength_d", label = "Transition Length (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTDLength_d", label = "Distal Length (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTToLength_d", label = "Total Length (in)",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTPPD_d", label = "Perpendicularity (in)",value = TRUE)
    
  }) #end obserEvent for input$checktapereddimensions
  
  
  observeEvent(input$unchecktapereddimensions,{
    #this unchecks all the checkboxes associated with tapered dimensional attribute inputs
    updateCheckboxInput(session, inputId = "PCTPIDI_d", label = "Proximal Inner Diameter (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTPODI_d", label = "Proximal Outer Diameter (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTPWT_d", label = "Proximal Wall Thickness (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTPOR_d", label = "Proximal Out of Roundness (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTPCCT_d", label = "Proximal Concentricity (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTDIDI_d", label = "Distal Inner Diameter (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTDODI_d", label = "Distal Outer Diameter (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTDWT_d", label = "Distal Wall Thickness (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTDOR_d", label = "Distal Out of Roundness (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTDCCT_d", label = "Distal Concentricity (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTPLength_d", label = "Proximal Length (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTTLength_d", label = "Transition Length (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTDLength_d", label = "Distal Length (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTToLength_d", label = "Total Length (in)",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTPPD_d", label = "Perpendicularity (in)",value = FALSE)
    
  }) #end obserEvent for input$unchecktapereddimensions
  
  observeEvent(input$checktaperedspecial,{
    #this checks all the checkboxes associated with tapered special operation inputs
    updateCheckboxInput(session, inputId = "PCTNEXIV_d", label = "NEXIV",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTAnnealed_d", label = "Annealed",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTCaliper_d", label = "Caliper",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTOS_d", label = "OD Sort",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTMP_d", label = "Melt Pump",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTHT_d", label = "Hypo Tip",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTSPD_d", label = "Sparker Die",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTSLD_d", label = "Slicking Die",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTDLN_d", label = "Delamination",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTULT_d", label = "Ultrasonic",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTVC_d", label = "Vacuum Calibration",value = TRUE)
    updateCheckboxInput(session, inputId = "PCTIRD_d", label = "Irradiated",value = TRUE)
    
  }) #end obserEvent for input$checktaperedspecial
  
  
  observeEvent(input$unchecktaperedspecial,{
    #this unchecks all the checkboxes associated with tapered special operation inputs
    updateCheckboxInput(session, inputId = "PCTNEXIV_d", label = "NEXIV",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTAnnealed_d", label = "Annealed",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTCaliper_d", label = "Caliper",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTOS_d", label = "OD Sort",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTMP_d", label = "Melt Pump",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTHT_d", label = "Hypo Tip",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTSPD_d", label = "Sparker Die",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTSLD_d", label = "Slicking Die",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTDLN_d", label = "Delamination",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTULT_d", label = "Ultrasonic",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTVC_d", label = "Vacuum Calibration",value = FALSE)
    updateCheckboxInput(session, inputId = "PCTIRD_d", label = "Irradiated",value = FALSE)
    
  }) #end obserEvent for input$unchecktaperedspecial
  
  observeEvent(input$resettaperedinputs,{
    #this will reset the tapered inputs when clicked
    #It first removes all the dataframes that have been created, resets the input values,
    #and then it resets the stack
    setMDFList(NULL) #resetst the DF list
    
    current_inputs <- isolate(tapered_inputs())
    original_inputs <- getOriginalTIVector()
    
    differ_indices <- which(current_inputs != original_inputs)
    value_names <- names(current_inputs[differ_indices])
    
    print(paste0("observeEvent(input$resettaperedinputs): ",value_names))
    
    count <- 1
    #this while loop iterates through all the values that differed
    while (count < length(value_names) + 1){
      current_name <- value_names[count]
      resetTI(current_name)
      count <- count + 1
    }#end while
    
    setTIStack(c()) #creates an empty stack
    
    #sets the data table to the original data set
    tapered_df_output$data <- tapered_pps_data
    
  }) #end obserEvent for input$resettaperedinputs
  
  
  
  
  #### Total ShoppingCart ####
  
  ### Shoppingcart
  
  observeEvent(c(singleshoppingcartparts$data,multishoppingcart$data, taperedshoppingcart$data), {
    #this is a shopping cart to hold all the total extrusion parts and SAP batches that a user wants.
    #this is linked to the output data, so only the output data located of the associated batches 
    #in the shopping cart is displayed
    totalshoppingcart$data = rbind(singleshoppingcart$data, multishoppingcart$data, taperedshoppingcart$data)
  }) #end singleshoppingcart
  
  observeEvent(c(singleshoppingcartparts$data,multishoppingcart$data, taperedshoppingcart$data),{
    #'this will hold only a list of parts, this way it is easier for users to look at all the parts
    #'when there are two many batches in the shopping cart.
    totalshoppingcartparts$data = rbind(singleshoppingcartparts$data, multishoppingcartparts$data, taperedshoppingcartparts$data)
  })
  
  totalshoppingcart <- reactiveValues(
    #this is a shopping cart to hold all the total extrusion parts and SAP batches that a user wants.
    #this is linked to the output data, so only the output data located of the associated batches 
    #in the shopping cart is displayed
    data = data.frame("Part" = numeric(0), "Delete Part" = numeric(0),
                      "SAP Batch" = numeric(0), "Delete Batch" = numeric(0),
                      stringsAsFactors = FALSE,
                      check.names = FALSE)
  ) #end singleshoppingcart
  
  totalshoppingcartparts <- reactiveValues(
    #this is a shopping cart to hold all the singl extrusion parts and SAP batches that a user wants.
    #this is linked to the output data, so only the output data located of the associated batches 
    #in the shopping cart is displayed
    data = data.frame("Part" = numeric(0), "Batches?" = numeric(0), "Delete Part" = numeric(0),
                      stringsAsFactors = FALSE,
                      check.names = FALSE)
  ) #end singleshoppingcart
  
  
  output$totalshoppingcart <- renderDataTable({
    #'this shopping cart allows a user to select parts and batches they want to examine. Once added
    #'to the cart, they can view all the MES, SAP, and AppStats data
    return(totalshoppingcart$data)
  },
  filter = "top",
  rownames = FALSE,
  escape = FALSE,
  server = FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=250,
                 autoWidth=TRUE,
                 pageLength = 5)
  ) #for the shoppingcart
  
  output$totalshoppingcartparts <- renderDataTable({
    #'this is a table that only lists the parts for quick viewing
    return(totalshoppingcartparts$data)
  },
  filter = "top",
  rownames = FALSE,
  escape = FALSE,
  server = FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=250,
                 autoWidth=TRUE,
                 pageLength = 5)  # make the shopping cart page shorter
  ) #for the shoppingcart
  
  
  
  ### PPS Tab
  
  output$totalshoppingcartpps <- renderDataTable({
    #this is to render a datatable that has all the PPS information of parts that have been saved
    #to the shopping cart
    
    raw_parts <- unlist(strsplit(unlist(strsplit(total_pps_data$`Part Number`, ">")), "<"))
    clean_parts <- raw_parts[seq(3,length(raw_parts), 4)]
    
    data <- total_pps_data[which(clean_parts %in% totalshoppingcartparts$data$'Part'),]
    return(data)
    
  },
  filter = "top",
  rownames = FALSE,
  escape = FALSE,
  server = FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 autoWidth=TRUE)
  )
  
  
  
  output$totalcartdownloadpps <- downloadHandler(
    #downlaod the tapered PPS data from the shopping cart
    filename = function() { paste("Total PPS Shopping Cart Data", '.csv', sep='') },
    content = function(file) {
      
      output <- total_pps_data
      
      if (length(grep("Part Number", colnames(output), ignore.case = TRUE) > 0)){
        #removes the html from the part number
        raw_parts <- unlist(strsplit(unlist(strsplit(output$`Part Number`, ">")), "<"))
        clean_parts <- raw_parts[seq(3,length(raw_parts), 4)]
        output$'Part Number' <- clean_parts
      }
      
      if (length(grep("PPS Number", colnames(output), ignore.case = TRUE) > 0)){
        #removes the html from the PPS number
        raw_pps <- unlist(strsplit(unlist(strsplit(output$`PPS Number`, ">")), "<"))
        clean_pps <- raw_parts[seq(3,length(raw_pps), 4)]
        output$'PPS Number' <- clean_pps
      }
      
      write.csv(output[which(output$`Part Number` %in% totalshoppingcartparts$data$'Part'),], 
                file, row.names = FALSE)
    }
  )
  
  
  
  
  
  #### MES Data and OPMR Data ####
  
  ## Single
  
  single_tari_columns_selected <- reactive({
    columns <- c(input$singletaritempcolumns, input$singletaripresscolumns,
                 input$singletarispeedcolumns, input$singletariextracolumns)
    
    column_indices_full <- sort(which(colnames(single_tari_parameter_and_yield_data) %in% columns))
    column_indices_short <- sort(which(colnames(single_tari_parameter_data) %in% columns))
    
    column_list <- list()
    column_list$full <- column_indices_full
    column_list$short <- column_indices_short
    
    return(column_list)
    
  })
  
  single_tari_parametersandyield_reactive <- reactive({
    df <- single_tari_parameter_and_yield_data[single_tari_parameter_and_yield_data$`SAP Batch Number` %in% singleshoppingcart$data$'SAP Batch',
                                               single_tari_columns_selected()$full]
    
    return(df)
  })
  
  output$singleMESparametersandyield <- renderDataTable({
    
    #This returns the table of the MES paramters and SAP yields times based on the SAP batch numbers 
    #in the shopping cart
    
    return (single_tari_parametersandyield_reactive())
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE,
                 colReorder = TRUE))
  
  
  single_tari_parameters_reactive <- reactive({
    
    df <- single_tari_parameter_data[single_tari_parameter_data$`SAP Batch Number` %in% singleshoppingcart$data$'SAP Batch',]
    
    return(df)
  })
  
  output$singleMESparameters <- renderDataTable({

    #This returns the table of the MES paramters and SAP yields times based on the SAP batch numbers 
    #in the shopping cart
    
    return(single_tari_parameters_reactive())
    
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX= TRUE,
                 scrollY=500,
                 autoWidth=TRUE))


  single_tari_time_reactive <- reactive({
    df <- single_tari_time_data[single_tari_time_data$`SAP Batch Number` %in% singleshoppingcart$data$'SAP Batch',]
  })

 
  output$singleMEStime <- renderDataTable({
    #This returns the table of the MES input times based on the SAP batch numbers in the
    #shopping cart
    return(single_tari_time_reactive())
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE,
                 colReorder = TRUE))
  
  
  single_tari_submitter_reactive <- reactive({
    df <- single_tari_submitter_data[single_tari_submitter_data$`SAP Batch Number` %in% singleshoppingcart$data$'SAP Batch',]
  })

  
  output$singleMESsubmitter <- renderDataTable({
    #This returns the table of the MES submitter IDs based on the SAP batch numbers in the
    #shopping cart
    
    return(single_tari_submitter_reactive())
    
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE,
                 colReorder = TRUE))

  
  single_tari_total_reactive <- reactive({
    df <- single_tari_total_data[single_tari_total_data$`SAP Batch Number` %in% singleshoppingcart$data$'SAP Batch',]
  })
  
    
  output$singleMEStotal <- renderDataTable({
    #This returns the table of all MES inputs based on the SAP batch numbers in the
    #shopping cart
    return(single_tari_total_reactive())
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE,
                 colReorder = TRUE))

  
  single_scrapcodes_reactive <- reactive({
    df <- scrapcodes_data[scrapcodes_data$Order %in% singleshoppingcart$data$'SAP Batch',]
  })
  
  output$singlescrapcodes <- renderDataTable({
    #This returns the table of SAP scrap codes based on the SAP batch numbers in the
    #shopping cart
    return(single_scrapcodes_reactive())
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE,
                 colReorder = TRUE))

  
  
  ## Multi MES Data
  
  multi_tari_columns_selected <- reactive({
    columns <- c(input$multitaritempcolumns, input$multitaripresscolumns,
                 input$multitarispeedcolumns, input$multitariextracolumns)
    
    column_indices_full <- sort(which(colnames(multi_tari_parameter_and_yield_data) %in% columns))
    column_indices_short <- sort(which(colnames(multi_tari_parameter_data) %in% columns))
    
    column_list <- list()
    column_list$full <- column_indices_full
    column_list$short <- column_indices_short
    
    
    return(column_list)
    
  })
  
  multi_tari_parametersandyield_reactive <- reactive({
    
    
    df <- multi_tari_parameter_and_yield_data[multi_tari_parameter_and_yield_data$`SAP Batch Number` %in% multishoppingcart$data$'SAP Batch',
                                               multi_tari_columns_selected()$full]
    
    return(df)
  })
  
  output$multiMESparametersandyield <- renderDataTable({
    
    #This returns the table of the MES paramters and SAP yields times based on the SAP batch numbers 
    #in the shopping cart
    
    return (multi_tari_parametersandyield_reactive())
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE,
                 colReorder = TRUE))
  
  
  multi_tari_parameters_reactive <- reactive({
    
    df <- multi_tari_parameter_data[multi_tari_parameter_data$`SAP Batch Number` %in% multishoppingcart$data$'SAP Batch',]
    
    return(df)
  })
  
  output$multiMESparameters <- renderDataTable({
    
    #This returns the table of the MES paramters and SAP yields times based on the SAP batch numbers 
    #in the shopping cart
    
    return(multi_tari_parameters_reactive())
    
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX= TRUE,
                 scrollY=500,
                 autoWidth=TRUE))
  
  
  multi_tari_time_reactive <- reactive({
    df <- multi_tari_time_data[multi_tari_time_data$`SAP Batch Number` %in% multishoppingcart$data$'SAP Batch',]
  })
  
  
  output$multiMEStime <- renderDataTable({
    #This returns the table of the MES input times based on the SAP batch numbers in the
    #shopping cart
    return(multi_tari_time_reactive())
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE,
                 colReorder = TRUE))
  
  
  multi_tari_submitter_reactive <- reactive({
    df <- multi_tari_submitter_data[multi_tari_submitter_data$`SAP Batch Number` %in% multishoppingcart$data$'SAP Batch',]
  })
  
  
  output$multiMESsubmitter <- renderDataTable({
    #This returns the table of the MES submitter IDs based on the SAP batch numbers in the
    #shopping cart
    
    return(multi_tari_submitter_reactive())
    
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE,
                 colReorder = TRUE))
  
  
  multi_tari_total_reactive <- reactive({
    df <- multi_tari_total_data[multi_tari_total_data$`SAP Batch Number` %in% multishoppingcart$data$'SAP Batch',]
  })
  
  
  output$multiMEStotal <- renderDataTable({
    #This returns the table of all MES inputs based on the SAP batch numbers in the
    #shopping cart
    return(multi_tari_total_reactive())
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE,
                 colReorder = TRUE))
  
  
  multi_scrapcodes_reactive <- reactive({
    df <- scrapcodes_data[scrapcodes_data$Order %in% multishoppingcart$data$'SAP Batch',]
  })
  
  output$multiscrapcodes <- renderDataTable({
    #This returns the table of SAP scrap codes based on the SAP batch numbers in the
    #shopping cart
    return(multi_scrapcodes_reactive())
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE,
                 colReorder = TRUE))
  
  
  ## Tapered
  
  tapered_tari_columns_selected <- reactive({
    columns <- c(input$taperedtaritempcolumns, input$taperedtaripresscolumns,
                 input$taperedtarispeedcolumns, input$taperedtariextracolumns)
    
    column_indices_full <- sort(which(colnames(tapered_tari_parameter_and_yield_data) %in% columns))
    column_indices_short <- sort(which(colnames(tapered_tari_parameter_data) %in% columns))
    
    column_list <- list()
    column_list$full <- column_indices_full
    column_list$short <- column_indices_short
    
    return(column_list)
    
  })
  
  tapered_tari_parametersandyield_reactive <- reactive({
    df <- tapered_tari_parameter_and_yield_data[tapered_tari_parameter_and_yield_data$`SAP Batch Number` %in% taperedshoppingcart$data$'SAP Batch',
                                               tapered_tari_columns_selected()$full]
  })
  
  output$taperedMESparametersandyield <- renderDataTable({
    
    #This returns the table of the MES paramters and SAP yields times based on the SAP batch numbers 
    #in the shopping cart
    
    return (tapered_tari_parametersandyield_reactive())
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE,
                 colReorder = TRUE))
  
  
  tapered_tari_parameters_reactive <- reactive({
    
    df <- tapered_tari_parameter_data[tapered_tari_parameter_data$`SAP Batch Number` %in% taperedshoppingcart$data$'SAP Batch',]
    
    return(df)
  })
  
  output$taperedMESparameters <- renderDataTable({
    
    #This returns the table of the MES paramters and SAP yields times based on the SAP batch numbers 
    #in the shopping cart
    
    return(tapered_tari_parameters_reactive())
    
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX= TRUE,
                 scrollY=500,
                 autoWidth=TRUE))
  
  
  tapered_tari_time_reactive <- reactive({
    df <- tapered_tari_time_data[tapered_tari_time_data$`SAP Batch Number` %in% taperedshoppingcart$data$'SAP Batch',]
  })
  
  
  output$taperedMEStime <- renderDataTable({
    #This returns the table of the MES input times based on the SAP batch numbers in the
    #shopping cart
    return(tapered_tari_time_reactive())
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE,
                 colReorder = TRUE))
  
  
  tapered_tari_submitter_reactive <- reactive({
    df <- tapered_tari_submitter_data[tapered_tari_submitter_data$`SAP Batch Number` %in% taperedshoppingcart$data$'SAP Batch',]
  })
  
  
  output$taperedMESsubmitter <- renderDataTable({
    #This returns the table of the MES submitter IDs based on the SAP batch numbers in the
    #shopping cart
    
    return(tapered_tari_submitter_reactive())
    
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE,
                 colReorder = TRUE))
  
  
  tapered_tari_total_reactive <- reactive({
    df <- tapered_tari_total_data[tapered_tari_total_data$`SAP Batch Number` %in% taperedshoppingcart$data$'SAP Batch',]
  })
  
  
  output$taperedMEStotal <- renderDataTable({
    #This returns the table of all MES inputs based on the SAP batch numbers in the
    #shopping cart
    return(tapered_tari_total_reactive())
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE,
                 colReorder = TRUE))
  
  
  tapered_scrapcodes_reactive <- reactive({
    df <- scrapcodes_data[scrapcodes_data$Order %in% taperedshoppingcart$data$'SAP Batch',]
  })
  
  output$taperedscrapcodes <- renderDataTable({
    #This returns the table of SAP scrap codes based on the SAP batch numbers in the
    #shopping cart
    return(tapered_scrapcodes_reactive())
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE,
                 colReorder = TRUE))

  
  
  #### MES Download Buttons ####
  
  
  ## Single Layer Extrusion
  
  output$smpydownload <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Single Layer Extrusion MES Parameters and Yield Data", '.csv', sep='') },
    content = function(file) {
      
      
      write.csv(single_tari_parametersandyield_reactive(), file, row.names = FALSE)
    }
  )
  
  output$smpdownload <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Single Layer Extrusion MES Parameters Data", '.csv', sep='') },
    content = function(file) {
      
      
      write.csv(single_tari_parameters_reactive(), file, row.names = FALSE)
    }
  )
  
  output$smsdownload <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Single Layer Extrusion MES Submitter Data", '.csv', sep='') },
    content = function(file) {
      
      
      write.csv(single_tari_submitter_reactive(), file, row.names = FALSE)
    }
  )
  
  output$smtdownload <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Single Layer Extrusion MES Timestamp Data", '.csv', sep='') },
    content = function(file) {
      
      
      write.csv(single_tari_time_reactive(), file, row.names = FALSE)
    }
  )
  
  output$smtodownload <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Single Layer Extrusion MES Total Data", '.csv', sep='') },
    content = function(file) {
      
      
      write.csv(single_tari_total_reactive(), file, row.names = FALSE)
    }
  )
  
  ## Multi Layer Extrusion
  
  output$mmpydownload <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Multi Layer Extrusion MES Parameters and Yield Data", '.csv', sep='') },
    content = function(file) {
      
      
      write.csv(multi_tari_parametersandyield_reactive(), file, row.names = FALSE)
    }
  )
  
  output$mmpdownload <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Multi Layer Extrusion MES Parameters Data", '.csv', sep='') },
    content = function(file) {
      
      
      write.csv(multi_tari_parameters_reactive(), file, row.names = FALSE)
    }
  )
  
  output$mmsdownload <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Multi Layer Extrusion MES Submitter Data", '.csv', sep='') },
    content = function(file) {
      
      
      write.csv(multi_tari_submitter_reactive(), file, row.names = FALSE)
    }
  )
  
  output$smtdownload <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Multi Layer Extrusion MES Timestamp Data", '.csv', sep='') },
    content = function(file) {
      
      
      write.csv(multi_tari_time_reactive(), file, row.names = FALSE)
    }
  )
  
  output$mmtodownload <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Multi Layer Extrusion MES Total Data", '.csv', sep='') },
    content = function(file) {
      
      
      write.csv(multi_tari_total_reactive(), file, row.names = FALSE)
    }
  )
  
  ## Tapered Extrusion
  
  output$tmpydownload <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Tapered Extrusion MES Parameters and Yield Data", '.csv', sep='') },
    content = function(file) {
      
      
      write.csv(tapered_tari_parametersandyield_reactive(), file, row.names = FALSE)
    }
  )
  
  output$tmpdownload <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Tapered Extrusion MES Parameters Data", '.csv', sep='') },
    content = function(file) {
      
      
      write.csv(tapered_tari_parameters_reactive(), file, row.names = FALSE)
    }
  )
  
  output$tmsdownload <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Tapered Extrusion MES Submitter Data", '.csv', sep='') },
    content = function(file) {
      
      
      write.csv(tapered_tari_submitter_reactive(), file, row.names = FALSE)
    }
  )
  
  output$tmtdownload <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Tapered Extrusion MES Timestamp Data", '.csv', sep='') },
    content = function(file) {
      
      
      write.csv(tapered_tari_time_reactive(), file, row.names = FALSE)
    }
  )
  
  output$tmtodownload <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Tapered Extrusion MES Total Data", '.csv', sep='') },
    content = function(file) {
      
      
      write.csv(tapered_tari_total_reactive(), file, row.names = FALSE)
    }
  )
  
  
  #### AppStats Stuff ####
  
  
  
  #Testing the appstats data
  output$nexiv <- renderDataTable({
    #This returns the table of the Applied Stats Nexiv Data based on the SAP batch numbers in the
    #shopping cart
    data <- nexiv[nexiv$`Batch #` %in% singleshoppingcart$data$'SAP Batch',]
    return(data)
  },
  filter = "top",options = list(orderClasses = TRUE,
                                columnDefs = list(list(className = 'dt-center',targets = "_all")),
                                scrollX=TRUE,
                                scrollY=500,
                                autoWidth=TRUE)
  )
  
  output$laserlinc <- renderDataTable({
    #This returns the table of the Applied Stats laserlinc data based on the SAP batch numbers in the
    #shopping cart
    data <- laserlinc[laserlinc$`Lot Number` %in% singleshoppingcart$data$'SAP Batch',]
    return(data)
  },
  filter = "top",
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE)
  )
  # end Single Extrusion PPS Data Server part and Shopping cart
  
  

  

  
  
  
  #### Shopping Cart add Buttons ####
  
  
# Add Manually part number input button
  
  observeEvent(input$singleMadd_button,{
    #used by single Mnanually add button in the shopping cart
    #get the part from the text input box
    part <- input$SinglePartNum_input
    
      
    print(part)
    
    #Action button to delete part
    deletepart <- as.character(
      actionButton(inputId = paste0("button_", part),
                   label = "Delete Part",
                   onclick = 'Shiny.onInputChange(\"singledelete_part_button\",  this.id)'))
    
    #Get the SAP batches
    SAP_batches <- single_tari_parameter_data$`SAP Batch Number`[single_tari_parameter_data$`Material Number` == part]
    numberofbatches <- length(SAP_batches)
    
    #Action button to delete batch
    batch_count <- 1
    vectorofbuttons <- c(rep(0, length(SAP_batches)))
    
    while(batch_count < length(SAP_batches) + 1){
      vectorofbuttons[batch_count] <- as.character(
        actionButton(inputId = paste0("button_", SAP_batches[batch_count]),
                     label = "Delete Batch",
                     onclick = 'Shiny.onInputChange(\"singledelete_batch_button\",  this.id)'))
      batch_count <- batch_count + 1
    }
    
    #Vectors of parts and buttons
    partvector <- rep(part, numberofbatches)
    deletepartvector <- rep(deletepart, numberofbatches)
    
    new_data <- cbind(partvector, deletepartvector, SAP_batches, vectorofbuttons)
    
    colnames(new_data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
    
    
    if (length(grep(part, singleshoppingcartparts$data$"Part")) == 0){
      #if part is not in the shopping cart yet. add it
      part_updated <- paste0("^", part, "$") #this will be used for grep if the part is in the dataset
      if (length(grep(part_updated, single_pps_data$`Part Number`)) > 0){
        #if it is in the data set, then add the part
        singleshoppingcart$data <- rbind(singleshoppingcart$data, new_data, stringsAsFactors = FALSE)
        colnames(singleshoppingcart$data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
        
      }
      else{
        #otherwise do nothing
      }
    }
    else{
      #else do nothing
    }
    
    
  })
  
  observeEvent(input$singleMadd_button,{
    #this observes whether the user manually add a part to shopping cart
    part <- input$SinglePartNum_input
    
    #Action button to delete part
    
    deletepart <- as.character(
      actionButton(inputId = paste0("button_", part),
                   label = "Delete Part",
                   onclick = 'Shiny.onInputChange(\"singledelete_part_button\",  this.id)'))
    
    #This determines if there are batches for the part
    SAP_batches <- single_tari_parameter_data$`SAP Batch Number`[single_tari_parameter_data$`Material Number` == part]
    numberofbatches <- length(SAP_batches)
    
    if(numberofbatches > 0){
      #if there are batches
      batches <- "Yes"
    }
    else{
      batches <- "No"
    }
    
    new_data <- cbind(part, batches, deletepart)
    
    colnames(new_data) <- c("Part", "Batches?", "Delete Part")
    
    if (length(grep(part, singleshoppingcartparts$data$"Part")) == 0){
      #if part is not in the shopping cart yet. add it
      part_updated <- paste0("^", part, "$") #this will be used for grep if the part is in the dataset
      if (length(grep(part_updated, single_pps_data$`Part Number`)) > 0){
        #if it is in the data set, then add the part
        singleshoppingcartparts$data <- rbind(singleshoppingcartparts$data, new_data, stringsAsFactors = FALSE)
        colnames(singleshoppingcartparts$data) <- c("Part", "Batches?", "Delete Part")
        updateTextInput(session,"SinglePartNum_input",value = "") #update input box, reset it to be blank
        
        
        #The addition worked
        showModal(modalDialog(
          title = "Add Part Number",
          "Success",
          easyClose = T
        ))
        
      }
      else{
        #otherwise do nothing
        #The addition worked
        showModal(modalDialog(
          title = "Add Part Number",
          "A Valid Part Number Was Not Entered",
          easyClose = T
        ))
      }
      
    }
    else{
      #else do nothing
      updateTextInput(session,"SinglePartNum_input",value = "") #update input box, reset it to be blank
      
      showModal(modalDialog(
        title = "Add Part Number",
        "The part is already in the shopping cart",
        easyClose = T
      ))
      
    }
    
  }) #end observeEvent for add single manual
  
  
  
  observeEvent(input$multiMadd_button,{
    #used by multi Mnanually add button in the shopping cart
    #get the part from the text input box
    part <- input$MultiPartNum_input
    
    
    print(part)
    
    #Action button to delete part
    deletepart <- as.character(
      actionButton(inputId = paste0("button_", part),
                   label = "Delete Part",
                   onclick = 'Shiny.onInputChange(\"multidelete_part_button\",  this.id)'))
    
    #Get the SAP batches
    SAP_batches <- multi_tari_parameter_data$`SAP Batch Number`[multi_tari_parameter_data$`Material Number` == part]
    numberofbatches <- length(SAP_batches)
    
    #Action button to delete batch
    batch_count <- 1
    vectorofbuttons <- c(rep(0, length(SAP_batches)))
    
    while(batch_count < length(SAP_batches) + 1){
      vectorofbuttons[batch_count] <- as.character(
        actionButton(inputId = paste0("button_", SAP_batches[batch_count]),
                     label = "Delete Batch",
                     onclick = 'Shiny.onInputChange(\"multidelete_batch_button\",  this.id)'))
      batch_count <- batch_count + 1
    }
    
    #Vectors of parts and buttons
    partvector <- rep(part, numberofbatches)
    deletepartvector <- rep(deletepart, numberofbatches)
    
    new_data <- cbind(partvector, deletepartvector, SAP_batches, vectorofbuttons)
    
    colnames(new_data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
    
    
    if (length(grep(part, multishoppingcartparts$data$"Part")) == 0){
      #if part is not in the shopping cart yet. add it
      part_updated <- paste0("^", part, "$") #this will be used for grep if the part is in the dataset
      if (length(grep(part_updated, multi_pps_data$`Part Number`)) > 0){
        #if it is in the data set, then add the part
        multishoppingcart$data <- rbind(multishoppingcart$data, new_data, stringsAsFactors = FALSE)
        colnames(multishoppingcart$data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
        
      }
      else{
        #otherwise do nothing
      }
    }
    else{
      #else do nothing
    }
    
    
  })
  

  observeEvent(input$multiMadd_button,{
    #this observes whether the user manually add a part to shopping cart
    part <- input$MultiPartNum_input
    
    #Action button to delete part
    deletepart <- as.character(
      actionButton(inputId = paste0("button_", part),
                   label = "Delete Part",
                   onclick = 'Shiny.onInputChange(\"multidelete_part_button\",  this.id)'))
    
    #This determines if there are batches for the part
    SAP_batches <- multi_tari_parameter_data$`SAP Batch Number`[multi_tari_parameter_data$`Material Number` == part]
    numberofbatches <- length(SAP_batches)
    
    if(numberofbatches > 0){
      #if there are batches
      batches <- "Yes"
    }
    else{
      batches <- "No"
    }
    
    new_data <- cbind(part, batches, deletepart)
    
    colnames(new_data) <- c("Part", "Batches?", "Delete Part")
    
    if (length(grep(part, multishoppingcartparts$data$"Part")) == 0){
      #if part is not in the shopping cart yet. add it
      part_updated <- paste0("^", part, "$") #this will be used for grep if the part is in the dataset
      if (length(grep(part_updated, multi_pps_data$`Part Number`)) > 0){
        #if it is in the data set, then add the part
        multishoppingcartparts$data <- rbind(multishoppingcartparts$data, new_data, stringsAsFactors = FALSE)
        colnames(multishoppingcartparts$data) <- c("Part", "Batches?", "Delete Part")
        updateTextInput(session,"MultiPartNum_input",value = "") #update input box, reset it to be blank
        
        
        #The addition worked
        showModal(modalDialog(
          title = "Add Part Number",
          "Success",
          easyClose = T
        ))
        
      }
      else{
        #otherwise do nothing
        #The addition worked
        showModal(modalDialog(
          title = "Add Part Number",
          "A Valid Part Number Was Not Entered",
          easyClose = T
        ))
      }
      
    }
    else{
      #else do nothing
      updateTextInput(session,"MultiPartNum_input",value = "") #update input box, reset it to be blank
      
      showModal(modalDialog(
        title = "Add Part Number",
        "The part is already in the shopping cart",
        easyClose = T
      ))
      
    }
    
  }) #end observeEvent for add multi manual
  
  
  observeEvent(input$taperedMadd_button,{
    #used by tapered Mnanually add button in the shopping cart
    #get the part from the text input box
    part <- input$TaperedPartNum_input
    
    
    print(part)
    
    #Action button to delete part
    deletepart <- as.character(
      actionButton(inputId = paste0("button_", part),
                   label = "Delete Part",
                   onclick = 'Shiny.onInputChange(\"tapereddelete_part_button\",  this.id)'))
    
    #Get the SAP batches
    SAP_batches <- tapered_tari_parameter_data$`SAP Batch Number`[tapered_tari_parameter_data$`Material Number` == part]
    numberofbatches <- length(SAP_batches)
    
    #Action button to delete batch
    batch_count <- 1
    vectorofbuttons <- c(rep(0, length(SAP_batches)))
    
    while(batch_count < length(SAP_batches) + 1){
      vectorofbuttons[batch_count] <- as.character(
        actionButton(inputId = paste0("button_", SAP_batches[batch_count]),
                     label = "Delete Batch",
                     onclick = 'Shiny.onInputChange(\"tapereddelete_batch_button\",  this.id)'))
      batch_count <- batch_count + 1
    }
    
    #Vectors of parts and buttons
    partvector <- rep(part, numberofbatches)
    deletepartvector <- rep(deletepart, numberofbatches)
    
    new_data <- cbind(partvector, deletepartvector, SAP_batches, vectorofbuttons)
    
    colnames(new_data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
    
    
    if (length(grep(part, taperedshoppingcartparts$data$"Part")) == 0){
      #if part is not in the shopping cart yet. add it
      part_updated <- paste0("^", part, "$") #this will be used for grep if the part is in the dataset
      if (length(grep(part_updated, tapered_pps_data$`Part Number`)) > 0){
        #if it is in the data set, then add the part
        taperedshoppingcart$data <- rbind(taperedshoppingcart$data, new_data, stringsAsFactors = FALSE)
        colnames(taperedshoppingcart$data) <- c("Part", "Delete Part", "SAP Batch", "Delete Batch")
        
      }
      else{
        #otherwise do nothing
      }
    }
    else{
      #else do nothing
    }
    
    
  })
  
  observeEvent(input$taperedMadd_button,{
    #this observes whether the user manually add a part to shopping cart
    part <- input$TaperedPartNum_input
    
    #Action button to delete part
    deletepart <- as.character(
      actionButton(inputId = paste0("button_", part),
                   label = "Delete Part",
                   onclick = 'Shiny.onInputChange(\"tapereddelete_part_button\",  this.id)'))
    
    #This determines if there are batches for the part
    SAP_batches <- tapered_tari_parameter_data$`SAP Batch Number`[tapered_tari_parameter_data$`Material Number` == part]
    numberofbatches <- length(SAP_batches)
    
    if(numberofbatches > 0){
      #if there are batches
      batches <- "Yes"
    }
    else{
      batches <- "No"
    }
    
    new_data <- cbind(part, batches, deletepart)
    
    colnames(new_data) <- c("Part", "Batches?", "Delete Part")
    
    if (length(grep(part, taperedshoppingcartparts$data$"Part")) == 0){
      #if part is not in the shopping cart yet. add it
      part_updated <- paste0("^", part, "$") #this will be used for grep if the part is in the dataset
      if (length(grep(part_updated, tapered_pps_data$`Part Number`)) > 0){
        #if it is in the data set, then add the part
        taperedshoppingcartparts$data <- rbind(taperedshoppingcartparts$data, new_data, stringsAsFactors = FALSE)
        colnames(taperedshoppingcartparts$data) <- c("Part", "Batches?", "Delete Part")
        updateTextInput(session,"TaperedPartNum_input",value = "") #update input box, reset it to be blank
        
        
        #The addition worked
        showModal(modalDialog(
          title = "Add Part Number",
          "Success",
          easyClose = T
        ))
        
      }
      else{
        #otherwise do nothing
        #The addition worked
        showModal(modalDialog(
          title = "Add Part Number",
          "A Valid Part Number Was Not Entered",
          easyClose = T
        ))
      }
      
    }
    else{
      #else do nothing
      updateTextInput(session,"TaperedPartNum_input",value = "") #update input box, reset it to be blank
      
      showModal(modalDialog(
        title = "Add Part Number",
        "The part is already in the shopping cart",
        easyClose = T
      ))
      
    }
    
  }) #end observeEvent for add tapered manual
  
  #***************Data Analysis Tab******************************
  
  #### Sampling and Test Method Information ####
  
  single_sampling_data_test <- reactiveValues(data = single_sampling_data)
  
  observeEvent(input$search_single_sampling,{
    search <- c(input$sspn, input$ssppsn, input$ssatt, input$ssmm, input$sssam, input$sslsl,
                input$sslcl, input$sstar, input$ssucl, input$ssusl, input$ssprean, input$sspostan,
                input$ssoff, input$sspostirr)
    columns <- ncol(single_sampling_data)
    count <- 1
    placeholder <- single_sampling_data
    
    while (count < columns + 1){
      placeholder <- placeholder[grep(search[count], placeholder[,count], ignore.case = TRUE),]
      count <- count + 1
    }#end while
    
    
    single_sampling_data_test$data <- placeholder
    
  })
  
  observeEvent(input$reset_single_sampling,{
    single_sampling_data_test$data <- single_sampling_data
  })
  
  
  output$single_sampling_ui <- renderDataTable(single_sampling_data_test$data,
                                          rownames = FALSE,
                                          escape = FALSE,
                                          server = FALSE,
                                          options = list(orderClasses = TRUE,
                                                         columnDefs = list(list(className = 'dt-center',targets = "_all")),
                                                         scrollX=TRUE,
                                                         scrollY=500,
                                                         autoWidth=TRUE))
  
  
  
  #Multi
  multi_sampling_data_test <- reactiveValues(data = multi_sampling_data)
  
  observeEvent(input$search_multi_sampling,{
    search <- c(input$mspn, input$msppsn, input$msatt, input$msmm, input$mssam, input$mslsl,
                input$mslcl, input$mstar, input$msucl, input$msusl, input$msprean, input$mspostan,
                input$msoff, input$mspostirr)
    columns <- ncol(multi_sampling_data)
    count <- 1
    placeholder <- multi_sampling_data
    
    while (count < columns + 1){
      placeholder <- placeholder[grep(search[count], placeholder[,count], ignore.case = TRUE),]
      count <- count + 1
    }#end while
    
    
    multi_sampling_data_test$data <- placeholder
    
  })
  
  observeEvent(input$reset_multi_sampling,{
    multi_sampling_data_test$data <- multi_sampling_data
  })
  
  output$multi_sampling_ui <- renderDataTable(multi_sampling_data_test$data,
                                               rownames = FALSE,
                                               escape = FALSE,
                                               server = FALSE,
                                              options = list(orderClasses = TRUE,
                                                             columnDefs = list(list(className = 'dt-center',targets = "_all")),
                                                             scrollX=TRUE,
                                                             scrollY=500,
                                                             autoWidth=TRUE))
  
  
  
  
  
  #Tapered
  tapered_sampling_data_test <- reactiveValues(data = tapered_sampling_data)
  
  observeEvent(input$search_tapered_sampling,{
    search <- c(input$tspn, input$tsppsn, input$tsatt, input$tsmm, input$tssam, input$tslsl,
                input$tslcl, input$tstar, input$tsucl, input$tsusl, input$tsprean, input$tspostan,
                input$tsoff, input$tspostirr)
    columns <- ncol(tapered_sampling_data)
    count <- 1
    placeholder <- tapered_sampling_data
    
    while (count < columns + 1){
      placeholder <- placeholder[grep(search[count], placeholder[,count], ignore.case = TRUE),]
      count <- count + 1
    }#end while
    
    
    tapered_sampling_data_test$data <- placeholder
    
  })
  
  observeEvent(input$reset_tapered_sampling,{
    tapered_sampling_data_test$data <- tapered_sampling_data
  })
  
  output$tapered_sampling_ui <- renderDataTable(tapered_sampling_data_test$data,
                                               rownames = FALSE,
                                               escape = FALSE,
                                               server = FALSE,
                                               options = list(orderClasses = TRUE,
                                                              columnDefs = list(list(className = 'dt-center',targets = "_all")),
                                                              scrollX=TRUE,
                                                              scrollY=500,
                                                              autoWidth=TRUE))
  
  
  
  
  
  #Extra
  extra_sampling_data_test <- reactiveValues(data = extra_sampling_data)
  
  observeEvent(input$search_extra_sampling,{
    search <- c(input$espn, input$esppsn, input$esatt, input$esmm, input$essam, input$eslsl,
                input$eslcl, input$estar, input$esucl, input$esusl, input$esprean, input$espostan,
                input$esoff, input$espostirr)
    columns <- ncol(extra_sampling_data)
    count <- 1
    placeholder <- extra_sampling_data
    
    while (count < columns + 1){
      placeholder <- placeholder[grep(search[count], placeholder[,count], ignore.case = TRUE),]
      count <- count + 1
    }#end while
    
    
    extra_sampling_data_test$data <- placeholder
    
  })
  
  observeEvent(input$reset_extra_sampling,{
    extra_sampling_data_test$data <- extra_sampling_data
  })
  
  output$extra_sampling_ui <- renderDataTable(extra_sampling_data_test$data,
                                               rownames = FALSE,
                                               escape = FALSE,
                                               server = FALSE,
                                              options = list(orderClasses = TRUE,
                                                             columnDefs = list(list(className = 'dt-center',targets = "_all")),
                                                             scrollX=TRUE,
                                                             scrollY=500,
                                                             autoWidth=TRUE))
  
  
  
  
  
  
  #All
  all_sampling_data_test <- reactiveValues(data = all_sampling_data)
  
  observeEvent(input$search_all_sampling,{
    search <- c(input$aspn, input$asppsn, input$asatt, input$asmm, input$assam, input$aslsl,
                input$aslcl, input$astar, input$asucl, input$asusl, input$asprean, input$aspostan,
                input$asoff, input$aspostirr)
    columns <- ncol(all_sampling_data)
    count <- 1
    placeholder <- all_sampling_data
    
    while (count < columns + 1){
      placeholder <- placeholder[grep(search[count], placeholder[,count], ignore.case = TRUE),]
      count <- count + 1
    }#end while
    
    
    all_sampling_data_test$data <- placeholder
    
  })
  
  observeEvent(input$reset_all_sampling,{
    all_sampling_data_test$data <- all_sampling_data
  })
  
  output$all_sampling_ui <- renderDataTable(all_sampling_data_test$data,
                                               rownames = FALSE,
                                               escape = FALSE,
                                               server = FALSE,
                                            options = list(orderClasses = TRUE,
                                                           columnDefs = list(list(className = 'dt-center',targets = "_all")),
                                                           scrollX=TRUE,
                                                           scrollY=500,
                                                           autoWidth=TRUE))
  
  
  
  
  #### Extra Information ####
  
  #The resin data
  output$resin_data_ui <- renderDataTable(resin_data,
                                          filter = "top",
                                          rownames = FALSE,
                                          escape = FALSE,
                                          server = FALSE,
                                          options = list(orderClasses = TRUE,
                                                         columnDefs = list(list(className = 'dt-center',targets = "_all")),
                                                         scrollX=TRUE,
                                                         scrollY=500,
                                                         autoWidth=TRUE))
  
  #The screw information
  output$screw_data_ui <- renderDataTable(screw_data,
                                          filter = "top",
                                          rownames = FALSE,
                                          escape = FALSE,
                                          server = FALSE,
                                          options = list(orderClasses = TRUE,
                                                         columnDefs = list(list(className = 'dt-center',targets = "_all")),
                                                         scrollX=TRUE,
                                                         scrollY=500,
                                                         autoWidth=TRUE))
  
  

  
  #***********Analysis Tools*********************
  
  
  # #MES Data Analysis
  # 
  # # Swith the data set
  # plotdata <- reactive({
  #   switch(input$Data_set, "Single" = single_tari_parametersandyield_reactive(), 
  #          "Multi"=multi_tari_parametersandyield_reactive(),
  #          "Tapered"=tapered_tari_parametersandyield_reactive())
  # })
  # 
  # 
  # #X-variable& Y-variable
  # output$Xvar_ui<-renderUI({
  #   selectInput("Xvar","X-value",choices=names(plotdata()),selected = "Start Date")
  # })
  # 
  # output$Yvar_ui<-renderUI({
  #   selectInput("Yvar","Y-value",choices=names(plotdata()),selected="Yield Qty")
  # })
  # 
  # #get the x-value
  # xvar<-reactive({
  #   data<-input$Xvar
  #   return(data)
  # })
  # xvals<-reactive({
  #   data<-plotdata()[[xvar()]]
  # })
  # #get the y-value
  # yvar<-reactive({
  #   data<-input$Yvar
  #   return(data)
  # })
  # yvals<-reactive({
  #   data<-plotdata()[[yvar()]]
  # })
  # 
  # 
  # #Group by
  # output$Groupby_ui<-renderUI({
  #   selectInput(
  #     "Groupby","Group by:",
  #     choices=names(plotdata()),selected = "Material Number"
  #   )
  # })
  # 
  # 
  # Text1<-reactive({
  #   #Filter1<-input$Filter1
  #   Group<-input$Groupby
  #   return(Group)
  # })
  # 
  # 
  # ranges2 <- reactiveValues(x = NULL, y = NULL)
  # 
  # output$MES_plot1 <- renderPlot({
  #   Groupby<-factor(plotdata()[,input$Groupby]) #factorize the variables
  #   # Plot Type will depends on the chosen plot type by user
  #   if(length(input$PlotType)==1){
  #     if(input$PlotType=="Scatter"){
  #       print(xvals())
  #       p<-ggplot(plotdata(), aes(xvals(), yvals())) +geom_point(aes(colour=Groupby,shape=Groupby))#+geom_line(aes(colour=Groupby))
  #     } else if (input$PlotType=="Line"){
  #       p<-ggplot(plotdata(), aes(xvals(), yvals())) +geom_line(aes(colour=Groupby))+geom_line(aes(colour=Groupby))
  #     }
  #   } 
  #   else if (length(input$PlotType)==2){
  #     p<-ggplot(plotdata(), aes(xvals(), yvals())) +geom_point(aes(colour=Groupby,shape=Groupby))+geom_line(aes(colour=Groupby))
  #   }
  #   p<-p+labs(x=xvar(),y=yvar(),title=input$plottitle,subtitle=paste("Group by: ",subtitle=input$Groupby))+theme(legend.position = "right",plot.title = element_text(hjust = 0.5,face="bold",color="#000000",size=30),
  #                                                                                                                plot.subtitle = element_text(hjust = 0.5,face="bold",color="#000000",size=15))+labs(caption=paste("The plot is group by:\n",Text1()))
  #   p
  # })
  # 
  # output$plotui<-renderUI({
  #   plotOutput("MES_plot1",height = 400,
  #              hover = hoverOpts(id = "plot_hover", delay = 0),
  #              brush = brushOpts(
  #                id = "MES_plot1_brush",
  #                # delay = 0,
  #                # delayType = input$brush_policy,
  #                # direction = input$brush_dir,
  #                resetOnNew = TRUE))}) #end plotui
  # 
  # observe({
  #   brush <- input$MES_plot1_brush
  #   if (!is.null(brush)) {
  #     ranges2$x <- c(brush$xmin, brush$xmax)
  #     ranges2$y <- c(brush$ymin, brush$ymax)
  #     
  #   } else {
  #     ranges2$x <- NULL
  #     ranges2$y <- NULL
  #   }
  # })
  # 
  # output$MES_plot2 <- renderPlot({
  #   Groupby<-factor(plotdata()[,input$Groupby])
  #   p<-ggplot(plotdata(), aes(xvals(), yvals())) +geom_point(aes(colour=Groupby,shape=Groupby))+coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
  #   p+labs(x=xvar(),y=yvar())
  # })
  # 
  # #display brushed points
  # brushed_data<-reactive({
  #   brushed_data <- brushedPoints(plotdata(), input$MES_plot1_brush,xvar=xvar(),yvar=yvar())
  #   data<-datatable(brushed_data)
  #   return(data)
  # })
  # 
  # 
  # output$plot_brushed_points <-DT::renderDataTable(
  #   {brushed_data()},
  #   options = list(orderClasses = TRUE,
  #                  columnDefs = list(list(className = 'dt-center',
  #                                         targets = "_all"
  #                  )
  #                  ),
  #                  scrollX=TRUE,
  #                  scrollY=500,
  #                  autoWidth=TRUE),
  #   filter = "top",
  #   rownames = FALSE, 
  #   escape = FALSE, #escape allows for html elements to be rendered in the table
  #   server = FALSE
  # )
  
  

  
  #Preview the manually uploaded data set
  # output$UploadDataPreview <-DT ::renderDataTable({
  #   data<-uploadfile[1:5,]
  #   return(data)
  # },
  # options = list(orderClasses = TRUE,
  #                columnDefs = list(list(className = 'dt-center',
  #                                       targets = "_all"
  #                )
  #                ),
  #                scrollX=TRUE,
  #                scrollY=500,
  #                autoWidth=TRUE),
  # rownames = FALSE,
  # escape = FALSE, 
  # server = FALSE) 
  
  
  
  
  
  #Download the plot to local
  
 
  
  # output$plotdownload <- downloadHandler(
  #   filename =  function() {
  #     paste(input$plottitle, input$savetype, sep=".")
  #   },
  #   # content is a function with argument file. content writes the plot to the device
  #   
  # 
  #   content = function(file) {
  #     if(input$savetype == "png")
  #       png(file) # open the png device
  #     else
  #       pdf(file) # open the pdf device
  #     myPlot() 
  #     dev.off()  # turn the device off
  #     
  #   } 
  # )
  
  
  
  
  
  
  #### Extra HTML ####
  
  #This will change the header of the app to match that of the currently selected tab
  tabtitles <- c("Welcome",
                 "Single Layer Extrusion Catalog", "Multi Layer Extrusion Catalog", "Tapered Extrusion Catalog", 
                 "Single Layer Sampling Information", "Multi Layer Sampling Information", "Tapered Sampling Information", "Extra Extrusion Sampling Information", "Total Extrusion Sampling Information",
                 "Single MES Parameters and Yield", "Single MES Parameters", "Single MES Timestamps", "Single MES Submitters", "Single MES Total",
                 "Multi MES Parameters and Yield", "Multi MES Parameters", "Multi MES Timestamps", "Multi MES Submitters", "Multi MES Total",
                 "Tapered MES Parameters and Yield", "Tapered MES Parameters", "Tapered MES Timestamps", "Tapered MES Submitters", "Tapered MES Total",
                 "Single Scrap Codes", "Multi Scrap Codes", "Tapered Scrap Codes",
                 "Resin Information", "Screw Information",
                 "Single Layer Shopping Cart", "Multi Layer Shopping Cart", "Tapered Shopping Cart", "Total Shopping Cart",
                 "MES Data Analysis", "Scrap Analysis", "Financial Data Analysis",
                 "Help")
  
  tabids <- c("welcome",
              "singleppstab", "multippstab", "taperedppstab", 
              "singlesamplingtab", "multisamplingtab", "taperedsamplingtab", "extrasamplingtab", "totalsamplingtab",
              "singlemesparametersandyieldtab", "singlemesparameterstab", "singlemestimetab", "singlemessubmitterstab", "singlemestotaltab",
              "multimesparametersandyieldtab", "multimesparameterstab", "multimestimetab", "multimessubmitterstab", "multimestotaltab",
              "taperedmesparametersandyieldtab", "taperedmesparameterstab", "taperedmestimetab", "taperedmessubmitterstab", "taperedmestotaltab",
              "singlescrapcodestab", "multiscrapcodestab", "taperedscrapcodestab",
              "resininfotab", "screwinfotab",
              "singleshoppingcarttab", "multishoppingcarttab", "taperedshoppingcarttab", "totalshoppingcarttab",
              "MESDataAnalysis", "ScrapAnalysis", "FinancialDataAnalysis",
              "help")
  
  
  output$currenttabtitle <- renderUI({
    
    #finds the index of th tab, gets the tab name from the vector and then renders it as HTML
    return(HTML(tabtitles[grep(input$tabs, tabids, ignore.case = TRUE)]))
    
  })
  
  
  
  
  #### Analysis Tab Updated ####
  
  
  listofgraphs <- list(list("Scatter Chart" = 6,
                            "Line Chart" = 7,
                            "Line Chart with 2 Y-Axes" = 8,
                            "Bar Chart (Horzintal Bars)" = 9,
                            "Column Chart (Vertical Bars)" = 10,
                            "Area Chart" = 11,
                            "Stepped Area Chart" = 12,
                            "Bubble Chart" = 14,
                            "Pie Chart" = 15,
                            "Histogram" = 16,
                            "Motion Chart" = 17,
                            "Annotation Time Line Chart" = 18),
                       list("Scatter Plot" = 19,
                            "Counts Plot" = 20,
                            "Area Chart" = 21,
                            "Ordered Bar Chart" = 22,
                            "Histogram" = 23,
                            "Density Plot" = 24,
                            "Box Plot" = 25,
                            "Pie Chart" = 26,
                            "Bubble Plot" = 27,
                            "Tree Map" = 28,
                            "Marginal Histogram" = 29,
                            "Marginal Boxplot" = 30,
                            "Dendrogram" = 31,
                            "Cluster for PCA" = 32),
                       list("Scatter Plot" = 33,
                            "Line Plot" = 34,
                            "Filled Area Plot" = 35,
                            "Box Plot" = 36,
                            "Histogram" = 37,
                            "2D Histogram" = 38,
                            "Bubble Chart" = 39,
                            "Heat Map" = 40,
                            "Stacked Area Plot" = 41,
                            "3D Scatter Plot" = 42,
                            "3D Line Plot" = 43,
                            "3D Mesh Plot" = 44,
                            "3D Mesh Plot" = 45,
                            "Parallel Coordinates Plot" = 46)
  )
  
  
  observe({
    data <- switch(input$dataset, "1" = single_tari_parametersandyield_reactive(),
                   "2"=multi_tari_parametersandyield_reactive(),
                   "3"=tapered_tari_parametersandyield_reactive())
    plottingdata$data <- data
    plottingdata$filtered_data <- data
  })
  
  
  plottingdata <- reactiveValues(
    #current data for plotting
    data = NULL,
    filtered_data = NULL,
    omitted_data = NULL #stores the data remove because the values are NA
  )
  
  output$graphchoiceoutput <- renderUI(
    selectInput(inputId = "graphtype",
                #the graphs available are dependent on the graph package
                label = "Select a Chart/Graph Type",
                choices = listofgraphs[[as.numeric(input$graphpackage)]]
    )
  )
  
  
  graphinformation <- reactiveValues(
    graphaxeshtml = NULL
  )
  
  
  observe({
    #for googleplots
    
    graphtypeid <- input$graphtype #gets the graph id that lets the program know what type of graph
    
    #'this will store the html to render the next questions that the user must answer to plot the
    #'data.
    #'
    data <- plottingdata$data
    
    if (is.null(need(graphtypeid, message = FALSE))){
      #the need makes sure the data is present
      axeshtml <- switch(graphtypeid,
                         "1" = #not curerntly available
                           Null,
                         "2" = #not curerntly available
                           Null,
                         "3" = #not curerntly available
                           Null,
                         "4" = #not curerntly available
                           Null,
                         "5" = #not curerntly available
                           Null,
                         # "6" = #googleVis Scatter Plot
                         #   tagList(radioButtons("isxcategorical", "Would You Like to have the X-Axis be Categorical (Such as Having the X-Axis be Material Number, Line, Batch, Or Even Columns)",
                         #                        choices = list("No" = 0, "Yes" = 1),
                         #                        selected = "0"),
                         #           conditionalPanel(
                         #             condition = "input.isxcategorical == '0'",
                         #             #if the user does NOT want the x-axis to be categorical
                         #             selectInput("xaxis_data", "Choose Data for the X-Axis",
                         #                         choices = colnames(plottingdata$data),
                         #                         selected = NULL)
                         #           ),
                         #             #if the user does want the x-axis to be categorical
                         #           conditionalPanel(
                         #             condition = "input.isxcategorical == '1'",
                         #             #if the user does want the x-axis to be categorical
                         #             radioButtons("xcategoricalselection", "Select What Grouping You want for the X Axis",
                         #                          choices = list("Material Number" = 1, "Line" = 2, 
                         #                                         "SAP Batch Number" = 3, "The Columns" = 4),
                         #                          selected = "1"),
                         #             uiOutput("xaxis_data_render")
                         #             #the xaxis data will be inputted here by inserUI in the observe
                         #             #event of xcategoricalselection
                         #           ), #end conditionPanel
                         #           selectInput("yaxis_data", "Choose Data for the Y-Axis",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL),
                         #        radioButtons("xaxis_scale", "Choose a Scale for the X-Axis",
                         #                    choices = list("Linear" = 1, "Log" = 2),
                         #                    selected = NULL),
                         #        radioButtons("yaxis_scale", "Choose a Scale for the Y-Axis",
                         #                     choices = list("Linear" = 1, "Log" = 2),
                         #                     selected = NULL)
                         #        ),
                         # "7" = #googleVis Line Chart
                         #   list(radioButtons("isxcategorical", "Would You Like to have the X-Axis be Categorical (Such as Having the X-Axis be Material Number, Line, Batch, Or Even Columns)",
                         #                    choices = list("No" = 0, "Yes" = 1),
                         #                    selected = "0"),
                         #        conditionalPanel(
                         #          condition = "input.isxcategorical == '0'",
                         #          #if the user does NOT want the x-axis to be categorical
                         #          selectInput("xaxis_data", "Choose Data for the X-Axis",
                         #                      choices = colnames(plottingdata$data),
                         #                      selected = NULL)
                         #        ),
                         #        #if the user does want the x-axis to be categorical
                         #        conditionalPanel(
                         #          condition = "input.isxcategorical == '1'",
                         #          #if the user does want the x-axis to be categorical
                         #          radioButtons("xcategoricalselection", "Select What Grouping You want for the X Axis",
                         #                       choices = list("Material Number" = 1, "Line" = 2, 
                         #                                      "SAP Batch Number" = 3, "The Columns" = 4),
                         #                       selected = "1"),
                         #          uiOutput("xaxis_data_render")
                         #          #the xaxis data will be inputted here by inserUI in the observe
                         #          #event of xcategoricalselection
                         #        ), #end conditionPanel
                         #        selectInput("yaxis_data", "Choose Data for the Y-Axis",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL),
                         #        radioButtons("xaxis_scale", "Choose a Scale for the X-Axis",
                         #                     choices = list("Linear" = 1, "Log" = 2),
                         #                     selected = NULL),
                         #        radioButtons("yaxis_scale", "Choose a Scale for the Y-Axis",
                         #                     choices = list("Linear" = 1, "Log" = 2),
                         #                     selected = NULL)
                         #        ),
                         # "8" = #googleVis Line Chart with 2 Y-Axes
                         #   tagList(radioButtons("isxcategorical", "Would You Like to have the X-Axis be Categorical (Such as Having the X-Axis be Material Number, Line, Batch, Or Even Columns)",
                         #                       choices = list("No" = 0, "Yes" = 1),
                         #                       selected = "0"),
                         #           conditionalPanel(
                         #             condition = "input.isxcategorical == '0'",
                         #             #if the user does NOT want the x-axis to be categorical
                         #             selectInput("xaxis_data", "Choose Data for the X-Axis",
                         #                         choices = colnames(plottingdata$data),
                         #                         selected = NULL)
                         #           ),
                         #           #if the user does want the x-axis to be categorical
                         #           conditionalPanel(
                         #             condition = "input.isxcategorical == '1'",
                         #             #if the user does want the x-axis to be categorical
                         #             radioButtons("xcategoricalselection", "Select What Grouping You want for the X Axis",
                         #                          choices = list("Material Number" = 1, "Line" = 2, 
                         #                                         "SAP Batch Number" = 3, "The Columns" = 4),
                         #                          selected = "1"),
                         #             uiOutput("xaxis_data_render")
                         #             #the xaxis data will be inputted here by inserUI in the observe
                         #             #event of xcategoricalselection
                         #           ), #end conditionPanel
                         #        selectInput("yaxis_data1", "Choose Data for the First Y-Axis",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL),
                         #        selectInput("yaxis_data2", "Choose Data for the Second Y-Axis",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL),
                         #        radioButtons("xaxis_scale", "Choose a Scale for the X-Axis",
                         #                     choices = list("Linear" = 1, "Log" = 2),
                         #                     selected = NULL),
                         #        radioButtons("yaxis_scale1", "Choose a Scale for the First Y-Axis",
                         #                     choices = list("Linear" = 1, "Log" = 2),
                         #                     selected = NULL),
                         #        radioButtons("yaxis_scale2", "Choose a Scale for the Second Y-Axis",
                         #                     choices = list("Linear" = 1, "Log" = 2),
                         #                     selected = NULL)
                         #        ),
                         # "9" = #googleVis Bar Chart
                         #   tagList(selectInput("xaxis_data", "Choose Data for the X-Axis",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL),
                         #        selectInput("yaxis_data", "Choose Data for the Y-Axis",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL),
                         #        radioButtons("xaxis_scale", "Choose a Scale for the X-Axis",
                         #                     choices = list("Linear" = 1, "Log" = 2),
                         #                     selected = NULL)
                         #   ),
                         # "10" = #googleVis Column Chart
                         #   list(selectInput("xaxis_data", "Choose Data for the X-Axis",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL),
                         #        selectInput("yaxis_data", "Choose Data for the Y-Axis",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL),
                         #        radioButtons("yaxis_scale", "Choose a Scale for the Y-Axis",
                         #                     choices = list("Linear" = 1, "Log" = 2),
                         #                     selected = NULL)
                         #   ),
                         # "11" = #googleVis Area Chart
                         #   tagList(selectInput("xaxis_data", "Choose Data for the X-Axis",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL),
                         #        selectInput("yaxis_data", "Choose Data for the Y-Axis",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL),
                         #        radioButtons("xaxis_scale", "Choose a Scale for the X-Axis",
                         #                     choices = list("Linear" = 1, "Log" = 2),
                         #                     selected = NULL),
                         #        radioButtons("yaxis_scale", "Choose a Scale for the Y-Axis",
                         #                     choices = list("Linear" = 1, "Log" = 2),
                         #                     selected = NULL)
                         #   ),
                         # "12" = #googleVis Stepped Area Chart
                         #   list(selectInput("xaxis_data", "Choose Data for the X-Axis",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL),
                         #        selectInput("yaxis_data", "Choose Data for the Y-Axis",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL),
                         #        radioButtons("xaxis_scale", "Choose a Scale for the X-Axis",
                         #                     choices = list("Linear" = 1, "Log" = 2),
                         #                     selected = NULL),
                         #        radioButtons("yaxis_scale", "Choose a Scale for the Y-Axis",
                         #                     choices = list("Linear" = 1, "Log" = 2),
                         #                     selected = NULL)
                         #   ),
                         # "13" = #not curerntly available
                         #   Null,
                         # "14" = #googleVis Bubble Chart
                         #   tagList(selectInput("idaxis_data", "Choose Grouping for the ID of the Bubble",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL),
                         #        selectInput("xaxis_data", "Choose Data for the X-Axis",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL),
                         #        selectInput("yaxis_data", "Choose Data for the Y-Axis",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL),
                         #        radioButtons("xaxis_scale", "Choose a Scale for the X-Axis",
                         #                     choices = list("Linear" = 1, "Log" = 2),
                         #                     selected = NULL),
                         #        radioButtons("yaxis_scale", "Choose a Scale for the Y-Axis",
                         #                     choices = list("Linear" = 1, "Log" = 2),
                         #                     selected = NULL),
                         #        selectInput("coloraxis_data", "Choose Grouping for the Color",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL),
                         #        selectInput("sizeaxis_data", "Choose Data for the Size of the Bubble",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL)
                         #   ),
                         # "15" = #googleVis Pie Chart
                         #   tagList(selectInput("idaxis_data", "Choose Data for the ID of Each Slice",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL),
                         #        selectInput("comparisonaxis_data", "Choose Data for Comparison",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL)
                         #   ),
                         # "16" = #googleVis Histogram
                         #   tagList(selectInput("xaxis_data", "Choose Data for the X-Axis",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL),
                         #        selectInput("yaxis_data", "Choose Data for the Y-Axis",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL),
                         #        radioButtons("yaxis_scale", "Choose a Scale for the Y-Axis",
                         #                     choices = list("Linear" = 1, "Log" = 2),
                         #                     selected = NULL)
                         #   ),
                         # "17" = #googleVis Motion Chart
                         #   tagList(selectInput("idaxis_data", "Choose Data for ID of Each Bubble",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL),
                         #        selectInput("timeaxis_data", "Choose Data for the Time Axis",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL)
                         #   )
                         #   ,
                         # "18" = #googleVis Annotation Chart
                         #   tagList(#the time-axis will be chosen automatically as the start date,
                         #        selectInput("timeaxis_data", "Choose Data for the Time Axis",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL),
                         #        selectInput("yaxis_data", "Choose Data for the Y-Axis",
                         #                    choices = colnames(plottingdata$data),
                         #                    selected = NULL),
                         #        radioButtons("yaxis_scale", "Choose a Scale for the Y-Axis",
                         #                     choices = list("Linear" = 1, "Log" = 2),
                         #                     selected = NULL)
                         #   ), #perhaps these will be used later
                         "19" = #GGplot2 Scatter Plot
                           tagList(radioButtons("isxcategorical", "Would You Like to have the X-Axis be Categorical (Such as Having the X-Axis be Material Number, Line, Batch, Or Even Columns)",
                                                choices = list("No" = 0, "Yes" = 1),
                                                selected = "0"),
                                   selectInput("xaxis_data", "Choose Data for the X-Axis",
                                               choices = colnames(plottingdata$data),
                                               selected = "SAP Batch Number"),
                                   selectInput("yaxis_data", "Choose Data for the Y-Axis",
                                               choices = colnames(plottingdata$data),
                                               selected = "Yield Qty")
                           ),
                         "20" = tagList(),
                         "21" = tagList(),
                         "22" = tagList(),
                         "23" = tagList(),
                         "24" = tagList(),
                         "25" = tagList(),
                         "26" = tagList(),
                         "27" = tagList(),
                         "28" = tagList(),
                         "29" = tagList(),
                         "30" = tagList(),
                         "31" = tagList(),
                         "32" = tagList(),
                         "33" = tagList(),
                         "34" = tagList(),
                         "35" = tagList(),
                         "36" = tagList(),
                         "37" = tagList(),
                         "38" = tagList(),
                         "39" = tagList(),
                         "40" = tagList(),
                         "41" = tagList(),
                         "42" = tagList(),
                         "43" = tagList(),
                         "44" = tagList(),
                         "45" = tagList(),
                         "46" = tagList(),
                         "47" = tagList(),
                         "48" = tagList(),
                         "49" = tagList()
      )
      
      graphinformation$graphaxeshtml <- axeshtml
    }
    else{
      graphinformation$graphaxeshtml <- tags$h2("No Data in the Data Set.")
    }
    
    
  })#end observeEvent(input$graphtype)
  
  
  
  
  output$graphaxeshtmloutput <- renderUI({
    #will render the information the user has to input for the axes of the specif graph
    return(graphinformation$graphaxeshtml)
  })
  
  
  observeEvent(input$xcategoricalselection,{
    #this observes what the user seleced for the categorical variable
    
    choice_options <- switch(input$xcategoricalselection,
                             #based on waht the user selected for the categorical variable, this
                             #will change
                             "1" = unique(plottingdata$data[,"Material Number"]),
                             "2" = unique(plottingdata$data[,"Line"]),
                             "3" = unique(plottingdata$data[,"SAP Batch Number"]),
                             "4" = colnames(plottingdata$data)
    )
    
    whatisselected <- switch(input$xcategoricalselection,
                             #depend on the user's selection the initial choices selected changes
                             #all have everything selected except for the "Column" selection
                             "1" = choice_options,
                             "2" = choice_options,
                             "3" = choice_options,
                             "4" = NULL
                             
    )
    
    output$xaxis_data_render <- renderUI(
      selectizeInput(inputId = "xaxis_data",
                     label = "Select the Inputs for the Category (multiple are allowed)", 
                     choices = choice_options,
                     selected = whatisselected,
                     multiple = TRUE) 
      
    )#end output$xaxis_data_render
  })
  
  output$batchfilterchoicesoutput <- renderUI(
    #the UI for choosing the batches for the batch filter
    selectizeInput(inputId = "batchfilterchoices",
                   label = "Select the Batch Numbers",
                   choices = unique(plottingdata$data[,"SAP Batch Number"]),
                   selected = unique(plottingdata$data[,"SAP Batch Number"]),
                   multiple = TRUE)
  )
  
  output$materialfilterchoicesoutput <- renderUI({
    #the UI for choosing the batches for the material filter
    ui <- selectizeInput(inputId = "materialfilterchoices",
                         label = "Select the Material Numbers",
                         choices = unique(plottingdata$data[,"Material Number"]),
                         selected = unique(plottingdata$data[,"Material Number"]),
                         multiple = TRUE)
    return(ui)
  })
  
  output$linefilterchoicesoutput <- renderUI(
    #the UI for choosing the batches for the line filter
    selectizeInput(inputId = "linefilterchoices",
                   label = "Select the Lines",
                   choices = unique(plottingdata$data[,"Line"]),
                   selected = unique(plottingdata$data[,"Line"]),
                   multiple = TRUE)
  )
  
  output$daterangefilteroutput <- renderUI(
    #Ui for the daterange filter
    dateRangeInput(inputId = "daterangefilter",
                   label = 'Start Date range input: yyyy-mm-dd',
                   start = min(plottingdata$filtered_data[,"Start Date"]), 
                   end = max(plottingdata$filtered_data[,"Start Date"])
    )
  )
  
  output$firstcolumnchoiceoutput <- renderUI(
    #UI for columnn to choose the first column filter
    selectInput(inputId = "firstcolumnchoice",
                #'user selects the column to filter
                label = "Select a Column to Filter",
                choices = (colnames(plottingdata$data)[which(colnames(plottingdata$data)
                                                                                 %out%
                                                                                   c("SAP Batch Number",
                                                                                     "Material Number",
                                                                                     "Line",
                                                                                     "Start Date")
                )
                ]
                ),
                #removes columns that already have filters in place
                selected = NULL
    )
  )
  
  output$secondcolumnchoiceoutput <- renderUI(
    #UI for columnn to choose the first column filter
    selectInput(inputId = "secondcolumnchoice",
                #'user selects the column to filter
                label = "Select a Column to Filter",
                choices = (colnames(plottingdata$data)[which(colnames(plottingdata$data)
                                                                                 %out%
                                                                                   c("SAP Batch Number",
                                                                                     "Material Number",
                                                                                     "Line",
                                                                                     "Start Date")
                )
                ]
                ),
                #removes columns that already have filters in place
                selected = NULL
    )
  )
  
  output$firstcolumnfilter <- renderUI(
    #output for the user to select the values for the first column filter
    selectizeInput(inputId = "firstcolumnvalues",
                   label = "Choose the Values",
                   choices = unique(plottingdata$data),
                   selected = unique(plottingdata$data),
                   multiple = TRUE
    )
  )
  
  output$secondcolumnfilter <- renderUI(
    #output for the user to select the values for the second column filter
    selectizeInput(inputId = "secondcolumnvalues",
                   label = "Choose the Values",
                   choices = unique(plottingdata$data),
                   selected = unique(plottingdata$data),
                   multiple = TRUE
    )
  )
  
  output$maingroupchoiceoutput <- renderUI(
    #choosing column for the main grouping
    selectInput(inputId = "maingroupchoice",
                label = "Please Select a Column for the Main Grouping (Color)",
                choices = colnames(plottingdata$data),
                selected = "Material Number",
                multiple = FALSE)
  )
  
  output$subgroupchoiceoutput <- renderUI(
    #choosing column for the main grouping
    selectInput(inputId = "subgroupchoice",
                label = "Please Select a Column for the Sub Grouping (Shape)",
                choices = colnames(plottingdata$data),
                selected = "Material Number",
                multiple = FALSE)
  )
  
  
  # output$googleplot <- renderGvis(
  #   gvisLineChart(data = test_data,
  #                 xvar = "Start Date",
  #                 yvar = "Yield Percentage",
  #                 options=list(gvis.editor="Edit me!",
  #                              explorer="{actions: ['dragToZoom','rightClickToReset'],maxZoomIn:0.05}",
  #                              crosshair="{trigger:'both'}",
  #                              chartArea="{width:'85%',height:'80%'}",
  #                              height= 1000,
  #                              width = 1000),
  #                 chartid = "googleplotid"
  #                 )
  # ) #perhaps this will be used later
  
  
  
  #### filtering functions ####
  
  observe({
    #observe for filtering the data
    
    placeholder_data <- plottingdata$data
    
    usematerialfilter <- input$usematerialfilter == "1"
    usebatchfilter <- input$usebatchfilter == "1"
    uselinefilter <- input$uselinefilter == "1"
    usedatefilter <- input$usedatefilter == "1"
    usefirstcolumnfilter <- input$usefirstcolumnfilter == "1"
    usesecondcolumnfilter <- input$usesecondcolumnfilter == "1"
    
    need_material <- need(input$materialfilterchoices, message = FALSE)
    isolate({
      if (usematerialfilter){
        #filters the data based on the user's material selections
        placeholder_data <- specificfilter(placeholder_data,
                                           "Material Number", 
                                           input$includeexcludematerial,
                                           input$materialfilterchoices
        )
      }
    })
    if (usebatchfilter){
      #filters the data based on the user's batch selections
      placeholder_data <- specificfilter(placeholder_data,
                                         "SAP Batch Number", 
                                         input$includeexcludebatch,
                                         input$batchfilterchoices
      )
    }
    if (uselinefilter){
      #filters the data based on the user's line selections
      placeholder_data <- specificfilter(placeholder_data,
                                         "Line", 
                                         input$includeexcludeline,
                                         input$linefilterchoices
      )
    }
    
    if (usedatefilter){
      #filters the date data
      start <- input$daterangefilter[1]
      end <- input$daterangefilter[2]
      
      placeholder_data <- placeholder_data[placeholder_data[,"Start Date"] >= start,]
      placeholder_data <- placeholder_data[placeholder_data[,"Start Date"] <= end,]
      
    }
    
    
    first_need_condition <- need(input$firstcolumnchoice, message = FALSE)
    
    if (usefirstcolumnfilter && is.null(first_need_condition)){
      if (input$firstcolumnoperator == "1"){
        inequality <- input$firstcolumninequality #inequality symbol
        
        placeholder_data <- switch(inequality,
                                   "1" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          < input$firstcolumninequalityinput,],
                                   "2" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          <= input$firstcolumninequalityinput,],
                                   "3" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          >= input$firstcolumninequalityinput,],
                                   "4" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          > input$firstcolumninequalityinput,]
        )
        
      }
      else if (input$firstcolumnoperator == "2"){
        #range filter
        
        firstinequality <- input$firstcolumnrangemininequality #inequality symbol
        
        placeholder_data <- switch(firstinequality,
                                   "1" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          < input$firstcolumnrangemininput,],
                                   "2" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          <= input$firstcolumnrangemininput,],
                                   "3" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          >= input$firstcolumnrangemininput,],
                                   "4" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          > input$firstcolumnrangemininput,]
        )
        secondinequality <- input$firstcolumnrangemaxinequality #inequality symbol
        
        placeholder_data <- switch(secondinequality,
                                   "1" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          < input$firstcolumnrangemaxinput,],
                                   "2" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          <= input$firstcolumnrangemaxinput,],
                                   "3" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          >= input$firstcolumnrangemaxinput,],
                                   "4" = placeholder_data[placeholder_data[,input$firstcolumnchoice]
                                                          > input$firstcolumnrangemaxinput,]
        )
      }
      else if (input$firstcolumnoperator == "3"){
        #matching values filter
        
        includeorexclude <- input$firstcolumnvaluesie #(1 for include, 0 for exclude)
        
        if (includeorexclude == "1"){
          #include
          placeholder_data <- placeholder_data[which(placeholder_data[,input$firstcolumnchoice]
                                                     %in% input$firstcolumnvalues),]
        }
        else if (includeorexclude == "0"){
          #exclude
          placeholder_data <- placeholder_data[which(placeholder_data[,input$firstcolumnchoice]
                                                     %out% input$firstcolumnvalues),]
        }
        
      }
    }#end use first column filter
    
    second_need_condition <- need(input$secondcolumnchoice, message = FALSE)
    
    if (usesecondcolumnfilter && is.null(second_need_condition)){
      if (input$secondcolumnoperator == "1"){
        inequality <- input$secondcolumninequality #inequality symbol
        
        placeholder_data <- switch(inequality,
                                   "1" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          < input$secondcolumninequalityinput,],
                                   "2" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          <= input$secondcolumninequalityinput,],
                                   "3" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          >= input$secondcolumninequalityinput,],
                                   "4" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          > input$secondcolumninequalityinput,]
        )
        
      }
      else if (input$secondcolumnoperator == "2"){
        #range filter
        
        secondinequality <- input$secondcolumnrangemininequality #inequality symbol
        
        placeholder_data <- switch(secondinequality,
                                   "1" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          < input$secondcolumnrangemininput,],
                                   "2" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          <= input$secondcolumnrangemininput,],
                                   "3" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          >= input$secondcolumnrangemininput,],
                                   "4" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          > input$secondcolumnrangemininput,]
        )
        secondinequality <- input$secondcolumnrangemaxinequality #inequality symbol
        
        placeholder_data <- switch(secondinequality,
                                   "1" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          < input$secondcolumnrangemaxinput,],
                                   "2" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          <= input$secondcolumnrangemaxinput,],
                                   "3" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          >= input$secondcolumnrangemaxinput,],
                                   "4" = placeholder_data[placeholder_data[,input$secondcolumnchoice]
                                                          > input$secondcolumnrangemaxinput,]
        )
      }
      else if (input$secondcolumnoperator == "3"){
        #matching values filter
        
        includeorexclude <- input$secondcolumnvaluesie #(1 for include, 0 for exclude)
        
        if (includeorexclude == "1"){
          #include
          placeholder_data <- placeholder_data[which(placeholder_data[,input$secondcolumnchoice]
                                                     %in% input$secondcolumnvalues),]
        }
        else if (includeorexclude == "0"){
          #exclude
          placeholder_data <- placeholder_data[which(placeholder_data[,input$secondcolumnchoice]
                                                     %out% input$secondcolumnvalues),]
        }
        
      }
    }#end use second column filter
    
    
    
    #cleans the data to plot by removing NA and blank values
    
    if (is.null(need(input$xaxis_data, message = FALSE)) && 
        is.null(need(input$yaxis_data, message = FALSE)) &&
        length(grep(input$xaxis_data, colnames(plottingdata$data))) != 0 &&
        length(grep(input$yaxis_data, colnames(plottingdata$data))) != 0){
      #if both are present, then clean the data to remove NAs and blanks ("")
      #the grep is to stop the rerending of a new dataset from breaking to due to the old selected
      #columns not being in the new dataset.
      
      
      clean_data <- placeholder_data[which(!is.na(placeholder_data[, input$xaxis_data])),]
      clean_data <- clean_data[which(!is.na(clean_data[, input$yaxis_data])),]
      
      omitted_datax1 <- placeholder_data[which(is.na(placeholder_data[, input$xaxis_data])),]
      omitted_datay1 <- placeholder_data[which(is.na(placeholder_data[, input$yaxis_data])),]
      
      clean_data <- clean_data[which(clean_data[, input$xaxis_data] != ""),]
      clean_data <- clean_data[which(clean_data[, input$yaxis_data] != ""),]
      
      omitted_datax2 <- placeholder_data[which(placeholder_data[, input$xaxis_data] == ""),]
      omitted_datay2 <- placeholder_data[which(placeholder_data[, input$yaxis_data] == ""),]
      
      omitted_data <- rbind(omitted_datax1, omitted_datay1, omitted_datax2, omitted_datay2)
      #removes duplicate batches
      omitted_data <- omitted_data[which(!duplicated(omitted_data[,"SAP Batch Number"])),]
      
      plottingdata$omitted_data <- omitted_data
      
      if (nrow(clean_data) != 0){
        clean_data[,input$xaxis_data] <- numericIfPossible(clean_data[,input$xaxis_data])
        clean_data[,input$yaxis_data] <- numericIfPossible(clean_data[,input$yaxis_data])
      }
      
      plottingdata$filtered_data <- clean_data
    }
    else{
      plottingdata$filtered_data <- placeholder_data
    }
    
  })
  
  specificfilter <- function(dataframe, column, includeoption, inputs){
    #takes the material, batch, and line filter to filter the data based on include or exclude
    #and the inputs the user has selected
    
    data <- dataframe
    
    include <- includeoption == "1" #did the user choose to include the data selected
    
    if (include){
      #the user chose to include
      data <- data[which(data[,column] %in% inputs),] #the rows of the data that match
    }
    else{
      #the user chose to exclude
      data <- data[which(data[,column] %out% inputs),] #the rows of the data that match
    }
    
    
    return(data)
    
  }#end specific filter
  
  
  
  #### Edit Plots ####
  
  
  
  #### Ggplot rendering ####
  
  output$testdatatable <- renderDataTable({
    return(plottingdata$filtered_data)
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE,
                 colReorder = TRUE))
  
  plots <- reactiveValues(
    #storing the plots, things when then be added to these base plots as the user edits the plots
    mainplot = NULL,
    zoomplot = NULL
  )
  
  
  
  output$mainplotoutput <- renderPlot({
    data <- plottingdata$filtered_data
    xdata <- data[,input$xaxis_data]
    ydata <- data[,input$yaxis_data]
    
    if (input$isxcategorical == "1"){
      #user has selected for the variable to be categorical
      #this will really only apply to numerical data, but I will convert it to character
      xdata <- as.character(xdata)
      plottingdata$filtered_data[,input$xaxis_data] <- xdata
    }
    
    if (input$xaxis_scale == "2"){
      #user has selected a log scale for the data
      if (is.numeric(xdata)){
        #if it is numeric, change to a log scale
        xdata <-  log10(xdata)
      }
      else{
        #if it is not numeric, output a popup box that says a log scale cannot be used
        #for a categorical variable
        #also updates the radio button to be linear
        showModal(modalDialog(
          title = "Add Part Number",
          "A log scale cannot be applied to a categorical varibale.",
          easyClose = T
        ))
        updateRadioButtons(session, 
                           inputId = "xaxis_scale", 
                           label = "Choose a Scale for the X-Axis",
                           choices = list("Linear" = 1, "Log" = 2),
                           selected = "1")
      }
    }
    
    if (input$yaxis_scale == "2"){
      #user has selected a log scale for the data
      if (is.numeric(ydata)){
        #if it is numeric, change to a log scale
        ydata <-  log10(ydata)
      }
      else{
        #if it is not numeric, output a popup box that says a log scale cannot be used
        #for a categorical variable
        #also updates the radio button to be linear
        showModal(modalDialog(
          title = "Add Part Number",
          "A log scale cannot be applied to a categorical varibale.",
          easyClose = T
        ))
        updateRadioButtons(session, 
                           inputId = "xaxis_scale", 
                           label = "Choose a Scale for the X-Axis",
                           choices = list("Linear" = 1, "Log" = 2),
                           selected = "1")
      }
    }
    
    plot <- ggplot(data, aes(xdata, ydata))
    
    if (input$usemaingroup == "1" && input$usesubgroup == "0"){
      #main grouping but not subgroupind
      maingroup <- factor(data[,input$maingroupchoice])
      plot <- plot + geom_point(aes(colour=maingroup))
    }
    else if (input$usemaingroup == "1" && input$usesubgroup == "1"){
      maingroup <- factor(data[,input$maingroupchoice])
      subgroup <- factor(data[,input$subgroupchoice])
      plot <- plot + geom_point(aes(colour=maingroup,shape=subgroup)) + scale_shape_manual(values=1:nlevels(subgroup))
      #use main grouping and subgrouping
    }
    else{
      plot <- plot + geom_point()
    }
    
    
    changexticks <- input$changexticks == "1"
    if(changexticks){
      xspacing <- input$xtickspacing
      
      if (input$changexticks == "1"){
        #selected to change
        xpresent <- need(xspacing, message = FALSE)
        x_is_integer <- as.integer(xspacing)
        if (is.null(xpresent) && !is.na(x_is_integer)){
          #if it is present and is an integer, it will update the tick spacing
          values <- plottingdata$filtered_data[,isolate(input$xaxis_data)]
          
          
          if (is.numeric(xdata)){
            #continuous if numeric
            xtick_spacing <- seq(min(values), max(values), length.out = as.numeric(xspacing))
            plot <- plot + scale_x_continuous(breaks=c(xtick_spacing))
          }
          else{
            #discrete if not
            xtick_spacing<- values[seq(1, length(values), length.out = as.numeric(xspacing))]
            plot <- plot + scale_x_discrete(breaks=c(xtick_spacing))
          }
          
        }
      }
      
    }
    
    changeyticks <- input$changeyticks == "1"
    if(changeyticks){
      yspacing <- input$ytickspacing
      
      if (input$changeyticks == "1"){
        #selected to change
        ypresent <- need(yspacing, message = FALSE)
        y_is_integer <- as.integer(yspacing)
        if (is.null(ypresent) && !is.na(y_is_integer)){
          #if it is present and is an integer, it will update the tick spacing
          values <- plottingdata$filtered_data[,isolate(input$yaxis_data)]
          
          if (is.numeric(ydata)){
            #continuous if numeric
            ytick_spacing <- seq(min(values), max(values), length.out = as.numeric(yspacing))
            plot <- plot + scale_y_continuous(breaks=c(ytick_spacing))
          }
          else{
            #discrete if not
            ytick_spacing<- values[seq(1, length(values), length.out = as.numeric(yspacing))]
            plot <- plot + scale_ydiscrete(breaks=c(ytick_spacing))
          }
          
        }
      }
      
    }
    
    if (input$boxplotinput == "1"){
      if (is.numeric(xdata)){
        showModal(modalDialog(
          title = "Add Part Number",
          "A boxplot cannot be applied to a continuous X-Axis.",
          easyClose = T
        ))
        updateRadioButtons(session, 
                           inputId = "boxplotinput", 
                           label = "Would you like to overlay a boxplot?",
                           choices = list("No" = 0, "Yes" = 1),
                           selected = "0")
      }
      else{
        plot <- plot + geom_boxplot()
      }
    }
    if (input$usejitter == "1"){
      plot <- plot + geom_jitter()
      if (is.numeric(xdata)){
        showModal(modalDialog(
          title = "Add Part Number",
          "It is not recommended to jitter on a continuous X-Axis.",
          easyClose = T
        ))
      }
    }
    
    
    return(plot)
    
    
  })
  
  brushselection <- reactiveValues(x = NULL, y = NULL) #the values for the brush area
  
  observe({
    #editing the brush if it is selected
    brush <- input$mainplot_brush
    if (!is.null(brush)) {
      brushselection$x <- c(brush$xmin, brush$xmax)
      brushselection$y <- c(brush$ymin, brush$ymax)
      
    } else {
      brushselection$x <- NULL
      brushselection$y <- NULL
    }
  })
  
  output$zoomplot <- renderPlot({
    #the plot that is zoomed in
    data <- plottingdata$filtered_data
    xdata <- data[,input$xaxis_data]
    ydata <- data[,input$yaxis_data]
    
    if (input$isxcategorical == "1"){
      #user has selected for the variable to be categorical
      #this will really only apply to numerical data, but I will convert it to character
      xdata <- as.character(xdata)
    }
    
    if (input$xaxis_scale == "2"){
      #user has selected a log scale for the data
      if (is.numeric(xdata)){
        #if it is numeric, change to a log scale
        xdata <-  log10(xdata)
      }
      else{
        #do nothing
      }
    }
    
    if (input$yaxis_scale == "2"){
      #user has selected a log scale for the data
      if (is.numeric(ydata)){
        #if it is numeric, change to a log scale
        ydata <-  log10(ydata)
      }
      else{
        #do nothing
      }
    }
    
    plot <- ggplot(data, aes(xdata, ydata))
    
    if (input$usemaingroup == "1" && input$usesubgroup == "0"){
      #main grouping but not subgroupind
      maingroup <- factor(data[,input$maingroupchoice])
      plot <- plot + geom_point(aes(colour=maingroup)) + coord_cartesian(xlim = brushselection$x, ylim = brushselection$y, expand = FALSE)
    }
    else if (input$usemaingroup == "1" && input$usesubgroup == "1"){
      maingroup <- factor(data[,input$maingroupchoice])
      subgroup <- factor(data[,input$subgroupchoice])
      plot <- plot + geom_point(aes(colour=maingroup,shape=subgroup)) + scale_shape_manual(values=1:nlevels(subgroup)) + coord_cartesian(xlim = brushselection$x, ylim = brushselection$y, expand = FALSE)
      #use main grouping and subgrouping
    }
    else{
      plot <- plot + geom_point() + coord_cartesian(xlim = brushselection$x, ylim = brushselection$y, expand = FALSE)
    }
    
    changexticks <- input$changexticks == "1"
    if(changexticks){
      xspacing <- input$xtickspacing
      
      if (input$changexticks == "1"){
        #selected to change
        xpresent <- need(xspacing, message = FALSE)
        x_is_integer <- as.integer(xspacing)
        if (is.null(xpresent) && !is.na(x_is_integer)){
          #if it is present and is an integer, it will update the tick spacing
          values <- plottingdata$filtered_data[,isolate(input$xaxis_data)]
          
          
          if (is.numeric(xdata)){
            #continuous if numeric
            xtick_spacing <- seq(min(values), max(values), length.out = as.numeric(xspacing))
            plot <- plot + scale_x_continuous(breaks=c(xtick_spacing))
          }
          else{
            #discrete if not
            xtick_spacing<- values[seq(1, length(values), length.out = as.numeric(xspacing))]
            plot <- plot + scale_x_discrete(breaks=c(xtick_spacing))
          }
          
        }
      }
      
    }
    
    changeyticks <- input$changeyticks == "1"
    if(changeyticks){
      yspacing <- input$ytickspacing
      
      if (input$changeyticks == "1"){
        #selected to change
        ypresent <- need(yspacing, message = FALSE)
        y_is_integer <- as.integer(yspacing)
        if (is.null(ypresent) && !is.na(y_is_integer)){
          #if it is present and is an integer, it will update the tick spacing
          values <- plottingdata$filtered_data[,isolate(input$yaxis_data)]
          
          if (is.numeric(ydata)){
            #continuous if numeric
            ytick_spacing <- seq(min(values), max(values), length.out = as.numeric(yspacing))
            plot <- plot + scale_y_continuous(breaks=c(ytick_spacing))
          }
          else{
            #discrete if not
            ytick_spacing<- values[seq(1, length(values), length.out = as.numeric(yspacing))]
            plot <- plot + scale_ydiscrete(breaks=c(ytick_spacing))
          }
          
        }
      }
      
    }
    
    
    if (input$boxplotinput == "1"){
      if (is.numeric(xdata)){
        #do nothing if it is numeric
      }
      else{
        plot <- plot + geom_boxplot()
      }
    }
    
    if (input$usejitter == "1"){
      plot <- plot + geom_jitter()
    }
    
    return(plot)
  })
  
  output$omitteddatatable <-DT::renderDataTable({
    return(plottingdata$omitted_data)
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE,
                 colReorder = TRUE))
  
  brushed_data<-reactive({
    brushed_data <- brushedPoints(plottingdata$filtered_data, input$mainplot_brush,
                                  xvar=input$xaxis_data,yvar=input$yaxis_data)
    data<-data.frame(brushed_data, stringsAsFactors = FALSE, check.names = FALSE)
    return(data)
  })
  
  output$zoomdatatable <-DT::renderDataTable({
    return(brushed_data())
  },
  filter = "none",
  extensions = 'ColReorder',
  rownames= FALSE,
  options = list(orderClasses = TRUE,
                 columnDefs = list(list(className = 'dt-center',targets = "_all")),
                 scrollX=TRUE,
                 scrollY=500,
                 autoWidth=TRUE,
                 colReorder = TRUE))
  
  
  ## Plot Hovers
  
  output$mainplot_hover_info <- renderUI({
    hover <- input$mainplot_hover
    
    point <- nearPoints(plottingdata$filtered_data, hover, 
                        xvar=input$xaxis_data,yvar=input$yaxis_data,
                        threshold = 5, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b> SAP Batch Number: ", point$"SAP Batch Number", "</b>", 
                    "<br> Material Number: ", point$"Material Number",
                    "<br> Line: ", point$"Line",
                    "<br>", input$xaxis_data, ": ", point[,input$xaxis_data],
                    "<br>", input$yaxis_data, ": ", point[,input$yaxis_data]
      )
      )
      )
    )
  })
  
  output$zoomplot_hover_info <- renderUI({
    hover <- input$zoomplot_hover
    
    point <- nearPoints(plottingdata$filtered_data, hover, 
                        xvar=input$xaxis_data,yvar=input$yaxis_data,
                        threshold = 5, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b> SAP Batch Number: ", point$"SAP Batch Number", "</b>", 
                    "<br> Material Number: ", point$"Material Number",
                    "<br> Line: ", point$"Line",
                    "<br>", input$xaxis_data, ": ", point[,input$xaxis_data],
                    "<br>", input$yaxis_data, ": ", point[,input$yaxis_data]
      )
      )
      )
    )
  })
  
  
  #### Summary Boxes ####
  
  output$sapbatchboxoutput <- renderValueBox({
    valueBox(
      value = length(unique(plottingdata$data[,"SAP Batch Number"])),
      subtitle = "SAP Batches",
      icon = icon("info-circle")
    )
  })
  
  output$materialboxoutput <- renderValueBox({
    valueBox(
      value = length(unique(plottingdata$data[,"Material Number"])),
      subtitle = "Materials",
      icon = icon("info-circle")
    )
  })
  
  output$lineboxoutput <- renderValueBox({
    valueBox(
      value = length(unique(plottingdata$data[,"Line"])),
      subtitle = "Lines",
      icon = icon("info-circle")
    )
  })
  
  output$operatorboxoutput <- renderValueBox({
    valueBox(
      value = length(unique(plottingdata$data[,"Start Operator ID"])),
      subtitle = "Start Operators",
      icon = icon("info-circle")
    )
  })
  
  output$monthlyrunsboxoutput <- renderValueBox({
    numberofmonths <- as.numeric((as.POSIXlt(max(plottingdata$data$`Start Date`), format="%Y-%m-%d") 
                                  - 
                                    as.POSIXlt(min(plottingdata$data$`Start Date`), format="%Y-%m-%d"))/30.44)
    
    ui <- valueBox(
      value = signif(length(plottingdata$data[,"SAP Batch Number"])/numberofmonths, 
                     digits = 4),
      subtitle = "Runs Per Month",
      icon = icon("info-circle")
    )
    return(ui)
  })
  
  output$averageyieldboxoutput <- renderValueBox({
    yieldqty <- sum(as.integer(plottingdata$data$`Yield Qty`[plottingdata$data$`Yield Qty` != ""]))
    scrapqty <- sum(as.integer(plottingdata$data$`Scrap Qty`[plottingdata$data$`Scrap Qty` != ""]))
    
    ui <- valueBox(
      value = signif(((yieldqty/(yieldqty + scrapqty))*100), 
                     digits = 4),
      subtitle = "Average Yield",
      icon = icon("info-circle")
    )
    
    return(ui)
  })
  
  output$startdateboxoutput <- renderValueBox({
    valueBox(
      value = min(plottingdata$data[,"Start Date"]),
      subtitle = "Earliest Run (Year-Month-Day)",
      icon = icon("info-circle")
    )
  })
  
  output$enddateboxoutput <- renderValueBox({
    valueBox(
      value = max(plottingdata$data[,"Start Date"]),
      subtitle = "Latest Run (Year-Month-Day)",
      icon = icon("info-circle")
    )
  })
  
  
  #### Download buttons ####
  
  output$downloadfiltereddata <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Filtered Data", '.csv', sep='') },
    content = function(file) {
      output <- plottingdata$filtered_data
      write.csv(output, file, row.names = FALSE)
    }
  )
  
  output$downloadzoomdata <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Zoom Data", '.csv', sep='') },
    content = function(file) {
      output <- brushed_data()
      write.csv(output, file, row.names = FALSE)
    }
  )
  
  output$downloadomitteddata <- downloadHandler(
    #downlaod the data
    filename = function() { paste("Omitted Data", '.csv', sep='') },
    content = function(file) {
      output <- plottingdata$omitted_data
      write.csv(output, file, row.names = FALSE)
    }
  )
  
  
  
  #### Extra Functions ####
  
  cleanStringSearch <- function(search_term){
    #escapes special characters for the regex search in grep
    search_term <- gsub("\\(", "\\\\(", search_term) #checks for (
    search_term <- gsub("\\)", "\\\\)", search_term) #checks for )
    search_term <- gsub("\\$", "\\\\$", search_term) #checks for $
    search_term <- gsub("\\[", "\\\\[", search_term) #checks for [
    search_term <- gsub("\\]", "\\\\]", search_term) #checks for ]
    search_term <- gsub("\\//", "\\\\//", search_term) #checks for //
    search_term <- gsub("\\{", "\\\\{", search_term) #checks for {
    search_term <- gsub("\\}", "\\\\}", search_term) #checks for }
    search_term <- gsub("\\$", "\\\\$", search_term) #checks for $
    search_term <- gsub("\\^", "\\\\^", search_term) #checks for ^
    search_term <- gsub("\\?", "\\\\?", search_term) #checks for ?
    
    
    return(search_term)
    
  }#end cleanStringSearch
  
  
  `%out%` <- function(a,b){
    #determines which values of a are not in b
    ! a %in% b
  } 
  
  
  numericIfPossible <- function(vector){
    #converts the vector to numeric if possible
    if (suppressWarnings(all(!is.na(as.numeric(as.character(vector)))))) {
      return(as.numeric(as.character(vector)))
    } else {
      return(vector)
    }
  }
  
  modeofvector <- function(x) {
    #gets the mode of the vector
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  
  
  } #end server

# Run the application 
shinyApp(ui = ui, server = server)
