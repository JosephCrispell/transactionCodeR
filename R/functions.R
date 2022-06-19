#' Build transaction classifications from dataframe
#'
#' Given dataframe of transaction types and delimited patterns this 
#' parses delimited patterns and stores results in list:
#' - key: type
#' - value: [patterns]
#' @param transactions_coding_df dataframe of transaction Type and Patterns
#' @param delimiter delimiter used in pattern column to separate patterns
#' @return Returns list of transaction types and patterns that define them
build_transaction_coding_dictionary <- function(transactions_coding_df, delimiter = ";"){
  
  # Check transactions coding dataframe
  col_names <- colnames(transactions_coding_df)
  if(col_names[1] != "Type" || col_names[2] != "Patterns" || ncol(transactions_coding_df) != 2){
    stop(paste("ERROR! Input transactions coding dataframe doesn't expected structure. Should have two columns",
               "entitled \"Type\" and \"Patterns\"!"))
  }
  
  # Changer patterns column to lowercase
  transactions_coding_df$Patterns <- tolower(transactions_coding_df$Patterns)
  
  # Initialise list to store transaction types and patterns
  transaction_coding_list <- list()
  
  # Examine each transaction coding
  for(row in seq_len(nrow(transactions_coding_df))){
    
    # Extract patterns for current transaction type
    patterns <- unlist(strsplit(transactions_coding_df[row, "Patterns"], split = delimiter))
    
    # Store the patterns
    transaction_coding_list[[transactions_coding_df[row, "Type"]]] <- patterns
  }
  
  return(transaction_coding_list)
}

#' Classify transaction description using key words
#'
#' Classifies a transaction description type using a key word dictionary
#' (list) describing key words (patterns) for different transaction types
#' @param description string description of transaction
#' @param transaction_coding list of pattern vectors defining transaction types
#' @return Returns of named vector with transaction type and pattern that matched
classify_transaction <- function(description, transaction_coding){
  
  # Check description is a string
  if(is.character(description) == FALSE){
    stop("ERROR! The transaction description provided is not in the correct format. Expecting string!")
  }
  
  # Initialise variable to store transaction type
  transaction_type <- NA
  type_pattern <- NA
  
  # Examine each transaction type
  for(type in names(transaction_coding)){
    
    # Examine each pattern for current transaction type
    for(pattern in transaction_coding[[type]]){
      
      # Check for match in transaction description
      if(grepl(pattern, description, ignore.case = TRUE)){
        transaction_type <- type
        type_pattern <- pattern
        break
      }
    }
  }
  
  return(c("type"=transaction_type, "pattern"=type_pattern))
}

#' Classify transaction descriptions using key words
#'
#' Classifies types for transaction descriptions type using a key word dictionary
#' (list) describing key words (patterns) for different transaction types
#' @param description string description of transaction
#' @param transaction_coding list of pattern vectors defining transaction types
#' @return Returns of named vector with transaction type and pattern that matched
classify_transactions <- function(descriptions, transaction_coding){
  
  # Classify each description - results stored in list
  types <- lapply(
    transactions[, description_column], 
    FUN=classify_transaction,
    transaction_coding
  )
  
  # Convert description types from list to dataframe
  types <- do.call(rbind, types)
  
  return(types)
}