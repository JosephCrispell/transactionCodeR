#' Build transaction classifications from dataframe
#'
#' Given dataframe of transaction types and delimited patterns this 
#' parses delimited patterns and stores results in list:
#' - key: type
#' - value: [patterns]
#' @param transactions_coding_df Dataframe of transaction Type and Patterns
#' @param delimiter Delimiter used in pattern column to separate patterns
#' @return Returns list of transaction types and patterns that define them
build_transaction_coding_dictionary <- function(transactions_coding_df, delimiter = ";"){
  
  # Check transactions coding dataframe
  col_names <- colnames(transactions_coding_df)
  if(col_names[1] != "Type" || col_names[2] != "Patterns" || ncol(transactions_coding_df) != 2){
    stop(paste("Input transactions coding dataframe doesn't expected structure. Should have two columns",
               "entitled \"Type\" and \"Patterns\"!"))
  }
  
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