
#' Remove weird characters from strings
#'
#' Removes any hidden characters or non-alphanumeric characters from strings
#' @param strings vector of character strings
#' @return Returns strings without hidden characters or non-alphanumeric characters
clean_strings <- function(strings){
  
  cleaned_strings <- sapply(
    strings,
    FUN = function(string){
      
      # Removed escaped characters
      string <- gsub(
        pattern = "\n|\t", 
        replacement = "",
        x = string
      )
      
      # Replace any non alphabet/numbers
      string <- gsub(
        pattern = "[^[:alnum:]]", 
        replacement = " ", 
        x = string
      )
      
      # Convert to lowercase
      string <- tolower(string)
      
      return(string)
    }
  )

  return(cleaned_strings)
}

#' Finds and summarises transactions that couldn't be classified
#'
#' Given transactions dataframe with "Type" column indicating whether classified.
#' This function finds unclassified transactions, sums and counts them.
#' Note, should have run classify_transactions on transactions dataframe
#' before using this function!
#' @param transactions dataframe containing transaction details
#' @param in_column name of column in transactions dataframe containing money going in (credit)
#' @param out_column name of column in transactions dataframe containing money going out (debit)
#' @return Returns dataframe of unique transaction description, their cost and number of appearances
identify_and_summarise_unclassified_transactions <- function(transactions, in_column, out_column){
  
  # Identify unclassified transactions
  unclassified <- transactions[transactions$Type == "Unclassified", ]
  
  # Add cost column
  unclassified[is.na(unclassified[, in_column]), in_column] <- 0
  unclassified[is.na(unclassified[, out_column]), out_column] <- 0
  unclassified$cost <- unclassified[, in_column] - unclassified[, out_column]
  
  # Identify standing orders/direct debits?
  unique_descriptions <- do.call(
    data.frame,
    aggregate(
      unclassified$cost,
      by = list(unclassified[, description_column]),
      FUN = function(values){
        
        # Calculate sum
        sum <- sum(values, na.rm = TRUE)
        
        # Count number of values
        count <- length(values)
        
        return(c("sum" = sum, "count" = count))
      }
    )
  )
  colnames(unique_descriptions) <- c("Description", "Total cost", "Number of transactions")
  
  # Order by cost and then count
  unique_descriptions <- unique_descriptions[
    order(
      unique_descriptions$`Total cost`,
      unique_descriptions$`Number of transactions`, 
      decreasing = c(FALSE, TRUE)
    ),
  ]
  
  return(unique_descriptions)
}

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
  column_names <- colnames(transactions_coding_df)
  if(column_names[1] != "Type" || column_names[2] != "Patterns" || ncol(transactions_coding_df) != 2){
    stop(paste("ERROR! Input transactions coding dataframe doesn't expected structure. Should have two columns",
               "entitled \"Type\" and \"Patterns\"!"))
  }
  
  # Changer patterns column to lowercase
  transactions_coding_df$Patterns <- tolower(transactions_coding_df$Patterns)
  
  # Initialise list to store transaction types and patterns
  transaction_coding_list <- lapply(
    transactions_coding_df$Patterns,
    FUN=function(delimited_patterns, delimiter){
      
      # Extract patterns
      patterns <- unlist(strsplit(delimited_patterns, split = delimiter))
      
      # Clean them up
      patterns <- clean_strings(patterns)
      
      return(patterns)
    },
    delimiter
  )
  names(transaction_coding_list) <- transactions_coding_df$Type
  
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
  transaction_type <- "Unclassified"
  type_pattern <- "Unclassified"
  
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
#' @param descriptions vector of string description of transactions
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

#' Calculate total transactions in and out by grouping columns
#'
#' Summarise total money going in (credit) and out (debit) of account (based on transactions)
#' by selected grouping columns.
#' @param transactions dataframe containing transaction details
#' @param in_column name of column in transactions dataframe containing money going in (credit)
#' @param out_column name of column in transactions dataframe containing money going out (debit)
#' @param grouping_columns vector of columns to group by
#' @param calculate_average boolean value to determine whether to add row at bottom for averages. Defaults to TRUE
#' @param calculate_difference boolean value to determine whether to calculate difference between in and out for each row. Defaults to TRUE
#' @param ... additional parameters to send to aggregate
#' @return Returns of dataframe recording total money in and out by month. If selected will include:
#'         - difference column (in minus out) -what you save/lose each month
#'         - Average row at bottom calculating average of in, out, and difference across months
summarise_transactions <- function(transactions, in_column, 
                                   out_column, grouping_columns,
                                   calculate_average = TRUE,
                                   calculate_difference = TRUE,
                                   ...){
  
  # Note required columns
  required_columns <- c(in_column, out_column, grouping_columns)
  
  # Check the required columns are in the transactions dataframe
  column_names <- colnames(transactions)
  if(sum(required_columns %in% column_names) != length(required_columns)){
    stop(paste0("ERROR! The required columns (", 
                paste(required_columns, collapse = ", "),
                ") are not present in the transaction dataframe!"))
  }
  
  # Build grouping list
  column_values_for_grouping <- lapply(
    grouping_columns,
    FUN=function(column, transactions){
      return(transactions[, column])
    },
    transactions
  )
  
  # Calculate summary stats by grouping columns
  summary_stats <- aggregate(
    transactions[, c(in_column, out_column)],
    by=column_values_for_grouping,
    ...,
    na.rm = TRUE,
    drop = FALSE
  )
  colnames(summary_stats)[seq_along(grouping_columns)] <- grouping_columns
  
  # Add difference column - if requested
  if(calculate_difference){
    summary_stats$difference <- summary_stats[, in_column] - summary_stats[, out_column]
  }
  
  # Calculate average in and out
  if(calculate_average){
    n_row <- nrow(summary_stats)
    column_names <- colnames(summary_stats)
    value_columns <- which(column_names %in% grouping_columns == FALSE)
    summary_stats[n_row + 1, 1] <- "Average"
    summary_stats[n_row + 1, value_columns] <- 
      colMeans(summary_stats[, value_columns], na.rm=TRUE)
  }
  
  # Check if grouped by month column
  if("month" %in% grouping_columns){
    summary_stats <- order_by_month(summary_stats)
  }
  
  return(summary_stats)
}

#' Order dataframe by month column
#'
#' For dataframe with month column (month formatted as %m/%Y) function will
#' order rows by this column
#' @param by_month_data dataframe with a month column
#' @param month_column name of month column. Defaults to "month"
#' @return returns datframe ordered by month column
order_by_month <- function(by_month_data, month_column = "month"){
  
  # Check month column present
  if(month_column %in% colnames(by_month_data) == FALSE){
    stop(paste0("Month column (", month_column, ") not present in data!"))
  }
  
  # Get months and format as dates
  months <- as.Date(
    paste0("01/", by_month_data[, month_column]),
    format = "%d/%m/%Y"
  )
  
  # Get order of months
  month_order <- order(months)
  
  return(by_month_data[month_order, ])
}