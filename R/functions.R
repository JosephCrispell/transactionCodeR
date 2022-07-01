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
      unlist(strsplit(delimited_patterns, split = delimiter))
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
  
  return(summary_stats)
}

#' Plot values by month
#'
#' Given dataframe with months in one column and values in other column(s),
#' this function will plot the values against the months
#' @param data dataframe with month and value columns
#' @param month_column column with months in. Defaults to "month"
#' @param y_axis_label label for Y axis. Defaults to "£"
#' @param title title for plot. Defaults to "Values by month"
#' @param point_shape shape for points on line. Defaults to 19 (circle)
#' @param legend_x x position for legend, can also be text version of position ("bottomleft"). Defaults to "left".
#' @param legend_y y position for legend. Defaults to NULL.
#' @param add_zero_line boolean value indicating whether to add horizontal line at zero on Y axis. Defaults to TRUE.
plot_values_by_month_static <- function(
  data, month_column = "month", y_axis_label = "£",
  title = "Values by month", point_shape = 19,
  legend_x = "left", legend_y = NULL, add_zero_line = TRUE
){
  
  # Get months and month column index
  column_names <- colnames(data)
  month_column_index <- which(column_names == month_column)
  months <- data[, month_column]
  
  # Calculate Y axis limits
  y_limits <- range(data[, -month_column_index], na.rm = TRUE)
  
  # Define colours for each column of values
  colours <- rainbow(ncol(data) - 1)
  
  # Create an empty plot
  plot(x=NA, y=NA,
       xlim=c(0, length(months) - 0.5), 
       ylim=y_limits,
       bty="n", las=1, xaxt = "n", xlab="", 
       ylab=y_axis_label, main = title)
  
  # Add X axis
  x_ticks <- seq(0.5, length(months) - 0.5)
  axis(side = 1, at = x_ticks, labels = months)
  
  # Add line for each set of values
  for(column_index in seq_len(ncol(data))[-month_column_index]){
    
    points(x=x_ticks, y=data[, column_index],
           type = "o", pch = point_shape,
           col = colours[column_index - 1])
  }
  
  # Add legend
  legend(x = legend_x, y = legend_y,
         legend = column_names[-month_column_index],
         col = colours,
         lty = 1, pch = point_shape,
         bty = "n")
  
  # Add zero line if requested
  if(add_zero_line){
    lines(x=x_ticks, y=rep(0, length(months)), lty = 2)
  }
}