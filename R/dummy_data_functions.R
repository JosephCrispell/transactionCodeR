#' Generate random transactions based on transaction types
#'
#' Given a list providing information about different transaction types
#' this function will generate random transactions for each type across
#' a set time period
#' @param start_date Date object for start date
#' @param end_date Date object for end date
#' @param transaction_types a list with details for each transaction type expected
#' @return a dataframe of random transactions matching details of each type of transaction
generate_transactions <- function(start_date, end_date, transaction_types){
  
  # Build a dataframe to store transactions
  full_transactions <- NULL
  
  # Examine each transaction type
  for(transaction_type in names(transaction_types)){
    
    # Generate transactions
    current_transactions <- generate_transactions_for_type(
      average_value = transaction_types[[transaction_type]]$average_value,
      type = transaction_type,
      frequency = transaction_types[[transaction_type]]$frequency,
      start_date = start_date, 
      end_date = end_date,
      random_n_per_month = transaction_types[[transaction_type]]$n_per_month,
      standard_deviation = transaction_types[[transaction_type]]$standard_deviation,
      day_of_month = transaction_types[[transaction_type]]$day_of_month,
      day_of_week = transaction_types[[transaction_type]]$day_of_week,
      patterns = transaction_types[[transaction_type]]$patterns
    )
    
    # Combine with growing table
    if(is.null(full_transactions)){
      full_transactions <- current_transactions
    }else{
      full_transactions <- rbind(full_transactions, current_transactions)
    }
  }
  
  # Order by date
  full_transactions$Date <- as.Date(full_transactions$Date)
  full_transactions <- full_transactions[order(full_transactions$Date), ]
  
  # Reset rownames
  rownames(full_transactions) <- seq_len(nrow(full_transactions))
  
  return(full_transactions)
}

#' Generate random transactions of a particular type
#'
#' Given information about transaction type, this function will generate random
#' transactions across time period for that type
#' @param average_value average value for type
#' @param type name of transaction type
#' @param frequency frequency that transaction type seen in transactions. Expecting one of c("monthly", "weekly", "daily", "weekdays", "random")
#' @param start_date Date object for start date
#' @param end_date Date object for end date
#' @param random_n_per_month if random frequency, on average how many transactions per month. Defaults to 4.
#' @param standard_deviation standard deviation from average value for transaction. Defaults to 10% of value.
#' @param day_of_month if monthly, which day of month. Defaults to 1 (first day).
#' @param day_of_week if weekly, which day of week. Defaults to 1 (first day).
#' @param patterns patterns to use as transaction descriptions. Defaults to type parameter.
#' @return a dataframe of random transactions matching details of transaction type
generate_transactions_for_type <- function(
  average_value, type, frequency,
  start_date, end_date,
  random_n_per_month = NULL,
  standard_deviation = NULL,
  day_of_month = NULL,
  day_of_week = NULL,
  patterns = NULL
){
  
  # Set default values for optional variables
  random_n_per_month <- ifelse(is.null(random_n_per_month), 4, random_n_per_month)
  standard_deviation <- ifelse(is.null(standard_deviation), abs(0.1*average_value), standard_deviation)
  day_of_month <- ifelse(is.null(day_of_month), 1, day_of_month)
  day_of_week <- ifelse(is.null(day_of_week), 1, day_of_week)
  patterns <- ifelse(is.null(patterns), type, patterns)
  
  # Generate transaction dates
  dates <- generate_transaction_dates(
    start_date, end_date, frequency, random_n_per_month, day_of_month, day_of_week
  )
  
  # Generate the transaction values
  values <- rnorm(length(dates), mean = average_value, sd = standard_deviation)
  
  # Store in dataframe
  transactions <- data.frame(
    "Date" = dates,
    "Description" = sample(patterns, size = length(dates), replace = TRUE),
    "In" = NA,
    "Out" = NA
  )
  if(average_value > 0){
    transactions$In <- values
  }else{
    transactions$Out <- values
  }
  
  return(transactions)
}

#' Generate random dates for transactions
#'
#' Given start and end date for period and frequency of transaction this function
#' provides dates for the transactions.
#' @param start_date Date object for start date
#' @param end_date Date object for end date
#' @param frequency frequency that transaction type seen in transactions. Expecting one of c("monthly", "weekly", "daily", "weekdays", "random")
#' @param random_n_per_month if random frequency, on average how many transactions per month. Defaults to 4.
#' @param day_of_month if monthly, which day of month. Defaults to 1 (first day).
#' @param day_of_week if weekly, which day of week. Defaults to 1 (first day).
#' @return vector of dates for transactions
generate_transaction_dates <- function(
  start_date, end_date, frequency, 
  random_n_per_month = 4,
  day_of_month = 1,
  day_of_week = 1
){
  
  # Check frequency has expected value
  expected_frequency_types <- c("monthly", "weekly", "daily", "weekdays", "random")
  if(frequency %in% expected_frequency_types == FALSE){
    stop(
      paste0(
        "Frequency type (", frequency, ") provided not recognised! Must be one of:\n\t",
        paste(expected_frequency_types, collapse = ", ")
      )
    )
  }
  
  # Generate each date series
  date_series <- list(
    "monthly"=seq(start_date, end_date, by = "month"),
    "weekly"=seq(start_date, end_date, by = "week"),
    "daily"=seq(start_date, end_date, by = "day")
  )
  date_series[["weekdays"]] <- 
    date_series[["daily"]][weekdays(date_series[["daily"]]) %in% c("Saturday", "Sunday") == FALSE]
  date_series[["random"]] <- sample(
    date_series[["daily"]],
    size = length(date_series[["monthly"]]) * random_n_per_month
  )
  
  # Change monthly/weekly series based on day of month/week
  date_series[["monthly"]] <- date_series[["monthly"]] + (day_of_month - 1)
  date_series[["weekly"]] <- date_series[["weekly"]] + (day_of_week - 1)
  
  # Return selected date series
  return(date_series[[frequency]])
}