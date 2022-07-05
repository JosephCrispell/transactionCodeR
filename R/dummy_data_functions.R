generate_transactions <- function(start_date, end_date, transaction_types){
  
  # Build a dataframe to store transactions
  full_transactions <- NULL
  
  # Examine each transaction type
  for(transaction_type in names(transaction_types)){
    
    # Generate transactions
    current_transactions <- generate_transactions_for_type(
      value = transaction_types[[transaction_type]]$average_value,
      type = transaction_type,
      frequency = transaction_types[[transaction_type]]$frequency,
      start_date = start_date, 
      end_date = end_date,
      random_n_per_month = transaction_types[[transaction_type]]$n_per_month,
      standard_deviation = transaction_types[[transaction_type]]$standard_deviation,
      day_of_month = transaction_types[[transaction_type]]$day_of_month,
      day_of_week = transaction_types[[transaction_type]]$day_of_week
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

generate_transactions_for_type <- function(
  value, type, frequency,
  start_date, end_date,
  random_n_per_month = NULL,
  standard_deviation = NULL,
  day_of_month = NULL,
  day_of_week = NULL
){
  
  # Set default values for couple of variables
  random_n_per_month <- ifelse(is.null(random_n_per_month), 4, random_n_per_month)
  standard_deviation <- ifelse(is.null(standard_deviation), abs(0.1*value), standard_deviation)
  day_of_month <- ifelse(is.null(day_of_month), 1, day_of_month)
  day_of_week <- ifelse(is.null(day_of_week), 1, day_of_week)
  
  # Generate transaction dates
  dates <- generate_transaction_dates(
    start_date, end_date, frequency, random_n_per_month, day_of_month, day_of_week
  )
  
  # Generate the transaction values
  values <- rnorm(length(dates), mean = value, sd = standard_deviation)
  
  # Store in dataframe
  transactions <- data.frame(
    "Date" = dates,
    "In" = ifelse(value > 0, values, NA),
    "Out" = ifelse(value < 0, values, NA),
    "Description" = type
  )
  
  return(transactions)
}

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
    size = length(monthly) * random_n_per_month
  )
  
  # Change monthly/weekly series based on day of month/week
  date_series[["monthly"]] <- date_series[["monthly"]] + (day_of_month - 1)
  date_series[["weekly"]] <- date_series[["weekly"]] + (day_of_week - 1)
  
  # Return selected date series
  return(date_series[[frequency]])
}