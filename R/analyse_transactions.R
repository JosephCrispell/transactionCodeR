#### Preparation ####

# Set working directory to current script's folder
script_folder <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_folder) 

# Note data folder
data_folder <- file.path("..", "data")

# Note key parameters
date_column <- "Transaction Date"
date_format <- "%d/%m/%Y"
description_column <- "Transaction Description"
in_column <- "Credit Amount"
  out_column <- "Debit Amount"

# Note input files
transactions_file <- file.path(data_folder, "14279266_20221417_0806.csv")
coding_file <- file.path(data_folder, "transaction_coding_dictionary.csv")

# Load functions
source("functions.R")

#### Load data ####

# Load the transactions
transactions <- read.csv(
  transactions_file, 
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Load transaction coding dictionary
transaction_types <- read.csv(
  coding_file,
  stringsAsFactors = FALSE
)
transaction_coding <- build_transaction_coding_dictionary(transaction_types)

#### Process transactions ####

# Convert date column to dates
transactions[, date_column] <- as.Date(
  transactions[, date_column], 
  format = date_format
)

# Extract month from dates
transactions$month <- format(transactions[, date_column], "%m/%Y")

# Convert descriptions to lowercase
transactions[, description_column] <- tolower(transactions[, description_column])

# Classify transactions based on key words
transactions[, c("Type", "Pattern")] <-  
  classify_transactions(
    transactions[, description_column],
    transaction_coding
  )

#### Identify unclassified transactions ####

# Identify unclassified transactions
unclassified <- transactions[transactions$Type == "Unclassified", ]

# Identify standing orders/direct debits?
unique_descriptions <- table(unclassified[, description_column])
frequent_unclassified_descriptions <- unique_descriptions[unique_descriptions > 1]
frequent_unclassified_descriptions <- 
  frequent_unclassified_descriptions[order(frequent_unclassified_descriptions, decreasing = TRUE)]

#### Calculate monthly statistics ####

# Fix NA values - not taken into consideration when calculating average


# Calculate total pay and costs by month
totals_by_month <- summarise_transactions(
  transactions = transactions,
  in_column = in_column,
  out_column = out_column,
  grouping_columns = "month",
  FUN=sum
)

# Calculate costs by month and type
totals_by_type_and_month <- summarise_transactions(
  transactions = transactions,
  in_column = in_column,
  out_column = out_column,
  grouping_columns = c("month", "Type"),
  FUN=sum,
  calculate_average = FALSE, calculate_difference = FALSE
)

# Calculate average in/out on each type
totals_by_type_and_month[is.na(totals_by_type_and_month)] <- 0
average_by_type <- summarise_transactions(
  transactions = totals_by_type_and_month,
  in_column = in_column,
  out_column = out_column,
  grouping_columns = "Type",
  FUN=mean,
  calculate_average = FALSE
)

# Get car associated costs
car_types <- c("Car", "Petrol", "Car emergency")
car_average_monthly_costs <- average_by_type[average_by_type$Type %in% car_types, ]

#### Plots ####

# Plot in and out payments by type through time
plot_monthly_in_out_for_each_type(totals_by_type_and_month)

# Plot only car costs
plot_monthly_in_out_for_each_type(
  totals_by_type_and_month[totals_by_type_and_month$Type %in% car_types, ]
)

# Get
