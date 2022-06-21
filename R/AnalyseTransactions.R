#### Preparation ####

# Set working directory to current script's folder
script_folder <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_folder)

# Note data folder
data_folder <- file.path("..", "data")

# Load functions
source("functions.R")

# Note key parameters
date_column <- "Transaction Date"
date_format <- "%d/%m/%Y"
description_column <- "Transaction Description"
in_column <- "Credit Amount"
out_column <- "Debit Amount"

# Load the transactions
transactions <- read.csv(
  file.path(data_folder, "14279266_20221417_0806.csv"), 
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Load transaction coding dictionary
transaction_types <- read.csv(
  file.path(data_folder, "transaction_coding_dictionary.csv"),
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
unclassified <- transactions[is.na(transactions$Type), ]

# Identify standing orders/direct debits?
unique_descriptions <- table(unclassified[, description_column])
frequent_unclassified_descriptions <- unique_descriptions[unique_descriptions > 1]

#### Calculate monthly pay and costs ####

# Calculate total pay and costs by month
totals_by_month <- summarise_monthly_totals(
  transactions = transactions,
  in_column = in_column,
  out_column = out_column
)

# Calculate costs by month and type
totals_by_type_and_month <- aggregate(
  transactions[]
)