#### Preparation ####

# Set working directory to current script's folder
script_folder <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script_folder)

# Load functions
source("functions.R")

# Note key parameters
date_column <- "Transaction Date"
date_format <- "%d/%m/%Y"
description_column <- "Transaction Description"

# Load the transactions
transactions <- read.csv(
  file.path("data", "14279266_20221417_0806.csv"), 
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Load transaction coding dictionary
transaction_types <- read.csv(
  "transaction_coding_dictionary.csv",
  stringsAsFactors = FALSE
)
transaction_coding <- build_transaction_coding_dictionary(transaction_types)

#### Process transactions ####

# Convert date column to dates
transactions[, date_column] <- as.Date(transactions[, date_column])
