#### Preparation ####

# Load required packages
library(rmarkdown) # building this report
library(DT) # interactive tables
library(plotly) # interactive graphics

# Set default code chunk options
knitr::opts_chunk$set(echo = FALSE)

# Check if current script being sourced for building report
if(exists("building_report") == FALSE){
  
  # Note data folder
  data_folder <- file.path("..", "data")
  
  # Note key parameters
  date_column <- "Transaction Date"
  date_format <- "%d/%m/%Y"
  description_column <- "Transaction Description"
  in_column <- "Credit Amount"
  out_column <- "Debit Amount"
  
  # Note input files
  transactions_file <- file.path(data_folder, "14279266_20225002_0707.csv")
  coding_file <- file.path(data_folder, "transaction_coding_dictionary.csv")
}

# Source functions
source("data_processing_functions.R")

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

# Get date range
transactions_dates <- range(transactions[, date_column])

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
unique_descriptions <- unique_descriptions[order(unique_descriptions, decreasing = TRUE)]

# Convert to dataframe
unique_descriptions <- as.data.frame(unique_descriptions)
colnames(unique_descriptions) <- c("Description", "Number of transactions")

#### Calculate monthly statistics ####

# Calculate total pay and costs by month
totals_by_month <- summarise_transactions(
  transactions = transactions,
  in_column = in_column,
  out_column = out_column,
  grouping_columns = "month",
  FUN=sum
)

# Make debit column negative
totals_by_month$`Debit Amount` <- -totals_by_month$`Debit Amount`

# Note average stats
n_rows <- nrow(totals_by_month)
average_in <- totals_by_month[n_rows, in_column]
average_out <- totals_by_month[n_rows, out_column]
average_savings <- totals_by_month[n_rows, "difference"]

#### Break it down by type ####

# Calculate costs by month and type
totals_by_type_and_month <- summarise_transactions(
  transactions = transactions,
  in_column = in_column,
  out_column = out_column,
  grouping_columns = c("month", "Type"),
  FUN=sum,
  calculate_average = FALSE
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
average_by_type <- average_by_type[order(average_by_type$difference), ]

# Get each types difference by month as column
# Taken from: https://stackoverflow.com/questions/67778341/are-there-alternative-r-base-functions-to-pivot-wider
totals_by_type_and_month$Type <- as.factor(totals_by_type_and_month$Type)
type_difference_by_month <- as.data.frame.matrix(
  xtabs(difference ~ month + Type, totals_by_type_and_month)
)
type_difference_by_month$month <- row.names(type_difference_by_month)
type_difference_by_month <- order_by_month(type_difference_by_month)
