#### Preparation ####

# Set working directory to current script location
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Note R library
r_library <- file.path("..", "..", "R")

# Source functions to generate dummy data
source(file.path(r_library, "dummy_data_functions.R"))

# Note the data folder
data_folder <- file.path("..", "..", "data")

# Note key parameters
start_date <- as.Date("01-01-2022", format = "%d-%m-%Y")
end_date <- as.Date("30-06-2022", format = "%d-%m-%Y")
transaction_types <- list(
  "Salary" = list(
    "average_value" = 1200, "type" = "in",
    "frequency" = "monthly", "day_of_month" = 5
  ),
  "Food" = list(
    "average_value" = 20, "type" = "out", "frequency" = "random",
    "n_per_month" = 4,
    "patterns" = c("supermarket", "market", "corner shop")
  ),
  "Travel" = list(
    "average_value" = 5, "type" = "out", "frequency" = "weekdays",
    "patterns" = c("train", "bus", "car")
  ),
  "Bills" = list(
    "average_value" = 150, "type" = "out",
    "frequency" = "monthly", "standard_deviation" = 0
  ),
  "Subscriptions" = list(
    "average_value" = 45, "type" = "out", "frequency" = "monthly",
    "standard_deviation" = 0, "patterns" = c("films", "music")
  ),
  "Rent" = list(
    "average_value" = 300, "type" = "out",
    "frequency" = "monthly", "standard_deviation" = 0
  )
)

#### Write out transaction coding/types dictionary ####

# TODO check transaction type info

# Write transaction types to file
write_transaction_types(
  file_path = file.path(
    data_folder,
    "dummy_transaction_coding_dictionary.csv"
  ),
  transaction_types = transaction_types
)

#### Generate transactions ####

# Generate dummy transactions for time period based on transaction type info
dummy_transactions <- generate_transactions(
  start_date = start_date,
  end_date = end_date,
  transaction_types = transaction_types
)

# Save in data folder
write.csv(
  dummy_transactions,
  file = file.path(data_folder, "dummy_transactions.csv"),
  row.names = FALSE
)
