#### Preparation ####

# Set working directory to current script location
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Source functions to generate dummy data
source("dummy_data_functions.R")

# Note the data folder
data_folder <- file.path("..", "data")

# Note key parameters
start_date <- as.Date("01-01-2022", format = "%d-%m-%Y")
end_date <- as.Date("30-06-2022", format = "%d-%m-%Y")
transaction_types <- list(
  "Salary"=list("average_value"=1200, "frequency"="monthly", "day_of_month" = 5),
  "Food"=list("average_value"=-150, "frequency"="random", "n_per_month" = 4,
              "patterns" = c("supermarket", "market", "corner shop")),
  "Travel"=list("average_value"=-100, "frequency"="weekdays", "patterns" = c("train", "bus", "car")),
  "Bills"=list("average_value"=-150, "frequency"="monthly", "standard_deviation" = 0),
  "Subscriptions"=list("average_value"=-45, "frequency"="monthly", "standard_deviation" = 0,
                       "patterns" = c("films", "music")),
  "Rent"=list("average_value"=-300, "frequency"="monthly", "standard_deviation" = 0)
)

#### Write out transaction coding/types dictionary ####

ADD SOMETHING HERE!

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
  row.names = TRUE
)

#### 
