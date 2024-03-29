% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_processing_functions.R
\name{summarise_transactions}
\alias{summarise_transactions}
\title{Calculate total transactions in and out by grouping columns}
\usage{
summarise_transactions(
  transactions,
  in_column,
  out_column,
  grouping_columns,
  calculate_average = TRUE,
  calculate_difference = TRUE,
  ...
)
}
\arguments{
\item{transactions}{dataframe containing transaction details}

\item{in_column}{name of column in transactions dataframe containing money
going in (credit)}

\item{out_column}{name of column in transactions dataframe containing money
going out (debit)}

\item{grouping_columns}{vector of columns to group by}

\item{calculate_average}{boolean value to determine whether to add row at
bottom for averages. Defaults to TRUE}

\item{calculate_difference}{boolean value to determine whether to calculate
difference between in and out for each row.
Defaults to TRUE}

\item{...}{additional parameters to send to aggregate}
}
\value{
Returns of dataframe recording total money in and out by month. If
selected will include:
- difference column (in minus out) -what you save/lose each month
- Average row at bottom calculating average of in, out, and
difference across months
}
\description{
Summarise total money going in (credit) and out (debit) of account (based on
transactions) by selected grouping columns.
}
