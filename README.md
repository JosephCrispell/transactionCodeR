[![R-CMD-check](https://github.com/JosephCrispell/transactionCodeR/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/JosephCrispell/transactionCodeR/actions/workflows/R-CMD-check.yml)
[![GitHub stars](https://img.shields.io/github/stars/JosephCrispell/transactionCodeR?style=social)](https://github.com/JosephCrispell/transactionCodeR/)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# transactionCodeR

## Summary
An early R package with functions to explore monthly bank transactions data breaking them down by the type of transaction. The type of transaction is defined based upon key words (or patterns) observed in each transaction's description. See an [example report](https://josephcrispell.github.io/standalone/transaction_coding_report.html) you can generate to give you an idea of what you can generate.

## Defining transaction types

Key words or patterns are used to define transaction types and can be defined in a simple `CSV` (Comma Seperated Values) file. For example, here is `data/dummy_transaction_coding_dictionary.csv` that is used to classify transactions in the dummy data:
|Type         |Patterns                      |
|-------------|------------------------------|
|Salary       |Salary                        |
|Food         |supermarket;market;corner shop|
|Travel       |train;bus;car                 |
|Bills        |Bills                         |
|Subscriptions|films;music                   |
|Rent         |Rent                          |
|Surfboard    |Surfboard                     |
|Exclude      |loan                          |

Note these are very simplistic. The key things to note:
- You can multiple key words (or patterns) matching to single transaction type
- You can have as many transaction types as you want
- The column names are required to be `Type` and `Patterns`
- The transaction type `"Exclude"` can be used to exclude transactions matching the patterns from summary statistics generated

## Bank transactions data

Bank transactions data should be in a standard format through your provider's online system. The following four columns are used by this package:

- `Transaction Date` - column containing transaction date
- `Transaction Description` - payment description column
- `Credit Amount` - amount of money paid in
- `Debit Amount` - amount of money paid out

Note that the names of the above can be specified. For example in the report Rmarkdown script here: https://github.com/JosephCrispell/transactionCodeR/blob/98d87143cf1f9599fb815d145a71b2785f6b6564/R/transaction_coding_report.Rmd#L26-L31

## Installation

### Requirements

`transactionCodeR` is an R package with the following requirements:

**data**
- Bank transactions data in `CSV` format (see [Bank transaction data](https://github.com/JosephCrispell/transactionCodeR#bank-transactions-data) section above)
- Key words (or patterns) defining transaction types (see [Defining transaction types](https://github.com/JosephCrispell/transactionCodeR#defining-transaction-types) section above)

**software**
- [R](https://www.r-project.org/)
- [RStudio](https://www.rstudio.com/) - you can get by without this!

**R packages**
- [plotly](https://plotly.com/r/) - for interactive visualisations
- [devtools](https://devtools.r-lib.org/) - for installing R package from GitHub

### R package installation

```
devtools::install_github("JosephCrispell/transactionCodeR")
library(basicPlotteR)
```

## Example scripts

There are three scripts provided with this R package. To locate these scripts once the R package is installed use the following code:
```r
transaction_report_script_path <- system.file(
    "inst", "R", "transaction_coding_report.Rmd",
    package = "transactionCodeR"
)
process_transactions_script_path <- system.file(
    "inst", "R", "process_transactions.R",
    package = "transactionCodeR"
)
dummy_data_script_path <- system.file(
    "inst", "R", "generate_dummy_data.R",
    package = "transactionCodeR"
)
```

These two scripts do the following (more detail on these in later sections):
- [`inst/R/transaction_coding_report.Rmd`](https://github.com/JosephCrispell/transactionCodeR/blob/main/inst/R/transaction_coding_report.Rmd) - building a report summarising transactions by month and type
- [`inst/R/process_transactions.R`](https://github.com/JosephCrispell/transactionCodeR/blob/main/inst/R/process_transactions.R) - code to process transactions and summarise by type and month
- [`inst/R/generate_dummy_data.R`](https://github.com/JosephCrispell/transactionCodeR/blob/main/inst/R/generate_dummy_data.R) - generating dummy transaction data

## Building your report


The [`inst/R/transaction_coding_report.Rmd`](https://github.com/JosephCrispell/transactionCodeR/blob/main/inst/R/transaction_coding_report.Rmd) Rmarkdown script represents a template report you can use to analyse your monthly bank transactions by their type.

The [`inst/R/transaction_coding_report.Rmd`](https://github.com/JosephCrispell/transactionCodeR/blob/main/inst/R/transaction_coding_report.Rmd) is designed to run on dummy transaction data but can easily be modified to run on your own data:
  - Edit the input parameters when knitting the Rmarkdown file providing the file names for your bank transactions data and transactions types files ([more info on knitting with parameters](https://bookdown.org/yihui/rmarkdown/params-knit.html))
  - Update the following lines with the correct column names and date format: https://github.com/JosephCrispell/transactionCodeR/blob/98d87143cf1f9599fb815d145a71b2785f6b6564/R/transaction_coding_report.Rmd#L26-L31

The report will automatically call the [`inst/R/process_transactions.R`](https://github.com/JosephCrispell/transactionCodeR/blob/main/inst/R/process_transactions.R) script to process the transactions data provided based upon the parameters set above.

[Here is an example report](https://josephcrispell.github.io/standalone/transaction_coding_report.html) that is generated based on the dummy data provided with this R package.

## Generating dummy data

For ease of use, some [dummy bank transactions data](https://github.com/JosephCrispell/transactionCodeR/blob/main/data/dummy_transactions.csv) were generated along with a [transaction type file](dummy_transaction_coding_dictionary.csv). These were generated (and can be readily recreated using the [`inst/R/generate_dummy_data.R`](https://github.com/JosephCrispell/transactionCodeR/blob/main/inst/R/generate_dummy_data.R) script) to provide examples of the input files.

While you are getting comfortable with this R package you can use these dummy data files as input, for example (as noted above) the example Rmarkdown report will by default point to these data files.

As noted above, you can recreate the dummy data using the [`inst/R/generate_dummy_data.R`](https://github.com/JosephCrispell/transactionCodeR/blob/main/inst/R/generate_dummy_data.R) script. Within this script you can edit the characteristics of the dummy data by editing the `transaction_types` list here: https://github.com/JosephCrispell/transactionCodeR/blob/cd35fa00891bc9c3903a8b7e11c57250e5ee332c/inst/R/generate_dummy_data.R#L18-L44

For each type of dummay data you create you can use the following parameters to create it's values specified within a list structure:
- `"average_value"`: average value (mean)
- `"standard_deviation"`: standard deviation from average value for transaction. Defaults to 10% of value.
- `"type"`: type of transaction ("in" (credit), or "out" (debit))
- `"frequency"`: frequency that transaction type seen in transactions. Expecting one of c("monthly", "weekly", "daily", "weekdays", "random", "once")
- `"day_of_month"`: if monthly, which day of month. Defaults to 1 (first day).
- `"n_per_month"`: if random frequency, on average how many transactions per month. Defaults to 4.
- `"day_of_week"`: if weekly, which day of week. Defaults to 1 (first day).
- `"patterns"`: patterns to use as transaction descriptions. Defaults to  name of transaction type.

## Precommit installation (*for development*)
The current repo uses a precommit continuous integration workflow. A precommit workflow triggers a set of task each time you commit any changed files. Here, the tasks mainly help with maintaining a standard coding style and spotting any minor mistakes in the code.

To install the workflow run the following: ([more info here](https://pre-commit.com/)):

- Install python precommit library with in the command line: `pip install pre-commit`
- Install the precommit hooks (tasks) by:
    - Cloning the repository
    - Navigating to the respository in the command line and run: `pre-commit install`
> Note you can interact with pre-commit from R directly using the [pre-commit](https://www.rdocumentation.org/packages/precommit/versions/0.2.2) package
