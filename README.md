[![R build status](https://github.com/JosephCrispell/transactionCodeR/workflows/R/badge.svg)](https://github.com/JosephCrispell/transactionCodeR/actions?workflow=R)
[![GitHub stars](https://img.shields.io/github/stars/JosephCrispell/transactionCodeR?style=social)](https://github.com/JosephCrispell/transactionCodeR/)

# transaction-coder

## Summary
An early R package with functions to explore monthly bank transactions data breaking them down by the type of transaction. The type of transaction is defined based upon key words (or patterns) observed in each transaction's description.

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

Note these are very simplistic. The key things to note:
- You can multiple key words (or patterns) matching to single transaction type
- You can have as many transaction types as you want
- The column names are required to be `Type` and `Patterns`

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
- Bank transactions data in `CSV` format (see [Bank transaction data section]() above)
- Key words (or patterns) defining transaction types (see [Defining transaction types section]() above)

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

### Precommit installation (*for development*)
The current repo uses a precommit continuous integration workflow. A precommit workflow triggers a set of task each time you commit any changed files. Here, the tasks mainly help with maintaining a standard coding style and spotting any minor mistakes in the code.

To install the workflow run the following: ([more info here](https://pre-commit.com/)):

- Install python precommit library with in the command line: `pip install pre-commit`
- Install the precommit hooks (tasks) by:
    - Cloning the repository
    - Navigating to the respository in the command line and run: `pre-commit install`
> Note you can interact with pre-commit from R directly using the [pre-commit](https://www.rdocumentation.org/packages/precommit/versions/0.2.2) package
