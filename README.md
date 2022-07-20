[![R build status](https://github.com/JosephCrispell/transactionCodeR/workflows/R/badge.svg)](https://github.com/JosephCrispell/transactionCodeR/actions?workflow=R)
[![GitHub stars](https://img.shields.io/github/stars/JosephCrispell/transactionCodeR?style=social)](https://github.com/JosephCrispell/transactionCodeR)


# transaction-coder
Code to classify bank account transactions based on key words

# Precommit installation
The current repo uses a precommit continuous integration workflow. A precommit workflow triggers a set of task each time you commit any changed files. Here, the tasks mainly help with maintaining a standard coding style and spotting any minor mistakes in the code.

To install the workflow run the following: ([more info here](https://pre-commit.com/)):

- Install python precommit library with in the command line: `pip install pre-commit`
- Install the precommit hooks (tasks) by:
    - Cloning the repository
    - Navigating to the respository in the command line and run: `pre-commit install`
