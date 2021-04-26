# Code for expanded analysis of REBOA using TQIP Dataset

This repository contains the code to reproduce results from the following paper:

Increased mortality with REBOA only mitigated by strong unmeasured confounding: an expanded sensitivity analysis using the National Trauma Data Bank

Results can be reproduced by downloading the TQIP data into `data/` and running the following:

1. `preprocess.R`: Preprocess and define the covariates/outcomes
2. `propensity_time.R`: Propensity score analysis to obtain treatment effect. Sensitivity analysis to unmeasured confounders.
3. `propensity_with_missing.R`: Same as `propensity_time.R`, except it includes the missing variables (secondary analysis)
4. `sensitivity.R`: Toy example exploring differences between sensitivity analysis with absolute vs. relative risk. Results not included in the paper.
