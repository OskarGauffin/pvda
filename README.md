# pvda <a href="https://oskargauffin.github.io/pvda/"><img src="man/figures/logo.png" align="right" height="120" alt="pvda website" /></a>

An R package for executing disproportionality analyses in pharmacovigilance, using the information component (IC), proportional reporting rate (PRR) and reporting odds ratio (ROR). 

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/OskarGauffin/pvda/branch/main/graph/badge.svg)](https://app.codecov.io/gh/OskarGauffin/pvda?branch=main)

[![R-CMD-check](https://github.com/OskarGauffin/pvda/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/OskarGauffin/pvda/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# Installation

``` r
# Install development version from GitHub using devtools:
devtools::install_github("OskarGauffin/pvda")

# Not yet on CRAN, but once there:
# install.packages("pvda")
```

# Example code
To run a disproportionality analysis, pass the adverse event report-level data to function *da* as:

``` r
library("pvda")

da1 <- 
drug_event_df |> 
da()

summary(da1)
```

The output object contains summary counts, as well as disproportionality point and interval estimates, and can be accessed as a regular list object:

``` r

da_results <- 
da1$da_df
```




