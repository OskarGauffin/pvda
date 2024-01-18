# pvda <a href="https://oskargauffin.github.io/pvda/"><img src="man/figures/logo.png" align="right" height="120" alt="pvda website" /></a>

An R package for executing disproportionality analyses in pharmacovigilance, using the information component (IC), proportional reporting rate (PRR) and reporting odds ratio (ROR). 

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/pvda)](https://CRAN.R-project.org/package=pvda)
[![R-CMD-check](https://github.com/OskarGauffin/pvda/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/OskarGauffin/pvda/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/OskarGauffin/pvda/branch/main/graph/badge.svg)](https://app.codecov.io/gh/OskarGauffin/pvda?branch=main)
<!-- badges: end -->

# Installation

``` r
# Install stable version from CRAN 
install.packages("pvda")
```

# Example code
To run a disproportionality analysis, pass the adverse event report-level data (here, drug_event_df) to function *da* as:

``` r
library("pvda")

da1 <- 
drug_event_df |> 
da()

summary(da1)
```

The output object contains summary counts, disproportionality point and interval estimates. To extract the results in a data frame, access "da_df" as a list object:

``` r

da_results <- 
da1$da_df
```




