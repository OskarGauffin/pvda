# pvda

An R package for executing disproportionality analyses with information component (IC) proportional reporting rate (PRR) and reporting odds ratio (ROR). 

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/OskarGauffin/pvutils/branch/main/graph/badge.svg)](https://app.codecov.io/gh/OskarGauffin/pvutils?branch=main)

[![R-CMD-check](https://github.com/OskarGauffin/pvutils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/OskarGauffin/pvutils/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# Installation

``` r
# Install from GitHub:
devtools::install_github("OskarGauffin/pvda")

# Not yet on CRAN, but once there:
# install.packages("pvda")
```

# Example code

``` r
library("pvda")

da1 <- 
drug_event_df |> 
pvda::da()

summary(da1)
```





