#' @title Reporting Odds Ratio
#'
#' @description Calculates the Reporting Odds Ratio ("ROR") and confidence interval
#' interval, used in disproportionality analysis.
#'
#' @details The ROR is an odds ratio calculated from reporting counts. The
#' R for Reporting in ROR is meant to emphasize an interpretation of reporting,
#' as the ROR is calculated from a reporting database.
#'
#' @param a Number of reports for the specific drug and event (i.e. the
#' observed count)
#' @param b Number of reports with the drug, without the event
#' @param c Number of reports without the drug, with the event
#' @param d Number of reports without the drug, without the event
#' @param sign_lvl Significance level of confidence interval. Default is
#' 0.95 (i.e. 95 \% confidence interval)
#' @return A tibble with three columns (point estimate and confidence bounds)
#' with each row for each observed-to-expected pair provided.
#'
#' @examples
#'
#' pvutils::ror(a = 5, b = 10, c = 20, d = 10000)
#'
#' @export
#'
ror <- function(a, b, c, d, sign_lvl = 0.95){

# Run input checks
checkmate::qassert(c(a,b,c,d), "N+[0,)")
checkmate::qassert(sign_lvl, "N1[0,1]")

sign_lvl_quantile <-  (1 - (1 - sign_lvl)/2)

ror <- tibble::tibble("ror" = a*d/(b*c))

ror_w_ci <- function(a, b, c, d, sign_lvl_quantile) {
output <- exp(log(a*d/(b*c) +
                    c(-1,0,1) * qnorm(sign_lvl_quantile) *
                      sqrt(1/a + 1/b + 1/c + 1/d)))
return(output)
}

credibility_intervals <- purrr::pmap(a,b,c,d,ror_w_ci, get_quantiles)

credibility_intervals |>
  dplyr::bind_cols(ic) |>
  dplyr::select("lower bound", ic, "upper bound")

}

#' @title Information component
#'
#' @description Calculates the information component ("IC") and credibility
#' interval, used in disproportionality analysis.
#'
#' @details The IC is based on the relative reporting rate (RRR), but modified with
#' an addition of "shrinkage" (typically of 0.5) to protect against spurious
#' associations.
#'
#' \deqn{\hat{IC} = log_{2}(\frac{\hat{O}+0.5}{\hat{E}+0.5})}
#'
#' where \eqn{\hat{O}} = observed number of reports, and expected \eqn{\hat{E}}
#' is (for RRR, and using the entire database as \emph{background}) estimated as
#'
#' \deqn{ \hat{E} = \frac{N_{drug} \times N_{event}}{N_{TOT}}}
#'
#' where \eqn{N_{drug}}, \eqn{N_{event}} and \eqn{N_{TOT}} are the number of reports with the drug,
#' the event, and in the whole database respectively.
#'
#' From a bayesian perspective, the credibility interval of the IC is constructed
#' from the poisson-gamma conjugacy. The shrinkage is then a prior distribution of
#' observed and expected equal to 0.5. The point of \eqn{log_{2}} is to provide
#' a log-scale for convenient plotting of multiple IC values side-by-side.
#'
#' @param obs A numeric vector with observed counts, i.e. number of reports
#' for the selected drug-event-combination.
#' @param exp A numeric vector with expected counts, i.e. number of reports
#' to be expected given a comparator or \emph{background}.
#' @param shrinkage A non-negative numeric of length 1, to be added to
#' observed and expected count. Default is 0.5.
#' @param sign_lvl Significance level of credibility interval. Default is
#' 0.95 (i.e. 95 \% credibility interval)
#'
#' @return A tibble with a row for each observed-to-expected pair provided.
#'
#' @examples
#' obs <- 20
#' exp <- 10
#' ic(obs = 20, exp = 10)
#' @export

ic <- function(obs, exp, shrinkage = 0.5, sign_lvl = 0.95) {

  # Run input checks
  checkmate::qassert(c(obs, exp), "N+[0,)")
  checkmate::qassert(shrinkage, "N1[0,)")
  checkmate::qassert(sign_lvl, "N1[0,1]")

  ic <- tibble::tibble("ic" = log2((obs + shrinkage) / (exp + shrinkage)))

  # Note, stats::qgamma allows vectorized inputs for shape and rate
  get_quantiles <- function(quantile) {
    output <- log2(stats::qgamma(p = quantile,
                                 shape = obs + shrinkage,
                                 rate = exp + shrinkage))
    return(output)
  }

  # Therefore we produce quantiles for each bound (lower and upper)
  quantiles <- c((1 - sign_lvl) / 2, (1 + sign_lvl) / 2)
  names(quantiles) <- c("lower bound", "upper bound")
  credibility_intervals <- purrr::map_dfc(quantiles, get_quantiles)

  credibility_intervals |>
    dplyr::bind_cols(ic) |>
    dplyr::select("lower bound", ic, "upper bound")
}
