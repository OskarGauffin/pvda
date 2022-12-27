#' Information component
#'
#' Calculates the information component ("IC") used in disproportionality analysis.
#'
#' @param obs A numeric vector with observed counts, i.e. number of reports for the selected drug-event-combination.
#' @param exp A numeric vector with expected counts, i.e. number of reports to be expected given a comparator or \emph{background}.
#' @shrinkage A non-negative numeric of length 1, to be added to observed and expected count. Default is 0.5.
#' @param sign_lvl Significance level of credibility interval. Default is 0.95 (i.e. 95 //% credibility interval)
#'
#' @return A tibble with a row for each observed-to-expected pair provided.
#'
#' @examples
# obs <- 20
# exp <- 10
#' ic(obs = 20, exp = 10)
#' @export

ic <- function(obs, exp, shrinkage = 0.5, sign_lvl = 0.95) {
  # Run input checks
  checkmate::qassert(obs, "N+[0,)")
  checkmate::qassert(exp, "N+[0,)")
  checkmate::qassert(shrinkage, "N1[0,)")
  checkmate::qassert(sign_lvl, "N1[0,1]")

  ic <- tibble::tibble("ic" = log2((obs + shrinkage) / (exp + shrinkage)))

  # Calculate credibility interval
  # qgamma allows vectorized inputs for shape and rate
  get_quantiles <- function(quantile) {
    output <- log2(qgamma(p = quantile, shape = obs + shrinkage, rate = exp + shrinkage))
    return(output)
  }

  # Hence, we calculate IC bounds in two calls (lower and upper)
  quantiles <- c((1 - sign_lvl) / 2, (1 + sign_lvl) / 2)
  names(quantiles) <- c("lower bound", "upper bound")
  credibility_intervals <- purrr::map_dfc(quantiles, get_quantiles)

  credibility_intervals |>
    dplyr::bind_cols(ic) |>
    dplyr::select("lower bound", ic, "upper bound")
}
