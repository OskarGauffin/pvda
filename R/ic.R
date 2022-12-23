theme(panel.background = element_rect(fill = "#253962", color = "#69B4A7"),
      panel.grid.major = element_line(color = 'grey50', size=0.2, linetype = 'dotted'),
      panel.grid.minor = element_line(color = 'grey50', size = 0.2)))



#' Information component
#'
#' Calculates the information component ("IC") used in disproportionality analysis.
#'
#' @param obs A numeric vector with observed counts, i.e. number of reports for the selected drug-event-combination.
#' @param exp A numeric vector with expected counts, i.e. number of reports to be expected given a comparator or \emph{background}.
#' @param sign_lvl Significance level of credibility interval, defaults to 0.95 (i.e. 95 //% credibility interval)
#'
#' @return A tibble with a row for each observed-to-expected pair provided.
#'
#' @examples
#' obs = 20
#' exp = 10
#' ic(obs=20, exp=10)
#' @export

ic <- function(obs, exp, sign_lvl = 0.95){

  # Run input checks
  checkmate::qassert(obs, "R+[0,)")
  checkmate::qassert(exp, "R+[0,)")
  checkmate::qassert(sign_lvl, "R1+[0,1]")

  # qgamma allows vectorized inputs for shape and rate, as long as the
  # p-argument is of length one.
  get_quantiles <- function(quantile){
    output <- qgamma(p=quantile, shape = obs + 0.5, rate = exp + 0.5)
    return(output)
  }

  # Hence, we calculate IC in three calls (lower, mid and upper)
  quantiles <- c((1 - sign_lvl)/2, 0.5, (1 + sign_lvl)/2)
  names(quantiles) = c("lower bound", "IC", "upper bound")

  purrr::map_dfc(quantiles, get_quantiles)

}
