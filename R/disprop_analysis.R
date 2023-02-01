#' @title Calculate counts required for expected counts, and expected counts
#' @description Produces various counts used in disproportionality analysis.
#' @param df A data table, or an object possible to convert to a data table, e.g.
#' a tibble or data.frame. For column specifications, see details.
#' @param da_estimators A character vector containing the desired expected counts.
#' Defaults to all possible options, i.e. c("rrr", "prr", "ror").
#'
#' @details
#' The passed data table should contain three columns: "report_id", "drug_name"
#' and "event_name". It should have one row per reported drug-event-combination,
#' i.e. receiving an additional report for drug A and event 1 would add one row
#' to the table. The same report_id can occur on several rows, if the same
#' report contains several drugs and/or events. Column report_id must be of type
#' numeric or character. Columns drug_name and event_name must be
#' characters.
#' @return A tibble with counts.
#' @importFrom dplyr arrange count distinct everything group_by mutate n_distinct
#' rename select ungroup
#' @import data.table
#' @export

add_expected_counts <- function(df,
                                da_estimators = c("rrr", "prr", "ror")) {
  # data.table complains if you haven't defined these variables as NULLs
  NULL -> desc -> ends_with -> exp_ror -> d -> b -> exp_prr -> n_tot_prr ->
  n_event_prr -> exp_rrr -> obs -> n -> n_event -> n_drug -> n_tot ->
  event -> drug -> report_id

  checkmate::qassert(df[[1]], c("S+", "N+"))
  checkmate::qassertr(df[2:3], "S+")

  if (!any(utils::hasName(df, c("report_id", "drug", "event")))) {
    stop("At least one of column names 'report_id', 'drug' and 'event' is not
         found. Please check the passed object.")
  }

  checkmate::qassert(da_estimators, "S+")
  if (any(!da_estimators %in% c("rrr", "prr", "ror"))) {
    stop("Only 'rrr', 'prr' and 'ror' are allowed in parameter 'da_estimators'")
  }


  if (!typeof(df) == "data.table") {
    df <- data.table::as.data.table(df)
  }

  #  Begin with the RRR counts, as they're the computationally most feasible,
  #  and useful for calculating PRR and ROR.
  count_dt <- dtplyr::lazy_dt(df, immutable = FALSE) |>
    distinct() |>
    mutate(n_tot = n_distinct(report_id)) |>
    group_by(drug) |>
    mutate(n_drug = n_distinct(report_id)) |>
    ungroup() |>
    group_by(event) |>
    mutate(n_event = n_distinct(report_id)) |>
    ungroup() |>
    count(drug, event, n_tot, n_drug, n_event) |>
    rename(obs = n) |>
    mutate(exp_rrr = n_drug * n_event / n_tot) |>
    select(drug, event, obs, n_drug, n_event, n_tot, exp_rrr)

  # Calc PRR counts if requested
  if (any(c("ror", "prr") %in% da_estimators)) {
    count_dt <- count_dt |>
      mutate(
        n_event_prr = n_event - obs,
        n_tot_prr = n_tot - n_drug
      ) |>
      mutate(exp_prr = n_drug * n_event_prr / n_tot_prr) |>
      select(everything(), n_event_prr, n_tot_prr, exp_prr)
  }

  # Calc ROR counts if requested. Count "a" equal obs.
  if ("ror" %in% da_estimators) {
    count_dt <- count_dt |>
      mutate(
        b = n_drug - obs,
        c = n_event_prr,
        d = n_tot_prr - n_event + obs
      ) |>
      mutate(exp_ror = b * c / d) |>
      select(everything(), b, c, d, exp_ror)
  }

  if (!"rrr" %in% da_estimators) {
    count_dt <- count_dt |> select(-ends_with("rrr"))
  }

  count_df <- count_dt |>
    arrange(desc(obs)) |>
    tibble::as_tibble()

  return(count_df)
}

#' @title Confidence intervals for Reporting Odds Ratio
#' @description Mainly for use in \code{\link{ror}}. Produces (symmetric,
#' normality based) confidence bounds for the ROR, for a passed probability.
#' Called twice in \code{ror} to create confidence intervals.
#' @param sign_lvl_probs The probabilities of the normal distribution, based on
#' a passed significance level (\code{sign_lvl}) in \code{\link{ror}}. If
#' \code{sgn_lvl = .95} in \code{ror}, quantiles of the normal distribution will
#' be extracted at \code{sgn_lvl_probs} of 0.025 and 0.975.
#' @seealso \code{\link{ror}}
#' @inheritParams ror
#' @export
ci_for_ror <- function(a, b, c, d, sign_lvl_probs) {
  exp(log((a * d) / (b * c)) + stats::qnorm(sign_lvl_probs) *
    sqrt(1 / a + 1 / b + 1 / c + 1 / d))
}

#' @title Confidence intervals for Information Component (IC)
#' @description Mainly used in \code{link{ic}}. Produces quantiles of the
#' posterior gamma distribution. Called twice in \code{ic} to create a
#' credibility interval.
#' @param sign_lvl_probs The probabilities of the posterior, based on
#' a passed significance level (\code{sign_lvl}) in \code{\link{ic}}. For
#' instance, if \code{sgn_lvl = .95} in \code{ic} is used, quantiles will be
#' extracted at \code{sgn_lvl_probs} 0.025 and 0.975.
#' @seealso \code{\link{ic}}
#' @inheritParams ic
#' @export
ci_for_ic <- function(obs, exp, sign_lvl_probs, shrinkage) {
  output <- log2(stats::qgamma(
    p = sign_lvl_probs,
    shape = obs + shrinkage,
    rate = exp + shrinkage
  ))
  return(output)
}

#' @title Reporting Odds Ratio
#'
#' @description Calculates Reporting Odds Ratio ("ROR") and confidence
#' intervals, used in disproportionality analysis.
#'
#' @details The ROR is an odds ratio calculated from reporting counts. The
#' R for Reporting in ROR is meant to emphasize an interpretation of reporting,
#' as the ROR is calculated from a reporting database. Note: the function is
#' vectorized, i.e. a, b, c and d can be vectors, see the examples.
#'
#' @param a Number of reports for the specific drug and event (i.e. the
#' observed count).
#' @param b Number of reports with the drug, without the event
#' @param c Number of reports without the drug, with the event
#' @param d Number of reports without the drug, without the event
#' @param sign_lvl Significance level of confidence interval. Default is
#' 0.95 (i.e. 95 \% confidence interval)
#' @return A tibble with three columns (point estimate and credibility bounds).
#' Number of rows equals length of inputs a, b, c, d.
#'
#' @examples
#'
#' pvutils::ror(a = 5, b = 10, c = 20, d = 10000)
#'
#' # Note that a, b, c and d can be vectors (of equal length, no recycling)
#' pvutils::ror(a = c(5, 10), b = c(10, 20), c = c(15, 30), d = c(10000, 10000))
#' @export
#'
ror <- function(a, b, c, d, sign_lvl = 0.95) {
  checkmate::qassert(c(a, b, c, d), "N+[0,)")
  checkmate::qassert(sign_lvl, "N1[0,1]")

  # Check that all vectors have the same length, seemed
  # hard to do in checkmate.
  if (!all(purrr::map(list(b, c, d), \(x){
    length(x)
  }) == length(a))) {
    stop("Vectors a, b, c and d are not of equal length.")
  }

  lower_prob <- (1 - sign_lvl) / 2
  upper_prob <- 1 - lower_prob

  output <- tibble::tibble(
    "ror_lower" = ci_for_ror(a, b, c, d, lower_prob),
    "ror" = a * d / (b * c),
    "ror_upper" = ci_for_ror(a, b, c, d, upper_prob)
  )

  return(output)
}

#' @title Information component
#'
#' @description Calculates the information component ("IC") and credibility
#' interval, used in disproportionality analysis.
#'
#' @details  The IC is based on the relative reporting rate (RRR), but modified with
#' an addition of "shrinkage" (typically of 0.5) to protect against spurious
#' associations.
#'
#' \deqn{\hat{IC} = log_{2}(\frac{\hat{O}+0.5}{\hat{E}+0.5})}
#'
#' where \eqn{\hat{O}} = observed number of reports, and expected \eqn{\hat{E}}
#' is (for RRR, and using the entire database as \emph{background}) estimated as
#'
#' \deqn{ \hat{E} = \frac{N_{drug} \times N_{event}}{N_{TOT}}}
#' µ
#' where \eqn{N_{drug}}, \eqn{N_{event}} and \eqn{N_{TOT}} are the number of repµorts with the drug,
#' the event, and in the whole database respectively.
#'
#' From a bayesian perspective, the credibility interval of the IC is constructed
#' from the poisson-gamma conjugacy. The shrinkage is then a prior distribution of
#' observed and expected equal to 0.5. The point of \eqn{log_{2}} is to provide
#' a log-scale for convenient plotting of multiple IC values side-by-side.
#'
#' Note: the function is vectorized, i.e. obs and exp can be vectors, see the examples.
#'
#' @param obs A numeric vector with observed counts, i.e. number of reports
#' for the selected drug-event-combination. Note that shrinkage (e.g. +0.5) is added
#' inside the function and should not be included here.
#' @param exp A numeric vector with expected counts, i.e. number of reports
#' to be expected given a comparator or \emph{background}. Note that shrinkage
#' (e.g. +0.5) is added inside the function and should not be included here.
#' @param shrinkage A non-negative numeric of length 1, to be added to
#' observed and expected count. Default is 0.5.
#' @param sign_lvl Significance level of credibility interval. Default is
#' 0.95 (i.e. 95 \% credibility interval)
#'
#' @return A tibble with three columns (point estimate and credibility bounds).
#'
#' @examples
#' pvutils::ic(obs = 20, exp = 10)
#'
#' # Note that obs and exp can be vectors (of equal length, no recycling)
#' pvutils::ic(obs = c(20, 30), exp = c(10, 10))
#'
#' @export

ic <- function(obs, exp, shrinkage = 0.5, sign_lvl = 0.95) {
  # Run input checks
  checkmate::qassert(c(obs, exp), "N+[0,)")
  checkmate::qassert(shrinkage, "N1[0,)")
  checkmate::qassert(sign_lvl, "N1[0,1]")

  if (!length(obs == length(exp))) {
    stop("Vectors 'obs' and 'exp' are not of equal length.")
  }

  lower_prob <- (1 - sign_lvl) / 2
  upper_prob <- 1 - lower_prob

  output <- tibble::tibble(
    "ic_lower" = ci_for_ic(obs, exp, lower_prob, shrinkage),
    "ic" = log2((obs + shrinkage) / (exp + shrinkage)),
    "ic_upper" = ci_for_ic(obs, exp, upper_prob, shrinkage)
  )

  return(output)
}

#' @title Confidence intervals for Proportional Reporting Rate
#' @description Mainly for use in \code{\link{prr}}. Produces (symmetric,
#' normality based) confidence bounds for the PRR, for a passed probability.
#' Called twice in \code{prr} to create confidence intervals.
#' @param sign_lvl_probs The probabilities of the normal distribution, based on
#' a passed significance level (\code{sign_lvl}) in \code{\link{prr}}. If
#' \code{sgn_lvl = .95} in \code{prr}, quantiles of the normal distribution will
#' be extracted at \code{sgn_lvl_probs} of 0.025 and 0.975.
#' @seealso \code{\link{prr}}
#' @inheritParams prr
#' @export
ci_for_prr <- function(obs, n_drug, n_event_prr, n_tot_prr, sign_lvl_probs) {
  s_hat <- sqrt(1 / obs + 1 / n_drug + 1 / n_event_prr + 1 / n_tot_prr)
  (obs) / (n_drug * n_event_prr / n_tot_prr) * exp(stats::qnorm(sign_lvl_probs) * s_hat)
}

#' @title Proportional Reporting Rate
#'
#' @description Calculates Proportional Reporting Rate ("PRR") with
#' confidence intervals, used in disproportionality analysis.
#'
#' @details The PRR is the proportion of reports with an event in set of exposed
#' cases, divided with the proportion of reports with the event in a background
#' or comparator, which does not include the exposed.
#' The \emph{Reporting} highlights the correct interpretation, of examining
#' reporting as the PRR is calculated from a reporting database.
#' Note: the function is vectorized, see the examples.
#'
#' @param obs Number of reports for the specific drug and event (i.e. the
#' observed count).
#' @param n_drug Number of reports with the drug, without the event
#' @param n_event_prr Number of reports with the event in the background.
#' @param n_tot_prr Number of reports in the background.
#' @param sign_lvl Significance level of confidence interval. Default is
#' 0.95 (i.e. 95 \% confidence interval)
#' @return A tibble with three columns (point estimate and credibility bounds).
#' Number of rows equals length of inputs obs, n_drug, n_event_prr and n_tot_prr.
#'
#' @examples
#'
#' pvutils::prr(obs = 5, n_drug = 10, n_event_prr = 20, n_tot_prr = 10000)
#'
#' # Note that input parameters can be vectors (of equal length, no recycling)
#' pvutils::prr(
#'   obs = c(5, 10),
#'   n_drug = c(10, 20),
#'   n_event_prr = c(15, 30),
#'   n_tot_prr = c(10000, 10000)
#' )
#' @export
#'
prr <- function(obs, n_drug, n_event_prr, n_tot_prr, sign_lvl = 0.95) {
  checkmate::qassert(c(obs, n_drug, n_event_prr, n_tot_prr), "N+[0,)")
  checkmate::qassert(sign_lvl, "N1[0,1]")

  # Check that all vectors have the same length, seemed
  # hard to do in checkmate.
  if (!all(purrr::map(
    list(n_drug, n_event_prr, n_tot_prr),
    \(x){
      length(x)
    }
  ) == length(obs))) {
    stop("Vectors obs, n_drug, n_event_prr and n_tot_prr are not of equal length.")
  }

  lower_prob <- (1 - sign_lvl) / 2
  upper_prob <- 1 - lower_prob

  output <- tibble::tibble(
    "prr_lower" = ci_for_prr(obs, n_drug, n_event_prr, n_tot_prr, lower_prob),
    "prr" = obs / n_drug * (n_event_prr / n_tot_prr),
    "prr_upper" = ci_for_prr(obs, n_drug, n_event_prr, n_tot_prr, upper_prob)
  )
  output
}

#' @title Wrapper for adding disproportionality estimates to data frame
#' containing expected counts
#' @inheritParams add_expected_counts
#' @param da_estimators Character vector, defaults to c("ic", "prr", "ror").
#' @param rule_of_N Numeric. To protect against spurious
#' associations due to small observed counts, prr and ror point and
#' interval estimates are set to NA when the observed is less or equal to
#' the value passed as 'rule_of_N'. Defaults to 3, but 5 is sometimes used.
#' Set to NULL if you don't want to apply any such rule.
#' @param number_of_digits Integer. Defaults to 2. Set to NULL to avoid rounding.
#' @param ... For passing additional arguments, e.g. significance level.
#' @return The passed data frame with additional columns as specified by
#' parameters.
#' @export
add_disproportionality <- function(df,
                            da_estimators = c("ic", "prr", "ror"),
                            rule_of_N = 3,
                            number_of_digits = 2,
                            ...) {

  checkmate::qassert(rule_of_N, c("N1[0,]", "0"))
  # Function round actually rounds decimals in digits, so we can pass this on
  # without further concerns
  checkmate::qassert(number_of_digits, c("N1[0,]", "0"))


  da_df <- df

  if ("ic" %in% da_estimators) {
    ic_df <- ic(da_df$obs, da_df$exp_rrr)
    da_df <- da_df |> dplyr::bind_cols(ic_df)
  }

  if ("prr" %in% da_estimators) {
    prr_df <- prr(
      obs = da_df$obs,
      n_drug = da_df$n_drug,
      n_event_prr = da_df$n_event_prr,
      n_tot_prr = da_df$n_tot_prr
    )

    da_df <- da_df |> dplyr::bind_cols(prr_df)
  }

  if ("ror" %in% da_estimators) {
    ror_df <- ror(
      a = da_df$obs,
      b = da_df$b,
      c = da_df$c,
      d = da_df$d
    )

    da_df <- da_df |> dplyr::bind_cols(ror_df)
  }

  # "Rule of three"
  if (any(c("ror", "prr") %in% da_estimators) & !is.null(rule_of_N)) {
    # Apply rule of N to these colnames
    da_estimators_not_ic <- stringr::str_subset(da_estimators, "ic", negate = T)

    # Only need to do this check once
    replace_these_rows <- da_df[["obs"]] <= rule_of_N

    da_df |>
      dplyr::mutate(dplyr::across(
        dplyr::starts_with(da_estimators_not_ic),
        ~ ifelse(replace_these_rows, NA, dplyr::cur_column())
      ))
  }

  # Rounding of output
  if (!is.null(number_of_digits)) {

    # Only apply to non-report-count columns, i.e. expected or da_estimates
    da_df |> dplyr::mutate(dplyr::across(
      dplyr::starts_with(c("exp", da_estimators)),
      ~ round(.x, digits = number_of_digits)
    ))
  }

  return(da_df)
}
