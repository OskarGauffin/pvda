# ---------------------------------------------------------------------------- #
# Headers below are function names, in foldable code sections,
# indexed according to their calling hierarchy.
# Collapse All with "Alt + O"
# And expand All with "Shift + Alt + O"
# ---------------------------------------------------------------------------- #
### Called from add_expected_counts ----
#  1.1 count_expected_rrr ----
#' @title Count Expected for Relative Reporting Rate
#' @description Internal function to provide expected counts related to the RRR
#' @param df See documentation for add_expected_counts
#' @param df_colnames See documentation for da
#' @param df_syms A list built from df_colnames through conversion to symbols.
#' @return A data frame with columns for obs, n_drug,
#' n_event, n_tot and (RRR) expected
#' @importFrom dtplyr lazy_dt
#' @importFrom dplyr distinct mutate n_distinct group_by ungroup count rename select
count_expected_rrr <- function(df, df_colnames, df_syms) {
  NULL -> desc -> ends_with -> exp_rrr -> obs -> n -> n_event -> n_drug -> n_tot
  assign(df_colnames$report_id, NULL)
  assign(df_colnames$event, NULL)
  assign(df_colnames$drug, NULL)

  count_dt <- dtplyr::lazy_dt(df, immutable = FALSE) |>
    dplyr::distinct() |>
    dplyr::mutate(n_tot = dplyr::n_distinct(!!df_syms$report_id)) |>
    dplyr::group_by(!!df_syms$drug) |>
    dplyr::mutate(n_drug = dplyr::n_distinct(!!df_syms$report_id)) |>
    dplyr::group_by(!!df_syms$drug) |>
    dplyr::group_by(!!df_syms$event) |>
    dplyr::mutate(n_event = dplyr::n_distinct(!!df_syms$report_id)) |>
    dplyr::ungroup() |>
    dplyr::count(
      !!df_syms$drug,
      !!df_syms$event,
      n_tot,
      n_drug,
      n_event
    ) |>
    dplyr::rename(obs = n) |>
    # Note that the as.numeric must be called in the same mutate as we do
    # the multiplication
    dplyr::mutate(exp_rrr = as.numeric(n_drug) * as.numeric(n_event) /
      as.numeric(n_tot)) |>
    dplyr::select(
      !!df_syms$drug,
      !!df_syms$event,
      obs,
      n_drug,
      n_event,
      n_tot,
      exp_rrr
    )

  return(count_dt)
}

#  1.2 count_expected_prr ----
#' @title Count expected for Proportional Reporting Rate
#' @description Internal function to provide expected counts related to the PRR
#' @param count_dt A data table, output from count_expected_rrr
#' @return A data table with added columns for n_event_prr
#' n_tot_prr and expected_prr
#'  @export
#' @importFrom dplyr mutate select
#' @importFrom tidyselect everything
count_expected_prr <- function(count_dt) {
  # data.table complains if you haven't defined these variables as NULLs
  NULL -> desc -> ends_with -> exp_prr -> n_tot_prr ->
  n_event_prr -> obs -> n -> n_event -> n_drug -> n_tot

  count_dt <- count_dt |>
    dplyr::mutate(
      n_event_prr = n_event - obs,
      n_tot_prr = n_tot - n_drug
    ) |>
    dplyr::mutate(exp_prr = as.numeric(n_drug) * as.numeric(n_event_prr) /
      as.numeric(n_tot_prr)) |>
    dplyr::select(tidyselect::everything(), n_event_prr, n_tot_prr, exp_prr)

  return(count_dt)
}

#  1.1 count_expected_ror ----
#' @title Count expected for Reporting Odds Ratio
#' @description Internal function to provide expected counts related to the ROR
#' @param count_dt A data table, output from count_expected_rrr
#' @return A data table with added columns for n_event_prr,
#' n_tot_prr and expected_prr
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @seealso
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{select}}
#'  \code{\link[tidyselect]{everything}}
#' @rdname count_expected_ror
#' @importFrom dplyr mutate select
#' @importFrom tidyselect everything
count_expected_ror <- function(count_dt) {
  # data.table complains if you haven't defined these variables as NULLs
  NULL -> desc -> ends_with -> exp_ror -> d -> b -> n -> n_event -> n_drug ->
  n_tot -> obs -> n_event_prr -> n_tot_prr

  count_dt <- count_dt |>
    dplyr::mutate(
      b = n_drug - obs,
      c = n_event_prr,
      d = n_tot_prr - n_event + obs
    ) |>
    dplyr::mutate(exp_ror = as.numeric(b) * as.numeric(c) / as.numeric(d)) |>
    dplyr::select(tidyselect::everything(), b, c, d, exp_ror)
  return(count_dt)
}



#-----------------------------------
### Called from add_disproportionality ----
# 1.1 ic ----
#' @title Information component
#'
#' @description Calculates the information component ("IC") and credibility
#' interval, used in disproportionality analysis.
#'
#' @details The IC is a log2-transformed observed-to-expected ratio, based on
#' the relative reporting rate (RRR) for counts, but modified with an addition
#' of "shrinkage" to protect against spurious associations.
#'
#' \deqn{\hat{IC} = log_{2}(\frac{\hat{O}+k}{\hat{E}+k})}
#'
#' where \eqn{\hat{O}} = observed number of reports, \eqn{k} is the shrinkage
#' (typically +0.5), and expected \eqn{\hat{E}} is (for RRR, and using the
#' entire database as comparator or \emph{background}) estimated as
#'
#' \deqn{ \hat{E} = \frac{\hat{N}_{drug} \times \hat{N}_{event}}{\hat{N}_{TOT}}}
#'
#' where \eqn{\hat{N}_{drug}}, \eqn{\hat{N}_{event}} and \eqn{\hat{N}_{TOT}} are the number of
#' reports with the drug, the event, and in the whole database respectively.
#'
#' The credibility interval is created from the quantiles of the posterior
#' gamma distribution with shape (\eqn{\hat{S}}) and rate (\eqn{\hat{R}}) parameters as
#'
#' \deqn{\hat{S} = \hat{O} + k}
#' \deqn{\hat{R} = \hat{E} + k}
#'
#' using the \code{stats::qgamma} function. Parameter \eqn{k} is the shrinkage defined
#' earlier. For completeness, a credibility interval of the gamma distributed \eqn{X} (i.e.
#' \eqn{X \sim \Gamma(\hat{S}, \hat{R})} where \eqn{\hat{S}} and \eqn{\hat{R}} are shape and rate parameters)
#' with associated quantile function \eqn{Q_X(p)} for a significance level \eqn{\alpha} is
#' constructed as
#'
#' \deqn{[Q_X(\alpha/2), Q_X(1-\alpha/2)]}
#'
#' @section Further details:
#' From a bayesian point-of-view, the credibility interval of the IC is constructed
#' from the poisson-gamma conjugacy. The shrinkage constitutes a prior of
#' observed and expected of 0.5. A shrinkage of +0.5 with a gamma-quantile based 95 \%
#' credibility interval cannot have lower bound above 0 unless the observed count
#' exceeds 3. One benefit of \eqn{log_{2}} is to provide
#' a log-scale for convenient plotting of multiple IC values side-by-side.
#'
#' @param obs A numeric vector with observed counts, i.e. number of reports
#' for the selected drug-event-combination. Note that shrinkage (e.g. +0.5) is added
#' inside the function and should not be included here.
#' @param exp A numeric vector with expected counts, i.e. number of reports
#' to be expected given a comparator or \emph{background}. Note that shrinkage
#' (e.g. +0.5) is added inside the function and should not be included here.
#' @param shrinkage A non-negative numeric value, to be added to
#' observed and expected count. Default is 0.5.
#' @inheritParams conf_lvl_to_quantile_prob
#'
#' @return A tibble with three columns (point estimate and credibility bounds).
#'
#' @examples
#' ic(obs = 20, exp = 10)
#'
#' # Note that obs and exp can be vectors (of equal length, no recycling allowed)
#' ic(obs = c(20, 30), exp = c(10, 10))
#' @importFrom Rdpack reprompt
#' @references \insertRef{Nor_n_2011}{pvutils}
#' @export

ic <- function(obs = NULL,
               exp = NULL,
               shrinkage = 0.5,
               conf_lvl = 0.95) {
  # Run input checks
  checkmate::qassert(c(obs, exp), "N+[0,)")
  checkmate::qassert(shrinkage, "N1[0,)")

  if (!length(obs == length(exp))) {
    stop("Vectors 'obs' and 'exp' are not of equal length.")
  }

  quantile_prob <- conf_lvl_to_quantile_prob(conf_lvl)
  ic_colnames <- colnames_da(quantile_prob, da_name = "ic")

  output <- tibble::tibble(
    !!ic_colnames$lower := ci_for_ic(obs, exp, quantile_prob$lower, shrinkage),
    "ic" = log2((obs + shrinkage) / (exp + shrinkage)),
    !!ic_colnames$upper := ci_for_ic(obs, exp, quantile_prob$upper, shrinkage)
  )

  return(output)
}

# 1.2 prr ----
#' @title Proportional Reporting Rate
#'
#' @description Calculates Proportional Reporting Rate ("PRR") with
#' confidence intervals, used in disproportionality analysis.
#'
#' @details The PRR is the proportion of reports with an event in set of exposed
#' cases, divided with the proportion of reports with the event in a background
#' or comparator, which does not include the exposed.
#'
#' @param obs Number of reports for the specific drug and event (i.e. the
#' observed count).
#' @param n_drug Number of reports with the drug, without the event
#' @param n_event_prr Number of reports with the event in the background.
#' @param n_tot_prr Number of reports in the background.
#' @inheritParams conf_lvl_to_quantile_prob
#'
#' @details The PRR is estimated from a observed-to-expected ratio, based on
#' similar to the RRR and IC, but excludes the exposure of interest from the
#' comparator.
#'
#' \deqn{\hat{PRR} = \frac{\hat{O}}{\hat{E}}}
#'
#' where \eqn{\hat{O}} is the observed number of reports, and expected \eqn{\hat{E}}
#' is estimated as
#'
#' \deqn{\hat{E} = \frac{\hat{N}_{drug} \times (\hat{N}_{event} - \hat{O})}{\hat{N}_{TOT}-\hat{N}_{drug}}}
#'
#' where \eqn{\hat{N}_{drug}}, \eqn{\hat{N}_{event}}, \eqn{\hat{O}} and \eqn{\hat{N}_{TOT}} are
#' the number of reports with the drug, the event, the drug and event, and
#' in the whole database respectively.
#'
#' A confidence interval is derived in Gravel (2009), using the delta method:
#' \deqn{\hat{s} = \sqrt{ 1/\hat{O} - 1/(\hat{N}_{drug}) + 1/(\hat{N}_{event} - \hat{O}) - 1/(\hat{N}_{TOT} - \hat{N}_{drug})}}
#'
#' and \deqn{[\hat{CI}_{\alpha/2}, \hat{CI}_{1-\alpha/2}] = }
#' \deqn{[\frac{\hat{O}}{\hat{E}} \times \exp(\Phi_{\alpha/2} \times \hat{s}),
#' \frac{\hat{O}}{\hat{E}} \times \exp(\Phi_{1-\alpha/2} \times \hat{s})]}
#'
#' Another version of this standard deviation is sometimes used where the last
#' fraction is added rather than subtracted, with negligible practical implications.
#'
#' @return A tibble with three columns (point estimate and credibility bounds).
#' Number of rows equals length of inputs obs, n_drug, n_event_prr and n_tot_prr.
#'
#' @examples
#'
#' pvutils::prr(
#'   obs = 5,
#'   n_drug = 10,
#'   n_event_prr = 20,
#'   n_tot_prr = 10000
#' )
#'
#' # Note that input parameters can be vectors (of equal length, no recycling)
#' pvutils::prr(
#'   obs = c(5, 10),
#'   n_drug = c(10, 20),
#'   n_event_prr = c(15, 30),
#'   n_tot_prr = c(10000, 10000)
#' )
#' @references
#' \insertRef{Montastruc_2011}{pvutils}
#'
#' \insertRef{MscThesis}{pvutils}
#' @export
#'
prr <- function(obs = NULL,
                n_drug = NULL,
                n_event_prr = NULL,
                n_tot_prr = NULL,
                conf_lvl = 0.95) {
  checkmate::qassert(c(obs, n_drug, n_event_prr, n_tot_prr), "N+[0,)")

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

  # Integer overflow on vaers-sized data sets if these are not converted to double
  obs <- as.numeric(obs)
  n_drug <- as.numeric(n_drug)
  n_event_prr <- as.numeric(n_event_prr)
  n_tot_prr <- as.numeric(n_tot_prr)

  quantile_prob <- conf_lvl_to_quantile_prob(conf_lvl)
  prr_colnames <- colnames_da(quantile_prob, "prr")

  output <- tibble::tibble(
    !!prr_colnames$lower := ci_for_prr(obs, n_drug, n_event_prr, n_tot_prr, quantile_prob$lower),
    "prr" = obs / (n_drug * (n_event_prr / n_tot_prr)),
    !!prr_colnames$upper := ci_for_prr(obs, n_drug, n_event_prr, n_tot_prr, quantile_prob$upper)
  )
  output
}

# 1.3 ror ----
#' @title Reporting Odds Ratio
#'
#' @description Calculates Reporting Odds Ratio ("ROR") and confidence
#' intervals, used in disproportionality analysis.
#'
#' @details The ROR is an odds ratio calculated from reporting counts. The
#' R for Reporting in ROR is meant to emphasize an interpretation of reporting,
#' as the ROR is calculated from a reporting database. Note: the function is
#' vectorized, i.e. a, b, c and d can be vectors, see the examples.
#' @param a Number of reports for the specific drug and event (i.e. the
#' observed count).
#' @param b Number of reports with the drug, without the event
#' @param c Number of reports without the drug, with the event
#' @param d Number of reports without the drug, without the event
#' @inheritParams conf_lvl_to_quantile_prob
#' @return A tibble with three columns (point estimate and credibility bounds).
#' Number of rows equals length of inputs a, b, c, d.
#' @details A reporting odds ratio is simply an odds ratio based on adverse event
#' reports.
#' \deqn{\hat{ROR} = \frac{a/b}{c/d}}
#'
#' where \eqn{a} = observed count (i.e. number of reports with exposure and
#' outcome), \eqn{b} = number of reports with the drug and without the event,
#' \eqn{c} = number of reports without the drug with the event and \eqn{d} =
#' number of reports with neither of the drug and the event.
#'
#' A confidence interval for the ROR can be derived through the delta method,
#' with a standard deviation:
#'
#' \deqn{\hat{s} = \sqrt{1/a + 1/b + 1/c + 1/d}}
#'
#' with the resulting confidence interval for significance level \eqn{\alpha}
#'
#' \deqn{[\hat{ROR} \times exp(\Phi_{\alpha/2} \times \hat{s}), \hat{ROR} \times exp(\Phi_{1-\alpha/2} \times \hat{s})]}
#'
#' @examples
#'
#' pvutils::ror(
#'   a = 5,
#'   b = 10,
#'   c = 20,
#'   d = 10000
#' )
#'
#' # Note that a, b, c and d can be vectors (of equal length, no recycling)
#' pvutils::ror(
#'   a = c(5, 10),
#'   b = c(10, 20),
#'   c = c(15, 30),
#'   d = c(10000, 10000)
#' )
#' @references
#' \insertRef{Montastruc_2011}{pvutils}
#' @export
#'
ror <- function(a = NULL,
                b = NULL,
                c = NULL,
                d = NULL,
                conf_lvl = 0.95) {
  checkmate::qassert(c(a, b, c, d), "N+[0,)")

  # Check that all vectors have the same length, seemed
  # hard to do in checkmate.
  if (!all(purrr::map(list(b, c, d), \(x){
    length(x)
  }) == length(a))) {
    stop("Vectors a, b, c and d are not of equal length.")
  }

  quantile_prob <- conf_lvl_to_quantile_prob(conf_lvl)
  ror_colnames <- colnames_da(quantile_prob, "ror")

  # Integer overflow on vaers-sized data sets if these are not converted to double
  a <- as.numeric(a)
  b <- as.numeric(b)
  c <- as.numeric(c)
  d <- as.numeric(d)

  output <- tibble::tibble(
    !!ror_colnames$lower := ci_for_ror(a, b, c, d, quantile_prob$lower),
    "ror" = a * d / (b * c),
    !!ror_colnames$upper := ci_for_ror(a, b, c, d, quantile_prob$upper)
  )

  return(output)
}

# 2.1 conf_lvl_to_quantile_prob ----
#' @title Quantile probabilities from confidence level
#' @description Calculates equi-tailed quantile probabilities from a
#' confidence level
#' @param conf_lvl Confidencelevel of confidence or credibility intervals.
#' Default is 0.95 (i.e. 95 \% confidence interval)
#' @return A list with two numerical vectors, "lower" and "upper".
#' @examples
#' conf_lvl_to_quantile_prob(0.95)
#' @export
conf_lvl_to_quantile_prob <- function(conf_lvl = 0.95) {
  checkmate::qassert(conf_lvl, "N1[0,1]")

  lower_prob <- (1 - conf_lvl) / 2
  upper_prob <- 1 - lower_prob
  output <- list("lower" = lower_prob, "upper" = upper_prob)

  return(output)
}


# 2.2 colnames_da -----

#' @title An internal function creating colnames for da confidence/credibility bounds
#' @description Given the output from quantile_prob, and a da_name string,
#' create column names such as PRR025, ROR025 and IC025
#' @param quantile_prob A list with two parameters, lower and upper. Default: list(lower = 0.025, upper = 0.975)
#' @param da_name A string, such as "ic", "prr" or "ror". Default: NULL
#' @return A list with two symbols, to be inserted in the dtplyr-chain
#' @export
colnames_da <- function(quantile_prob = list("lower" = 0.025, "upper" = 0.975),
                        da_name = NULL) {
  ic_lower_name <- paste0(da_name, 100 * quantile_prob$lower)
  ic_upper_name <- paste0(da_name, 100 * quantile_prob$upper)

  return(list(
    lower = rlang::sym(ic_lower_name),
    upper = rlang::sym(ic_upper_name)
  ))
}
# 2.3 ci_for_ic ----
#' @title Confidence intervals for Information Component (IC)
#' @description Mainly used in \code{link{ic}}. Produces quantiles of the
#' posterior gamma distribution. Called twice in \code{ic} to create a
#' credibility interval.
#' @param conf_lvl_probs The probabilities of the posterior, based on
#' a passed confidence level (\code{conf_lvl}) in \code{\link{ic}}. For
#' instance, if \code{sgn_lvl = .95} in \code{ic} is used, quantiles will be
#' extracted at \code{sgn_lvl_probs} 0.025 and 0.975.
#' @seealso \code{\link{ic}}
#' @inheritParams ic
#' @export
ci_for_ic <- function(obs,
                      exp,
                      conf_lvl_probs,
                      shrinkage) {
  output <- log2(stats::qgamma(
    p = conf_lvl_probs,
    shape = obs + shrinkage,
    rate = exp + shrinkage
  ))
  return(output)
}

# 2.4 ci_for_prr ----
#' @title Confidence intervals for Proportional Reporting Rate
#' @description Mainly for use in \code{\link{prr}}. Produces (symmetric,
#' normality based) confidence bounds for the PRR, for a passed probability.
#' Called twice in \code{prr} to create confidence intervals.
#' @param conf_lvl_probs The probabilities of the normal distribution, based on
#' a passed confidence level (\code{conf_lvl}) in \code{\link{prr}}. If
#' \code{sgn_lvl = .95} in \code{prr}, quantiles of the normal distribution will
#' be extracted at \code{sgn_lvl_probs} of 0.025 and 0.975.
#' @seealso \code{\link{prr}}
#' @inheritParams prr
#' @export
ci_for_prr <- function(obs = NULL,
                       n_drug = NULL,
                       n_event_prr = NULL,
                       n_tot_prr = NULL,
                       conf_lvl_probs = 0.95) {
  s_hat <- sqrt(1 / obs - 1 / n_drug + 1 / n_event_prr - 1 / n_tot_prr)
  (obs) / (n_drug * n_event_prr / n_tot_prr) * exp(stats::qnorm(conf_lvl_probs) * s_hat)
}

# 2.5 ci_for_ror ----
#' @title Confidence intervals for Reporting Odds Ratio
#' @description Mainly for use in \code{\link{ror}}. Produces (symmetric,
#' normality based) confidence bounds for the ROR, for a passed probability.
#' Called twice in \code{ror} to create confidence intervals.
#' @param conf_lvl_probs The probabilities of the normal distribution, based on
#' a passed confidence level (\code{conf_lvl}) in \code{\link{ror}}. If
#' \code{sgn_lvl = .95} in \code{ror}, quantiles of the normal distribution will
#' be extracted at \code{sgn_lvl_probs} of 0.025 and 0.975.
#' @seealso \code{\link{ror}}
#' @inheritParams ror
#' @export
ci_for_ror <- function(a, b, c, d, conf_lvl_probs) {
  exp(log((a * d) / (b * c)) + stats::qnorm(conf_lvl_probs) *
    sqrt(1 / a + 1 / b + 1 / c + 1 / d))
}

# 2.6 apply_rule_of_N ----
#' @title apply_rule_of_N
#' @description Internal fcn to set da-columns to NA when observed count < 3
#' @param da_df See the intermediate object da_df in add_disproportionality
#' @param da_estimators Default is c("ic", "prr", "ror").
#' @param rule_of_N PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details Sometimes, you want to protect yourself from spurious findings based
#' on small observed counts combined with infinitesimal expected counts.
#' @importFrom stringr str_subset
#' @importFrom dplyr mutate across starts_with cur_column
#' @export
apply_rule_of_N <- function(da_df = NULL,
                            da_estimators = c("ic", "prr", "ror"),
                            rule_of_N = NULL) {
  if (!is.null(rule_of_N)) {
    # Rule of N = 3 is built into the IC
    da_estimators_not_ic <- stringr::str_subset(da_estimators, "ic", negate = T)

    # We only need to check the observed once
    replace_these_rows <- da_df[["obs"]] < rule_of_N

    da_df <-
      da_df |>
      dplyr::mutate(
        dplyr::across(
          dplyr::starts_with(da_estimators_not_ic),
          ~ ifelse(replace_these_rows, NA, .x)
        )
      )
  }

  return(da_df)
}
# 2.7 round_columns_with_many_decimals ----
#' @title Rounds columns in da_df with many decimals
#' @description Internal function containing a mutate + across
#' @param da_df See add_disproportionality
#' @param da_estimators See add_disproportionality
#' @param number_of_digits See add_disproportionality
#' @return A df with rounded columns
#' @importFrom dplyr mutate across starts_with
#' @export
round_columns_with_many_decimals <- function(da_df = NULL, da_estimators = NULL, number_of_digits = NULL) {
  if (!is.null(number_of_digits)) {
    da_df <-
      da_df |> dplyr::mutate(dplyr::across(
        dplyr::starts_with(c("exp", da_estimators)),
        ~ round(.x, digits = number_of_digits)
      ))
  }
  return(da_df)
}

# 2.8 sort_by_lower_da_limit ----
#' @title Sort a disproportionality analysis by the lower da conf. or cred. limit
#' @description Sorts the output by the mean lower limit of a passed da estimator
#' @param df See add_disproportionality
#' @param df_colnames See add_disproportionality
#' @param df_syms See add_disproportionality
#' @param conf_lvl See add_disproportionality
#' @param sort_by See add_disproportionality
#' @param da_estimators See add_disproportionality
#' @param number_of_digits Numeric value. Set the number of digits to show in output by passing
#' an integer. Default value is 2 digits. Set to NULL to avoid rounding.
#' @return The df object, sorted.
#' @export
#' @importFrom checkmate qassert
#' @importFrom purrr pluck
#' @importFrom dplyr group_by summarise left_join arrange select
#' @importFrom rlang sym

round_and_sort_by_lower_da_limit <- function(df = NULL,
                                             df_colnames = NULL,
                                             df_syms = NULL,
                                             conf_lvl = NULL,
                                             sort_by = NULL,
                                             da_estimators = NULL,
                                             number_of_digits = 2) {
  NULL -> desc -> mean_da

  # The round, using number_of_digits below, rounds decimals in digits, so we
  # can pass this on without further checks
  checkmate::qassert(number_of_digits, c("N1[0,]", "0"))


  checkmate::qassert(sort_by, "S1")
  if (!sort_by %in% da_estimators) {
    stop("The da estimator you've passed as sort_by must be included in da_estimators,
             currently ", paste0(da_estimators, ", "))
  }

  sort_by_colname <- conf_lvl |>
    pvutils::conf_lvl_to_quantile_prob() |>
    pvutils::colnames_da(da_name = sort_by) |>
    purrr::pluck("lower")

  # Take the mean lower quantile for the chosen da and sort by it.
  sorted_df <- df |>
    dplyr::group_by(!!df_syms$drug, !!df_syms$event) |>
    dplyr::summarise(mean_da = mean(!!rlang::sym(sort_by_colname), na.rm = T))

  df <-
    df |>
    dplyr::left_join(sorted_df, by = c(df_colnames$drug, df_colnames$event))

  if (is.null(df_colnames$group_by)) {
    df <-
      df |>
      dplyr::arrange(desc(mean_da))
  } else {
    df <-
      df |>
      dplyr::arrange(desc(mean_da), !!df_syms$group_by)
  }

  df <-
    df |>
    dplyr::select(-mean_da)

  df <-
    df |>
    round_columns_with_many_decimals(da_estimators, number_of_digits)

  return(df)
}
#-----------------------------------
