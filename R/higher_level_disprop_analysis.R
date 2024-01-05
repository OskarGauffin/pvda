# ---------------------------------------------------------------------------- #
# Headers below are function names, in foldable code sections,
# indexed according to their calling hierarchy.
# Collapse All with "Alt + O"
# And expand All with "Shift + Alt + O"
# ---------------------------------------------------------------------------- #

# 0.1.1 grouped_da -----
#' @title Disproportionality Analysis by Subgroups
#' @description A package internal wrapper for executing da across subgroups
#' @param df See the da function
#' @param df_colnames See the da function
#' @param df_syms A list built from df_colnames through conversion to symbols.
#' @param expected_count_estimators See the da function
#' @param da_estimators See the da function
#' @param sort_by See the da function
#' @param conf_lvl See the da function
#' @param rule_of_N See the da function
#' @param number_of_digits See the da function
#' @return See the da function
#' @details See the da documentation
#' @importFrom rlang sym
#' @importFrom dplyr slice pull bind_cols select
grouped_da <- function(df = NULL,
                       df_colnames = NULL,
                       df_syms = NULL,
                       expected_count_estimators = NULL,
                       da_estimators = NULL,
                       sort_by = NULL,
                       conf_lvl = NULL,
                       rule_of_N = NULL,
                       number_of_digits = NULL) {
  assign(df_colnames$event, NULL)
  assign(df_colnames$drug, NULL)
  assign(df_colnames$group_by, NULL)

  drug <- rlang::sym(df_colnames[["drug"]])
  event <- rlang::sym(df_colnames[["event"]])
  group_by <- rlang::sym(df_colnames[["group_by"]])

  current_group <- df |>
    dplyr::slice(1) |>
    dplyr::pull(!!group_by)

  output <-
    df |>
    pvda::add_expected_counts(
      df_colnames = df_colnames,
      df_syms = df_syms,
      expected_count_estimators = expected_count_estimators
    ) |>
    pvda::add_disproportionality(
      df_syms = df_syms,
      da_estimators = da_estimators,
      conf_lvl = conf_lvl,
      rule_of_N = rule_of_N
    ) |>
    dplyr::bind_cols(!!group_by := current_group)

  return(output)
}

# 1.1 add_expected_counts ----
#' @title Produces expected counts
#' @description Produces various counts used in disproportionality analysis.
#' @param df An object possible to convert to a data table, e.g.
#' a tibble or data.frame, containing patient level reported drug-event-pairs.
#' See header 'The df object' below for further details.
#' @param df_colnames A list of column names to use in \code{df}. That is,
#' point \code{da} to the 'report id'-column (\code{report_id}), the
#' 'drug name'-column (\code{drug}), the 'adverse event'-column (\code{event})
#' and optionally a grouping column \code{group_by} to calculate disproportionality
#' across. See the vignette for further details.
#' @param df_syms A list built from df_colnames through conversion to symbols.
#' @param expected_count_estimators A character vector containing the desired
#' expected count estimators. Defaults to c("rrr", "prr", "ror").
#'
#' @section The df object:
#'  The passed \code{df} should be (convertible to) a data table and at least contain three
#'  columns: \code{report_id}, \code{drug} and \code{event}. The data table should contain one row
#'  per reported drug-event-combination, i.e. receiving a single additional report
#'  for drug X and event Y would add one row to the table. If the single report
#'  contained drug X for event Y and event Z, two rows would be added, with the
#'  same \code{report_id} and \code{drug} on both rows. Column \code{report_id} must be of type
#'  numeric or character. Columns \code{drug} and \code{event} must be of type character.
#'  If column \code{group_by} is provided, it can be either numeric or character.
#'  You can use a \code{df} with column names of your choosing, as long as you
#'  connect role and name in the \code{df_colnames}-parameter.
#'
#' @return A tibble containing the various counts.
#' @importFrom dplyr arrange count distinct everything group_by mutate n_distinct
#' rename select ungroup
#' @import data.table
#' @export
add_expected_counts <- function(df = NULL,
                                df_colnames = NULL,
                                df_syms = NULL,
                                expected_count_estimators = c("rrr", "prr", "ror")) {
  # dtplyr/data.table complains if we don't put obs to NULL. Similar requirements
  # in lower level functions (count_expected_rrr, count_expected_prr,
  # count_expected_ic) called in this function
  obs <- NULL
  checkmate::qassert(df[[df_colnames$report_id]], c("S+", "N+"))
  checkmate::qassert(df[[df_colnames$drug]], "S+")
  checkmate::qassert(df[[df_colnames$event]], "S+")

  obligatory_df_colnames <-
    df |>
    dplyr::select(df_colnames$report_id, df_colnames$drug, df_colnames$event) |>
    colnames()

  if (!any(utils::hasName(df, obligatory_df_colnames))) {
    stop("At least one of column names 'report_id', 'drug' and 'event' is not
         found. Please check the passed df object.")
  }

  checkmate::qassert(expected_count_estimators, "S+")
  if (any(!expected_count_estimators %in% c("rrr", "prr", "ror"))) {
    stop("Only 'rrr', 'prr' and 'ror' are allowed in parameter
         'expected_count_estimators'")
  }

  if (!typeof(df) == "data.table") {
    df <- data.table::as.data.table(df)
  }

  #  Begin with the RRR counts, as they're the computationally most feasible,
  #  and useful for calculating PRR and ROR.
  count_dt <- count_expected_rrr(df, df_colnames, df_syms)

  # Calc PRR counts if requested
  if (any(c("ror", "prr") %in% expected_count_estimators)) {
    count_dt <- count_expected_prr(count_dt)
  }

  # Calc ROR counts if requested. Count "a" equal obs.
  if ("ror" %in% expected_count_estimators) {
    count_dt <- count_expected_ror(count_dt)
  }

  if (!"rrr" %in% expected_count_estimators) {
    count_dt <- count_dt |> dplyr::select(-tidyselect::ends_with("rrr"))
  }

  # Until the base pipe can handle an arrange(desc(obs)):
  desc_obs_order <- dplyr::desc(x = as.data.frame(count_dt)$obs)

  count_df <-
    count_dt |>
    tibble::as_tibble()

  return(count_df)
}

# 1.2 add_disproportionality ----
#' @title Add disproportionality estimates to data frame
#' with expected counts
#' @inheritParams ror
#' @inheritParams add_expected_counts
#' @param df Intended use is on the output tibble from \code{add_expected_counts}.
#' @param da_estimators Character vector specifying which disproportionality
#' estimators to use, in case you don't need all implemented options. Defaults
#' to c("ic", "prr", "ror").
#' @param rule_of_N Numeric value. Sets estimates for ROR and PRR to NA when observed
#' counts are strictly less than the passed value of \code{rule_of_N}. Default value
#' is 3, 5 is sometimes used as a more liberal alternative. Set to NULL if you
#' don't want to apply any such rule.
#' @return The passed data frame with disproportionality point and interval
#' estimates.
#' @export
add_disproportionality <- function(df = NULL,
                                   df_syms = NULL,
                                   da_estimators = c("ic", "prr", "ror"),
                                   rule_of_N = 3,
                                   conf_lvl = 0.95) {
  NULL -> obs -> exp_rrr -> exp_prr -> exp_ror

  if (!all(da_estimators %in% c("ic", "prr", "ror"))) {
    stop("Passed parameter 'da_estimators' must consist of 'ic', 'prr' or 'ror'")
  }
  checkmate::qassert(rule_of_N, c("N1[0,]", "0"))

  df <- df

  if ("ic" %in% da_estimators) {
    ic_df <- pvda::ic(
      obs = df$obs,
      exp = df$exp_rrr,
      conf_lvl = conf_lvl
    )
    df <-
      df |>
      dplyr::bind_cols(ic_df)
  }

  if ("prr" %in% da_estimators) {
    prr_df <- pvda::prr(
      obs = df$obs,
      n_drug = df$n_drug,
      n_event_prr = df$n_event_prr,
      n_tot_prr = df$n_tot_prr,
      conf_lvl = conf_lvl
    )

    df <-
      df |>
      dplyr::bind_cols(prr_df)
  }

  if ("ror" %in% da_estimators) {
    ror_df <- pvda::ror(
      a = df$obs,
      b = df$b,
      c = df$c,
      d = df$d,
      conf_lvl = conf_lvl
    )

    df <- df |> dplyr::bind_cols(ror_df)
  }

  # Do some clean up
  df <-
    df |>
    apply_rule_of_N(da_estimators, rule_of_N) |>
    dplyr::select(
      !!df_syms$drug,
      !!df_syms$event,
      obs,
      exp_rrr,
      dplyr::starts_with("ic"),
      exp_prr,
      dplyr::starts_with("prr"),
      exp_ror,
      dplyr::starts_with("ror"),
      everything()
    )
}
# See lower_level_disprop_analysis.R for further details ----
