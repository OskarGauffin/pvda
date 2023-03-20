# ---------------------------------------------------------------------------- #
# Headers below are function names, in foldable code sections,
# indexed according to their calling hierarchy.
# Collapse All with "Alt + O"
# And expand All with "Shift + Alt + O"
# ---------------------------------------------------------------------------- #

# 0.1 da ----
#' @title Disproportionality Analysis
#' @description \code{da} is used to execute a disproportionality analysis,
#' potentially across subgroups (see parameter \code{group_by}), using the
#' Information Component (IC), Proportional Reporting Rate (PRR) and/or the
#' Reporting Odds Ratio (ROR).
#' @inheritParams add_expected_counts
#' @inheritParams add_disproportionality
#' @inheritParams ror
#' @param excel_path To write the output of \code{da} to an excel file, provide a path
#' to a folder e.g. to write to your current working directory, pass \code{getwd()}.
#'  The excel file will by default be named \code{da.xlsx}. To control the excel file name,
#'  pass a path ending with the desired filename suffixed with \code{.xlsx}. If you
#'  do not want to export the output to an excel file, pass NULL (the default).
#' @inheritParams add_expected_counts The df object
#' @return \code{da} returns a data frame (invisibly) containing counts and
#' estimates related to supported disproportionality estimators. Each row
#' corresponds to a drug-event pair.
#' @examples
#' ### Run a disproportionality analysis
#' da_1 <-
#'   drug_event_df |>
#'   da()
#'
#' ### Run a disproportionality analysis on a data frame with your own column names
#' # Create the list with colnames for the data
#' your_own_colnames <-
#'   list(
#'     report_id = "repId",
#'     drug = "drug",
#'     event = "event",
#'     group_by = NULL
#'   )
#'
#' # Rename the report_id variable and run again
#' drug_event_df |>
#'   dplyr::rename(repId = report_id) |>
#'   da(df_colnames = your_own_colnames)
#'
#' ### Run a disproportionality across two subgroups
#' # Create two groups (even/uneven report_ids) in drug_event_df
#' drug_event_df_with_grouping <-
#'   drug_event_df |>
#'   dplyr::mutate("group" = report_id %% 2)
#'
#' # Set the colname for the group_by-parameter to "group"
#' your_own_colnames <-
#'   list(
#'     report_id = "report_id",
#'     drug = "drug",
#'     event = "event",
#'     group_by = "group"
#'   )
#'
#' # Execute da across the subgroups
#' da_2 <-
#'   drug_event_df_with_grouping |>
#'   da(df_colnames = your_own_colnames)
#' @seealso
#'  \code{\link[pvutils]{add_expected_counts}}, \code{\link[pvutils]{add_disproportionality}}
#' @export
#' @importFrom checkmate qassert
#' @importFrom dplyr bind_cols select pull slice
#' @importFrom purrr map list_rbind
da <- function(df = NULL,
               df_colnames = list(
                 report_id = "report_id",
                 drug = "drug",
                 event = "event",
                 group_by = NULL
               ),
               da_estimators = c("ic", "prr", "ror"),
               rule_of_N = 3,
               conf_lvl = 0.95,
               number_of_digits = 2,
               excel_path = NULL) {
  checkmate::qassert(df_colnames$group_by, c("S1", "0"))
  checkmate::qassert(excel_path, c("S1", "0"))

  # ic uses expected counts from rrr
  expected_count_estimators <- gsub("ic", "rrr", da_estimators)

  assign(df_colnames$report_id, NULL)
  assign(df_colnames$drug, NULL)
  assign(df_colnames$event, NULL)

  if (is.null(df_colnames$group_by)) {
    # No subgrouping provided:
    output <- df |>
      pvutils::add_expected_counts(df_colnames, expected_count_estimators = expected_count_estimators) |>
      pvutils::add_disproportionality(
        da_estimators = da_estimators,
        conf_lvl = conf_lvl,
        rule_of_N = rule_of_N,
        number_of_digits = number_of_digits
      )
  } else {
    if (!df_colnames$group_by %in% colnames(df)) {
      stop("Passed grouping column name '", df_colnames$group_by, "' not found in passed df.")
    }
    # When subgroups are provided:
    output <- df |>
      split(f = df[[df_colnames$group_by]]) |>
      purrr::map(grouped_da,
                 df_colnames = df_colnames,
                 group_by = df_colnames$group_by,
                 expected_count_estimators = expected_count_estimators,
                 da_estimators = da_estimators,
                 conf_lvl = conf_lvl,
                 rule_of_N = rule_of_N,
                 number_of_digits = number_of_digits
      ) |>
      purrr::list_rbind()
  }

  write_to_excel(output, excel_path)

  return(invisible(output))
}
# 0.1.1 grouped_da -----
#' @title Disproportionality Analysis by Subgroups
#' @description A package internal wrapper for executing da across subgroups
#' @param df See the da function
#' @param df_colnames See the da function
#' @param group_by For convenience, the df_colnames$group_by is passed as a separate parameter
#' @param expected_count_estimators See the da function
#' @param da_estimators See the da function
#' @param conf_lvl See the da function
#' @param rule_of_N See the da function
#' @param number_of_digits See the da function
#' @return See the da function
#' @details See the da documentation
#' @importFrom rlang sym
#' @importFrom dplyr slice pull bind_cols select
grouped_da <- function(df = NULL,
                       df_colnames = NULL,
                       group_by = NULL,
                       expected_count_estimators = NULL,
                       da_estimators = NULL,
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

  df |>
    pvutils::add_expected_counts(
      df_colnames = df_colnames,
      expected_count_estimators = expected_count_estimators
    ) |>
    pvutils::add_disproportionality(
      da_estimators = da_estimators,
      conf_lvl = conf_lvl,
      rule_of_N = rule_of_N,
      number_of_digits = number_of_digits
    ) |>
    dplyr::bind_cols(!!group_by := current_group) |>
    dplyr::select(!!drug, !!event, !!group_by, everything())
}

# 1.1 add_expected_counts ----
#' @title Produces expected counts
#' @description Produces various counts used in disproportionality analysis.
#' @param df An object possible to convert to a data table, e.g.
#' a tibble or data.frame, containing patient level reported drug-event-pairs.
#' See below for further details.
#' @param df_colnames Provide a list with the column names of the variables to use
#' passed in \code{df}`i.e. point \code{da} to the column with the report_ids
#' (\code{report_id}), the drug names (\code{drug}), the adverse event names
#' (\code{event}) and optionally which subgroups to calculate disproportionality
#' across through \code{group_by}. See the examples.
#' @param expected_count_estimators A character vector containing the desired
#' expected count estimators. Defaults to the implemented options, i.e.
#' c("rrr", "prr", "ror").
#'
#' @section The df object:
#'  The passed \code{df} should be (convertible to) a data table and at least contain three
#'  columns: \code{report_id}, \code{drug} and \code{event}. The data table should contain one row
#'  per reported drug-event-combination, i.e. receiving a single additional report
#'  for drug X and event Y would add one row to the table. If the single report
#'  contained drug X for event Y and event Z, two rows would be added, with the
#'  same \code{report_id} and \code{drug} on both rows. Column \code{report_id} must be of type
#'  numeric or character. Columns \code{drug} and \code{event} must be of type character.
#'  You can use other names for your columns, as long as you specify them in df_colnames.
#'
#' @return A tibble containing the various counts.
#' @importFrom dplyr arrange count distinct everything group_by mutate n_distinct
#' rename select ungroup
#' @import data.table
#' @export
add_expected_counts <- function(df = NULL,
                                df_colnames = NULL,
                                expected_count_estimators = c("rrr", "prr", "ror")) {
  # dtplyr/data.table complains if we don't put obs to NULL. Similar requirements
  # in lower level functions (count_expected_rrr, count_expected_prr,
  # count_expected_ic) called in this function
  obs <- NULL
  checkmate::qassert(df[[df_colnames$report_id]], c("S+", "N+"))
  checkmate::qassert(df[[df_colnames$drug]], "S+")
  checkmate::qassert(df[[df_colnames$event]], "S+")

  df_colnames_wo_group_vec <-
    df |>
    dplyr::select(-df_colnames$group_by) |>
    colnames()

  if (!any(utils::hasName(df, df_colnames_wo_group_vec))) {
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
  count_dt <- count_expected_rrr(df_colnames, df)

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
    dplyr::arrange(desc_obs_order) |>
    tibble::as_tibble()

  return(count_df)
}

# 1.2 add_disproportionality ----
#' @title Add disproportionality estimates to data frame
#' with expected counts
#' @inheritParams ror
#' @param df Intended use is on the output tibble from \code{add_expected_counts}.
#' @param da_estimators Character vector specifying which disproportionality
#' estimators to use, in case you don't need all implemented options. Defaults
#' to c("ic", "prr", "ror").
#' @param rule_of_N Numeric value. Sets estimates for ROR and PRR to NA when observed
#' counts are strictly less than the passed value of \code{rule_of_N}. Default value
#' is 3, 5 is sometimes used as a more liberal alternative. Set to NULL if you
#' don't want to apply any such rule.
#' @param number_of_digits Numeric value. Set the number of digits to show in output by passing
#' an integer. Default value is 2 digits. Set to NULL to avoid rounding.
#' @return The passed data frame with disproportionality point and interval
#' estimates.
#' @export
add_disproportionality <- function(df = NULL,
                                   da_estimators = c("ic", "prr", "ror"),
                                   rule_of_N = 3,
                                   number_of_digits = 2,
                                   conf_lvl = 0.95) {
  if (!all(da_estimators %in% c("ic", "prr", "ror"))) {
    stop("Passed parameter 'da_estimators' must consist of 'ic', 'prr' or 'ror'")
  }
  checkmate::qassert(rule_of_N, c("N1[0,]", "0"))
  # The round, using number_of_digits below, rounds decimals in digits, so we
  # can pass this on without further checks
  checkmate::qassert(number_of_digits, c("N1[0,]", "0"))

  da_df <- df

  if ("ic" %in% da_estimators) {
    ic_df <- pvutils::ic(da_df$obs, da_df$exp_rrr)
    da_df <- da_df |> dplyr::bind_cols(ic_df)
  }

  if ("prr" %in% da_estimators) {
    prr_df <- pvutils::prr(
      obs = da_df$obs,
      n_drug = da_df$n_drug,
      n_event_prr = da_df$n_event_prr,
      n_tot_prr = da_df$n_tot_prr
    )

    da_df <- da_df |> dplyr::bind_cols(prr_df)
  }

  if ("ror" %in% da_estimators) {
    ror_df <- pvutils::ror(
      a = da_df$obs,
      b = da_df$b,
      c = da_df$c,
      d = da_df$d
    )

    da_df <- da_df |> dplyr::bind_cols(ror_df)
  }

  # Do some clean up
    da_df <-
      da_df |>
      apply_rule_of_N(da_estimators, rule_of_N) |>
      round_columns_with_many_decimals(da_estimators, number_of_digits)
}
  # See lower_level_disprop_analysis.R for further details ----
