# ---------------------------------------------------------------------------- #
# Function names are organized by headers in foldable code sections,
# structured according to their position in the code.
# Collapse All with "Alt+O"
# And expand All with "Shift+Alt+O"
# ---------------------------------------------------------------------------- #

# 0.1 da ----
#' @title Disproportionality Analysis
#' @description Execute a disproportionality analysis
#' @inheritParams add_expected_counts
#' @inheritParams add_disproportionality
#' @param group_by Provide a string with the name of a grouping variable in `df`
#'  to perform subgroup analyses (i.e. run disproportionality analysis within each group).
#'  Passing NULL, the default, uses all data in df as a single group.
#' @param write_path If you don't want to write the output to an excel file,
#'  pass the default value, NULL. To write to excel, provide a path to a folder
#'  e.g. to write to your current working directory, pass `getwd()`.
#'  The excel file will by default be named `da.xlsx`. To change the excel file name,
#'  pass a path ending with the specific filename suffixed with `.xlsx`.
#' @param ... Pass additional objects, e.g. count_expected_estimators,
#' da_estimators, or sign_lvl documented in lower level functions.
#' @return A dataframe containing counts and estimates related to
#' supported disproportionality estimators.
#' @examples
#' da_1 <- drug_event_df |> pvutils::da()
#'
#' # Split df into two groups (even/uneven report_ids)
#' drug_event_df_with_grouping  <- drug_event_df |>
#' dplyr::mutate("group" = report_id %% 2)
#' da_1 <- drug_event_df_with_grouping |> pvutils::da(group_by = "group")
#' @seealso
#'  \code{\link[checkmate]{qassert}}
#'  \code{\link[pvutils]{add_expected_counts}}, \code{\link[pvutils]{add_disproportionality}}
#'  \code{\link[dplyr]{bind_cols}}, \code{\link[dplyr]{select}}
#'  \code{\link[purrr]{map}}, \code{\link[purrr]{list_c}}
#' @rdname da
#' @export
#' @importFrom checkmate qassert
#' @importFrom dplyr bind_cols select pull slice
#' @importFrom purrr map list_rbind
da <- function(df, da_estimators = c("ic", "prr", "ror"), group_by = NULL, write_path = NULL, ...) {

    checkmate::qassert(group_by, c("S1", "0"))

    # Put a dot on the actual parameters, to not confuse param names and parameters
    .da_estimators <- da_estimators
    .expected_count_estimators = gsub("ic", "rrr", da_estimators)

    if(is.null(group_by)){
      # No subgrouping provided:
      output <- df |>
        pvutils::add_expected_counts(expected_count_estimators = .expected_count_estimators) |>
        pvutils::add_disproportionality(da_estimators = .da_estimators)
    } else {
      if(! group_by %in% colnames(df)){
        stop("Passed grouping column name '", group_by, "' not found in passed df.")
      }
      # When subgroups are provided:

    grouped_da <- function(df,
                           .group_by,
                           .expected_count_estimators = c("prr", "ror", "rrr"),
                           .da_estimators = c("ic", "prr", "ror")){

      NULL -> drug -> event -> group

      current_group <- df |>
        dplyr::slice(1) |>
        dplyr::pull(!!group_by)

      df |>
      pvutils::add_expected_counts(expected_count_estimators = .expected_count_estimators) |>
      pvutils::add_disproportionality(da_estimators = .da_estimators) |>
      # Ideally, group-variable should be preserved throughout the toolchain
      dplyr::bind_cols("group" = current_group) |>
      dplyr::select(drug, event, group, everything())
    }

    output <- df |>
    split(f = df[[group_by]]) |>
    purrr::map(grouped_da, .group_by = group_by) |>
    purrr::list_rbind()
    }

  write_to_excel(output, write_path)

  return(invisible(output))
}

# da <- pvutils::drug_event_df |> pvutils::da(write_path = getwd())

# 1.1 add_expected_counts ----
#' @title Calculate counts required for expected counts, and expected counts
#' @description Produces various counts used in disproportionality analysis.
#' @param df A data table, or an object possible to convert to a data table, e.g.
#' a tibble or data.frame. For column specifications, see details.
#' @param expected_count_estimators A character vector containing the desired
#' expected count estimators. Defaults to all possible options, i.e.
#' c("rrr", "prr", "ror").
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
                                expected_count_estimators = c("rrr", "prr", "ror")) {
  # data.table complains if we don't put obs to NULL. Similar requirements
  # in lower level functions called in this function
  obs <- NULL

  checkmate::qassert(df[[1]], c("S+", "N+"))
  checkmate::qassert(df[[2]], "S+")
  checkmate::qassert(df[[3]], "S+")

  if (!any(utils::hasName(df, c("report_id", "drug", "event")))) {
    stop("At least one of column names 'report_id', 'drug' and 'event' is not
         found. Please check the passed object.")
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
  count_dt <- count_expected_rrr(df)

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

  count_df <- count_dt |>
    dplyr::arrange(dplyr::desc(obs)) |>
    tibble::as_tibble()

  return(count_df)
}

# 1.2 add_disproportionality ----
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


# See lower_level_disprop_analysis.R for further details
