# ---------------------------------------------------------------------------- #
# Headers below are function names, in foldable code sections,
# indexed according to their calling hierarchy.
# Collapse All with "Alt + O"
# And expand All with "Shift + Alt + O"
# ---------------------------------------------------------------------------- #

# 0.1 da ----
#' @title Disproportionality Analysis
#' @description The function \code{da} executes disproportionality analyses,
#' i.e. compares the proportion of reports with a specific adverse event for a drug,
#' against corresponding event proportion among all other drugs in the passed data frame.
#' See the vignette for a brief introduction to disproportionality analysis.
#' Furthermore, \code{da} supports three estimators: Information Component (IC),
#' Proportional Reporting Rate (PRR) and the Reporting Odds Ratio (ROR).
#' @inheritParams add_expected_counts
#' @inheritParams add_disproportionality
#' @inheritParams ror
#' @param number_of_digits Round decimal columns to specified precision, default is two decimals.
#' @param excel_path To write the output of \code{da} to an excel file, provide a path
#' to a folder e.g. to write to your current working directory, pass \code{getwd()}.
#'  The excel file will by default be named \code{da.xlsx}. To control the excel file name,
#'  pass a path ending with the desired filename suffixed with \code{.xlsx}. If you
#'  do not want to export the output to an excel file, pass NULL (the default).
#' @param sort_by The output is sorted in descending order of the lower bound of
#'  the confidence/credibility interval for a da estimator, for each drug-event
#'  combination. If a grouping variable is passed, the sample average is taken
#'  across each drug-event-combination (ignoring NAs). Any of the passed strings
#'  in "da_estimators" is accepted, the default is "ic".
#' @inheritSection add_expected_counts The df object
#' @return \code{da} returns a data frame (invisibly) containing counts and
#' estimates related to supported disproportionality estimators. Each row
#' corresponds to a drug-event pair.
#' @examples
#' ### Run a disproportionality analysis
#' da_1 <-
#'   drug_event_df |>
#'   da()
#'
#' ### Run a disproportionality across subgroups, and "camelCase" column names
#' list_of_colnames <-
#'   list(
#'     report_id = "report_id",
#'     drug = "drug",
#'     event = "event",
#'     group_by = "group"
#'   )
#'
#' # Execute da across subgroups
#' da_2 <-
#'   drug_event_df |>
#'   da(df_colnames = list_of_colnames)
#'
#' # If e.g. the column with reportIDs had a different name than the default,
#' # you can specify the right column name as follows:
#'
#' renamed_df <-
#' drug_event_df |>
#' dplyr::rename(ReportID = report_id)
#'
#' list_of_colnames$report_id = "ReportID"
#'
#' da_3 <-
#' renamed_df |>
#' da(df_colnames = list_of_colnames)
#'
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
               sort_by = "ic",
               number_of_digits = 2,
               rule_of_N = 3,
               conf_lvl = 0.95,
               excel_path = NULL) {

  checkmate::qassert(df_colnames$group_by, c("S1", "0"))
  checkmate::qassert(excel_path, c("S1", "0"))
  df_syms <- lapply(df_colnames, \(x){if(!is.null(x)){rlang::sym(x)}})

  # ic uses expected counts from rrr
  expected_count_estimators <- gsub("ic", "rrr", da_estimators)

  NULL -> obs -> exp_rrr -> exp_prr -> exp_ror
  assign(df_colnames$report_id, NULL)
  assign(df_colnames$drug, NULL)
  assign(df_colnames$event, NULL)

  if (is.null(df_colnames$group_by)) {
    # No subgrouping provided:
    output <- df |>
      pvutils::add_expected_counts(
        df_colnames = df_colnames,
        df_syms = df_syms,
        expected_count_estimators = expected_count_estimators
      ) |>
      pvutils::add_disproportionality(
        df_syms = df_syms,
        da_estimators = da_estimators,
        conf_lvl = conf_lvl,
        rule_of_N = rule_of_N
      ) |>
      round_and_sort_by_lower_da_limit(df_colnames = df_colnames,
                             df_syms = df_syms,
                             conf_lvl = conf_lvl,
                             sort_by = sort_by,
                             da_estimators = da_estimators,
                             number_of_digits = number_of_digits)
  } else {
    if (!df_colnames$group_by %in% colnames(df)) {
      stop("Passed grouping column name '", df_colnames$group_by, "' not found in passed df.")
    }

    drug <- rlang::sym(df_colnames$drug)
    event <- rlang::sym(df_colnames$event)
    group_by <- rlang::sym(df_colnames$group_by)


    # When subgroups are provided:
    output <- df |>
      split(f = df[[df_colnames$group_by]]) |>
      purrr::map(grouped_da,
                 df_colnames = df_colnames,
                 df_syms = df_syms,
                 expected_count_estimators = expected_count_estimators,
                 da_estimators = da_estimators,
                 conf_lvl = conf_lvl,
                 rule_of_N = rule_of_N,
                 number_of_digits = number_of_digits
      ) |>
      purrr::list_rbind() |>
      dplyr::select(
        !!drug,
        !!event,
        !!group_by,
        obs,
        exp_rrr,
        dplyr::starts_with("ic"),
        exp_prr,
        dplyr::starts_with("prr"),
        exp_ror,
        dplyr::starts_with("ror"),
        everything()
      ) |>
      round_and_sort_by_lower_da_limit(df_colnames = df_colnames,
                             df_syms = df_syms,
                             conf_lvl = conf_lvl,
                             sort_by = sort_by,
                             da_estimators = da_estimators,
                             number_of_digits = number_of_digits)
  }

  write_to_excel(output, excel_path)
  class(output) = c("da", "tbl_df", "tbl", "data.frame")

  return(invisible(output))
}
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
    pvutils::add_expected_counts(
      df_colnames = df_colnames,
      df_syms = df_syms,
      expected_count_estimators = expected_count_estimators
    ) |>
    pvutils::add_disproportionality(
      df_syms = df_syms,
      da_estimators = da_estimators,
      conf_lvl = conf_lvl,
      rule_of_N = rule_of_N
    ) |>
    dplyr::bind_cols(!!group_by := current_group)

  return(output)
}

# 0.1.2 summary.da ----

#' @title Summary function for disproportionality objects
#' @description Provides summary counts of SDRs and shows the top five DECs
#' @param object The object outputted from \code{da}. Extended from the tibble object.
#' @param ... For passing additional parameters to extended classes.
#' @return Nothing
#' @export
#' @importFrom stringr str_subset regex str_which str_replace
#' @importFrom tidyr separate_wider_delim unite
#' @importFrom dplyr group_by summarise pull mutate count distinct slice_head across all_of select
#' @importFrom rlang sym
#' @importFrom tibble tibble
#' @importFrom cli col_blue col_grey
#' @importFrom stats setNames

summary.da <- function(object, ...){

  NULL -> drug -> event -> da_estimator -> exp_ror -> exp_prr ->
    conf_lvl_digits -> lower_bound_da

  da_estimators = c("ic", "prr", "ror")
  N_da_estimators = length(da_estimators)

  # Get the names of the column with lower bounds
  lower_bound_df_from_da <- function(df){

    df |>
    colnames() |>
    sort() |>
    stringr::str_subset(paste0(paste0(da_estimators, "[:digit:]"), collapse="|")) |>
    as.data.frame() |>
    setNames("col") |>
    tidyr::separate_wider_delim(cols=col, delim = stringr::regex("(?<=[a-zA-Z])\\s*(?=[0-9])"),
                                names = c("da_estimator", "conf_lvl_digits")) |>
    dplyr::group_by(da_estimator) |>
    dplyr::summarise(min = min(conf_lvl_digits))
  }

  # Extract the lower bound column names, split into two cols
  lower_bound_df <- lower_bound_df_from_da(object)

  # Extract the conf_lvl
    conf_lvl <- lower_bound_df |>
      dplyr::pull(min) |>
      as.numeric() |>
      (\(x){x[1]*2/100})()

  # Put the lower bound colnames back together
    lower_bound_col_names_wo_exp <-
    lower_bound_df |>
    tidyr::unite(col=lower_bound_da, da_estimator, min, sep="") |>
    dplyr::pull()

    # Take exp on the ic bound gives the same threshold value for all estimators
    ic_index <- stringr::str_which(lower_bound_col_names_wo_exp, "ic")
    ic_col <- lower_bound_col_names_wo_exp[ic_index]
    ic_col_sym <- rlang::sym(ic_col)
    ic_exp_col <- paste0("2^(", lower_bound_col_names_wo_exp[ic_index], ")", collapse="")
    ic_exp_sym <- rlang::sym(ic_exp_col)

    # Transform lower_ic_bound to O/E-scale, count DECs exceeding threshold = 1
    lower_bound_col_names <-
      lower_bound_col_names_wo_exp |>
      stringr::str_replace(ic_col, ic_exp_col)

    da_summary_counts <-
    object |>
    dplyr::mutate(!!ic_exp_sym := 2^(!!ic_col_sym)) |>
    dplyr::summarise(across(all_of(lower_bound_col_names), \(x){
      sum(1 < x, na.rm = TRUE)
    }))

    # Put together count table
    output <- tibble::tibble(colnames(da_summary_counts),da_summary_counts[1,] |>
      as.character()) |>
      setNames(c("da", "N")) |>
      dplyr::mutate(da = paste0(da, " ")) |>
      setNames(c("0 <   ", "N"))

    summary_df <- output

    # Count total N of DECs in object
    N_DECs <-
      object |>
      dplyr::count() |>
      as.numeric()

    # We just print drug, event, exp_rrr and da estimates from obj
    # Check if a grouping variable has been passed
    # ("are any drug-event-combinations occurring more than once?")
    drug <- rlang::sym(colnames(object)[1])
    event <- rlang::sym(colnames(object)[2])
    n_distinct_rows <- object |> dplyr::distinct(!!drug, !!event) |> dplyr::count()

    end_of_print_df = 3 + 4*N_da_estimators
    if(nrow(object) > n_distinct_rows){
      #  extra col if group_by was passed
      end_of_print_df = end_of_print_df + 1
    }

    top_five_rows <-
      object |>
      dplyr::slice_head(n=5) |>
      as.data.frame() |>
      dplyr::mutate(dplyr::across(dplyr::all_of(lower_bound_col_names_wo_exp), \(x){paste0("       ", x)}))

    top_five_rows <- top_five_rows[,1:end_of_print_df]
    top_five_rows <- top_five_rows |> dplyr::select(-exp_prr, -exp_ror)

    cat(cli::col_blue("Summary of Disproportionality Analysis \n\n"))
    cat(cli::col_blue("Number of SDRs (out of total ", N_DECs, " DECs) \n"))
    print(as.data.frame(output), row.names=FALSE)

    cat("\n")
    cat(cli::col_blue("Top 5 DECs \n"))
    print(top_five_rows, row.names = FALSE)
    cat("\n")
    cat(cli::col_grey("(sorted according to passed argument sort_by)"))

    return(invisible(summary_df))
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
    ic_df <- pvutils::ic(obs = df$obs,
                         exp = df$exp_rrr,
                         conf_lvl = conf_lvl)
    df <-
      df |>
      dplyr::bind_cols(ic_df)
  }

  if ("prr" %in% da_estimators) {
    prr_df <- pvutils::prr(
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
    ror_df <- pvutils::ror(
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
      everything())

}
# See lower_level_disprop_analysis.R for further details ----
