# 1.0 constructor da ----
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
#'  the confidence/credibility interval for a passed da estimator. Any of the passed strings
#'  in "da_estimators" is accepted, the default is "ic".
#'  If a grouping variable is passed, sorting is made by the sample average across each drug-event-combination (ignoring NAs).
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
#' ### Run a disproportionality across subgroups
#' list_of_colnames <-
#'   list(
#'     report_id = "report_id",
#'     drug = "drug",
#'     event = "event",
#'     group_by = "group"
#'   )
#'
#' da_2 <-
#'   drug_event_df |>
#'   da(df_colnames = list_of_colnames)
#'
#' # If columns in your df have different names than the default ones,
#' # you can specify the column names in the df_colnames parameter list:
#'
#' renamed_df <-
#'   drug_event_df |>
#'   dplyr::rename(ReportID = report_id)
#'
#' list_of_colnames$report_id <- "ReportID"
#'
#' da_3 <-
#'   renamed_df |>
#'   da(df_colnames = list_of_colnames)
#'
#' @export
#' @importFrom checkmate qassert
#' @importFrom dplyr bind_cols select pull slice
#' @importFrom purrr map list_rbind %||%
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

  # If null, set the default values of these parameters
  df_colnames[['report_id']] <- df_colnames[['report_id']] %||% "report_id"
  df_colnames[['drug']] <- df_colnames[['drug']] %||% "drug"
  df_colnames[['event']] <- df_colnames[['event']] %||% "event"

  input_params <- list(
    df = df,
    df_colnames = df_colnames,
    da_estimators = da_estimators,
    sort_by = sort_by,
    number_of_digits = number_of_digits,
    rule_of_N = rule_of_N,
    conf_lvl = conf_lvl,
    excel_path = excel_path
  )

  checkmate::qassert(df_colnames$group_by, c("S1", "N1", "0"))
  checkmate::qassert(excel_path, c("S1", "0"))
  df_syms <- lapply(df_colnames, \(x){
    if (!is.null(x)) {
      rlang::sym(x)
    }
  })

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
      round_and_sort_by_lower_da_limit(
        df_colnames = df_colnames,
        df_syms = df_syms,
        conf_lvl = conf_lvl,
        sort_by = sort_by,
        da_estimators = da_estimators,
        number_of_digits = number_of_digits
      )
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
      round_and_sort_by_lower_da_limit(
        df_colnames = df_colnames,
        df_syms = df_syms,
        conf_lvl = conf_lvl,
        sort_by = sort_by,
        da_estimators = da_estimators,
        number_of_digits = number_of_digits
      )
  }

  # If excel path is non-NULL
  write_to_excel(output, excel_path)

  output_list <- list(
    "da_df" = output,
    "input_params" = input_params
  )
  class(output_list) <- "da"

  return(invisible(output_list))
}

# 1.1 summary.da ----

#' @title Summary function for disproportionality objects
#' @description Provides summary counts of SDRs and shows the top five DECs
#' @param object A S3 obj of class "da", output from \code{pvutils::da()}.
#' @param ... For passing additional parameters to extended classes.
#' @return Passes a tibble with the SDR counts invisibly.
#' @export
#' @importFrom stringr str_subset regex str_which str_replace
#' @importFrom tidyr separate_wider_delim unite
#' @importFrom dplyr group_by summarise pull mutate count distinct slice_head across all_of select
#' @importFrom purrr pluck
#' @importFrom rlang sym
#' @importFrom tibble tibble
#' @importFrom cli col_blue col_grey
#' @importFrom stats setNames
summary.da <- function(object, ...) {
  # object <- drug_event_df |> da()

  NULL -> drug -> event -> da_estimator -> exp_ror -> exp_prr ->
    conf_lvl_digits -> lower_bound_da

  input_params <- object$input_params
  conf_lvl <- input_params$conf_lvl
  da_estimators <- input_params$da_estimators

  quant_prob_list <-
    conf_lvl |>
    conf_lvl_to_quantile_prob()

  lower_bound_column_names <- split(da_estimators, da_estimators) |>
    lapply(\(x){
      as.character(colnames_da(quant_prob_list, x)[[1]])
    }) |>
    purrr::list_c()

  # exp on the ic bound to have the same threshold value across all estimators
  ic_index <- stringr::str_which(lower_bound_column_names, "ic")
  ic_col <- lower_bound_column_names[ic_index]
  ic_col_sym <- rlang::sym(ic_col)
  ic_exp_col <- lower_bound_column_names[ic_index]
  ic_exp_sym <- rlang::sym(ic_exp_col)

  lower_bound_column_names_w_2 <-
    lower_bound_column_names |>
    stringr::str_replace(ic_col, ic_exp_col)

  da_summary_counts <-
    object |>
    purrr::pluck("da_df") |>
    dplyr::mutate(!!ic_exp_sym := 2^(!!ic_col_sym)) |>
    dplyr::summarise(across(all_of(lower_bound_column_names_w_2), \(x){
      sum(1 < x, na.rm = TRUE)
    }))

  # Put together an overview count table

  # Count total N of DECs in object
  N_DECs <-
    object$da_df |>
    dplyr::count() |>
    as.numeric()
  total_df <- tibble::tibble("Name" = "Total DEC count", "Stat sign" = N_DECs)


  output <- tibble::tibble(
    "Name" = colnames(da_summary_counts),
    "Stat sign" = da_summary_counts[1, ] |>
      as.numeric()
  ) |>
    dplyr::bind_rows(total_df)


  # The number of printed columns need to depend on da_estimators, in case
  # not all da estimators are used
  N_da_estimators <- length(da_estimators)
  end_of_print_df <- 3 + 4 * N_da_estimators

  # Add an extra column if a group_by-column was passed
  add_extra_col <- !is.null(input_params$df_colnames$group_by)
  if (add_extra_col) {
    end_of_print_df <- end_of_print_df + 1
  }

  # top_five_rows <-
  #   object |>
  #   purrr::pluck("da_df") |>
  #   dplyr::slice_head(n=5) |>
  #   as.data.frame() |>
  #   dplyr::mutate(dplyr::across(dplyr::all_of(lower_bound_column_names),
  #                               \(x){paste0("       ", x)}))
  #
  # top_five_rows <- top_five_rows[, 1:end_of_print_df]
  # top_five_rows <- top_five_rows |> dplyr::select(-exp_prr, -exp_ror)

  message(cli::col_blue("Summary of Disproportionality Analysis \nNumber of SDRs \n  "))
  print(output, row.names = FALSE)

  message(c("\n", cli::col_blue(paste0("Top disproportionate DECs"))))
  print(object, ...)

  return(invisible(output))
}

# 1.2 print.da ----
#' print function for da objects
#' @param x A S3 obj of class "da", output from \code{pvutils::da()}.
#' @param n Control the number of rows to print.
#' @inheritParams summary.da
#' @return Nothing, but prints the tibble da_df in the da object.
#' @export
#' @examples
#' da_1 <- drug_event_df |> da()
#' print(da_1)
#' @importFrom purrr flatten
#' @importFrom stringr str_detect
print.da <- function(x, n=10, ...) {
  # x <- drug_event_df |> da()
  # Prevent pillar package from printing negative numbers in red
  old_option <- getOption("pillar.neg")
  options(pillar.neg = FALSE)
  on.exit(options("pillar.neg" = old_option))

  # The following code could benefit from using the OO of the pillar package, but
  # this is not priority right now.
  quantile_prob_list <- x$input_params$conf_lvl |>
    conf_lvl_to_quantile_prob()

  column_group <- list()
  da_estimators <- x$input_params$da_estimators
  exp_estimators <- da_estimators |> stringr::str_replace("ic", "rrr")

  for (i in seq_along(da_estimators)) {
    column_group[[i]] <- colnames_da(quantile_prob_list, da_name = da_estimators[i])
  }

  # This should only be done when the last column to colour is present, otherwise
  # glue_data will throw an error.
  printed_by_default_tbl <- utils::capture.output(print(x$da_df, n = n, ...))

  column_names <- column_group |> purrr::flatten()
  end_column_names <- column_names[c(2,4,6)] |> as.character()

  all_end_column_names_present <-
    all(stringr::str_detect(printed_by_default_tbl[[2]], end_column_names))

  if(all_end_column_names_present){

    for (i in seq_along(column_group)) {
      # i = 1
      cg_i <- column_group[[i]]
      col <- c("red", "green", "magenta")[i]

      start_colour_here <- suppressWarnings(stringr::fixed(paste0("exp_", exp_estimators)))
      start_with_colour_inserted <- suppressWarnings(stringr::fixed(paste0("{", col, " {start_colour_here[i]}")))

      printed_by_default_tbl[2] <- stringr::str_replace(
        printed_by_default_tbl[2],
        start_colour_here[i],
        start_with_colour_inserted
      )

      printed_by_default_tbl[2] <- stringr::str_replace(
        printed_by_default_tbl[2],
        suppressWarnings(stringr::fixed(cg_i$upper)),
        stringr::fixed("{cg_i$upper}}")
      )

      # The insertion of the color codes shifts the da estimator column header
      #  slightly, so we adjust:
      add_this_number_of_spaces <- c(0, rep(1, length(da_estimators) - 1))

      printed_by_default_tbl[2] <- stringr::str_replace(
        printed_by_default_tbl[2],
        stringr::fixed(paste0(da_estimators[i], add_this_number_of_spaces[i], collapse = "")),
        stringr::fixed(da_estimators[i])
      )
    }
  } else {
    message("Your console window is too narrow to support coloured prinouts, reverting to non-coloured output.")
  }

  # Second and last two rows should be in grey
  last_two_rows <- length(printed_by_default_tbl) - 1

  # This needs to be in coarses pieces

  for (i in seq_along(printed_by_default_tbl)) {

    # print(glue::glue_col(printed_by_default_tbl))

    if (!i %in% c(1, 3, last_two_rows:(last_two_rows + 1))) {
      print(glue::glue_col(printed_by_default_tbl[i]))

    } else {
      message(cli::col_grey(printed_by_default_tbl[i]))
    }
  }

  # Print a helpful message about the sorting at the end
  sorted_by_arg <- x$input_params$sort_by
  sorted_by_message <- paste0(
    "(Drug-event-combinations sorted according to lower bound of ", sorted_by_arg, ")")

  message(cli::col_grey(paste0(sorted_by_message, " \n", "# Printing x[['da_df']]")))
}
