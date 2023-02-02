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
