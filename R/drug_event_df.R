#' A simulated ICSR database
#'
#' drug_event_df contains report_ids with drugs and events. You can use these
#' with the package tools to arrive at for instance observed counts and
#' disproportionality analysis-estimates.
#'
#' @format `drug_event_df`
#' A data frame with 3,971 rows and 3 columns. In total 1000 unique report_ids,
#' i.e. the same report_id can have several drugs and events.
#'
#' Number of drugs per report_id is sampled as 1 + Pois(3), with increasing
#' probability as the drug letter closes in on Z. Every drug is assigned
#' an event, with decreasing probability as the event index number increases
#' towards 1000. See the DATASET.R file in the data-raw folder for details.
#'
#' \describe{
#'   \item{report_id}{A patient or report identifier}
#'   \item{drug}{One of 26 fake drugs (Drug_A - Drug_Z)}
#'   \item{event}{Sampled events (Event_1 - Event_1000)}
#' }
#' @source Simulated data.
"drug_event_df"
