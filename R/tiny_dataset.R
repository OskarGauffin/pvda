#' A 110 reports big, simulated ICSR database
#'
#' tiny_dataset is used to demonstrate the package in examples. The larger
#' drug_event_df-dataset, also included in the package, took too long time
#' to run at, to be allowed at CRAN.
#'
#' @format `tiny_dataset`
#' A data frame with 110 rows and 3 columns. In total 110 unique report_ids.
#' In particular, for Drug A and Event 1 the observed count will be 4 and
#' exp_rrr = 1.1
#'
#' \describe{
#'   \item{report_id}{A report identifier, 1-110. }
#'   \item{drug}{Drugs named as Drug_A - Drug_Z.}
#'   \item{event}{Events named as Event_1 - Event_97)}
#'   \item{group}{In this example, sex of the patient, i.e. Male or Female.}
#' }
#' @source Simulated data.
"tiny_dataset"
