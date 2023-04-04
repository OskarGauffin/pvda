## code to prepare `drug_event_df` dataset goes here

simulate_dataset <- function(N = 1000) {

  # N = 1000
  set.seed(10)
  report_ids <- tibble::tibble("report_id" = 1:N)
  drug_names <- stringr::str_c("Drug_", toupper(letters))
  event_names <- stringr::str_c("Event_", 1:1000)

  sample_drugs_and_events <- function(report_id) {
    n_drugs <- rpois(1, 3) + 1
    drugs <- sample(drug_names, n_drugs, replace = T, prob = rev(cumsum(1:26 / 26)))
    events <- sample(event_names, n_drugs, replace = F, prob = pmax(cumprod(rep(0.95, 1000)), 1 / 1000))

    tibble::tibble(
      report_id = report_id$report_id,
      drug = drugs,
      event = events
    )
  }

  drug_event_df <- report_ids |>
    split(as.factor(report_ids$report_id)) |>
    purrr::map_dfr(sample_drugs_and_events)
}

# Run function and save using usethis
drug_event_df <- simulate_dataset(N = 10^3) |> dplyr::arrange(report_id, drug, event)

# Add some SDRs
sdr_df <- data.frame("report_id"=sample(drug_event_df$report_id, size=500, replace=T),
                     "drug" = sample(c("Drug_Z","Drug_U", "Drug_V"), size=500, replace=T),
                     "event"=sample(c("Event_1", "Event_2"), size=500, replace=T))

drug_event_df <- drug_event_df |> dplyr::bind_rows(sdr_df)

# summary.da(drug_event_df |> pvutils::da())

# Add groups
drug_event_df$group = drug_event_df$report_id %% 2

usethis::use_data(drug_event_df, overwrite = TRUE)
