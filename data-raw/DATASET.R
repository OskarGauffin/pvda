## code to prepare `drug_event_df` dataset goes here

set.seed(1)
report_ids <- tibble::tibble("report_id" = 1:1000)
drug_names <- stringr::str_c("Drug_", toupper(letters))
event_names <- stringr::str_c("Event_", 1:1000)

generate_data <- function(report_id){

  n_drugs <- rpois(1, 3) + 1
  drugs <- sample(drug_names, n_drugs, replace=T, prob=cumsum(1:26/26))
  events <- sample(event_names, n_drugs, replace=F, prob = pmax(cumprod(rep(0.95, 1000)), 1/1000))

  tibble::tibble(report_id = report_id$report_id,
                 drug = drugs,
                 event = events)
  }

drug_event_df <- report_ids |> split(as.factor(report_ids$report_id)) |> purrr::map_dfr(generate_data)


usethis::use_data(drug_event_df, overwrite = TRUE)
drug_event_df

• Document your data (see 'https://r-pkgs.org/data.html')

