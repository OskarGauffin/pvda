#
# library("dplyr")
#
# clean_vaers <- vaers::vaers_symptoms |>
#   select(everything(), -starts_with("SYMPTOMVERSION")) |>
#   tidyr::pivot_longer(cols = starts_with("SYMPTOM"),
#                       names_to = "event_nr",
#                       values_to = "event_name",
#                       values_drop_na = TRUE) |>
#   select(-event_nr) |> left_join(
#
#     vaers::vaers_vax |> select(VAERS_ID, VAX_NAME)) |>
#   select(report_id = VAERS_ID, drug = VAX_NAME, event = event_name) |>
#   as_tibble()
#
# # data.table::fwrite(clean_vaers, "clean_vaers.csv")
#
# tictoc::tic()
# clean_counts <- clean_vaers |> pvutils::add_expected_counts() |> pvutils::add_disproportionality()
# tictoc::toc()
# A
