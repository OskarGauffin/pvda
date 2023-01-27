test_that("Function ic works", {
  test_df <- ic(1, 1)

  expected_output <- tibble::tribble(
    ~"ic_lower", ~"ic", ~"ic_upper",
    log2(qgamma(0.025, 1.5, 1.5)),
    0,
    log2(qgamma(0.975, 1.5, 1.5))
  )

  expect_equal(test_df[, 1], expected_output[, 1])
  expect_equal(test_df[, 2], expected_output[, 2])
  expect_equal(test_df[, 3], expected_output[, 3])
})

test_that("Function ror works", {
  test_df <- ror(1:2, 2:3, 3:4, 4:5)

  expected_output <- tibble::tribble(
    ~"ror_lower", ~"ror", ~"ror_upper",
    NA, 1 * 4 / (2 * 3), NA,
    NA, 2 * 5 / (3 * 4), NA
  )

  # expect_equal(test_df[, 1], expected_output[, 1])
  expect_equal(test_df[, 2], expected_output[, 2])
  # expect_equal(test_df[, 3], expected_output[, 3])
})


test_that("Function add_expected_count works", {

  produced_output <- pvutils::add_expected_counts(drug_event_df,
                                    da_estimators = c("rrr", "prr", "ror"))

  # Should return as many rows as there are unique report_ids in drug_event_df
  # (N=1000)
  expect_equal(nrow(produced_output),
               nrow(dplyr::distinct(drug_event_df[, c("drug", "event")], )))

  # Some internal checks that the counts agree for at least one line
  first_row <- produced_output[1,]
  expect_equal(sum(first_row[,c("obs", "b", "c", "d")]), first_row$n_tot)
  expect_equal(sum(first_row[,c("obs", "b")]), first_row$n_drug)
  expect_equal(sum(first_row[,c("obs", "c")]), first_row$n_event)
  expect_equal(first_row$n_event_prr, first_row$c)
  expect_equal(first_row$n_tot_prr, diff(as.numeric(first_row[, c("n_drug", "n_tot")])))
})

test_that("The whole disproportionality function chain runs without NA output except in ROR", {

  output <- drug_event_df |>
    add_expected_counts() |>
    add_disprop_est() |>
    dplyr::select(-dplyr::starts_with("ror"))

  expect_equal(FALSE, any(is.na(output)))
})
