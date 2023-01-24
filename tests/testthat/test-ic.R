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


test_that("Function count_expected works", {

  produced_output <- count_expected(drug_event_df,
                                    da_estimators = c("rrr", "prr", "ror"))

  # Should return as many rows
  expect_equal(nrow(produced_output),
               nrow(dplyr::distinct(drug_event_df[,c("drug", "event")], )))

  first_row <- produced_output[1,]

  # Some internal checks that the counts agree for at least one line
  expect_equal(sum(first_row[,c("obs", "b", "c", "d")]), first_row$n_tot)
  expect_equal(sum(first_row[,c("obs", "b")]), first_row$n_drug)
  expect_equal(sum(first_row[,c("obs", "c")]), first_row$n_event)
  expect_equal(first_row$n_event_prr, first_row$c)
  expect_equal(first_row$n_tot_prr, diff(as.numeric(first_row[, c("n_drug", "n_tot")])))
})


