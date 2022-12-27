test_that("IC works", {

  test_df <- ic(1, 1)

  expected_output <- tibble::tribble(
    ~"lower bound", ~"ic", ~"upper bound",
    log2(qgamma(0.025, 1.5, 1.5)),
    0,
    log2(qgamma(0.975, 1.5, 1.5))
  )

  expect_equal(test_df[, 1], expected_output[, 1])
  expect_equal(test_df[, 2], expected_output[, 2])
  expect_equal(test_df[, 3], expected_output[, 3])
})
