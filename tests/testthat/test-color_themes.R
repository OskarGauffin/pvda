test_that("color_themes are working", {
  all_colours_match <- all(
    "#253962" == pvutils:::custom_colours("blue"),
    "#69B4A7" == pvutils:::custom_colours("green")
  )

  expect_equal(all_colours_match, TRUE)
})
