test_that("utility functions work", {
  expect_equal(
    deg_to_rad(90),
    0.5 * pi
  )
  expect_equal(
    deg_to_rad(180),
    1 * pi
  )
  expect_equal(
    deg_to_rad(270),
    1.5 * pi
  )
  expect_equal(
    deg_to_rad(360),
    2 * pi
  )

  expect_equal(
    all_missing(c(NA, NULL, NaN, Inf)),
    TRUE
  )

  expect_equal(
    all_missing(c(NA, 0, NULL, -Inf)),
    FALSE
  )
})
