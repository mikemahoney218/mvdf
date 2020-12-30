test_that("mvdf are stable", {
  load("testdata/iris_test.rds")
  load("testdata/iris_mvdf.rds")

  expect_equal(
    iris_test,
    mvdf_obj(iris, Sepal.Width, Sepal.Length, Petal.Length, metadata = iris)
  )

  # Fix test for R < 4.0
  out_mvdf <- mvdf(iris_test)
  out_mvdf$idx <- as.character(out_mvdf$idx)

  expect_equal(
    iris_mvdf,
    out_mvdf
  )

  expect_equal(
    mvdf_obj(data.frame(x = c(1, 1), y = c(1, 1), z = c(1, 1))),
    mvdf_obj(x = c(1, 1), y = c(1, 1), z = c(1, 1))
  )

  expect_error(
    mvdf_obj(data.frame(x = NA)),
    "x may not contain any missing values"
  )
})
