test_that("mvdf are stable", {

  load("testdata/iris_test.rds")
  mvdf(iris_test) <- mvdf(iris_test) # Convert idx to character on R < 4.0
  load("testdata/iris_mvdf.rds")
  iris_mvdf$idx <- as.character(iris_mvdf$idx)

  expect_equal(
    iris_test,
    mvdf_obj(iris, Sepal.Width, Sepal.Length, Petal.Length, metadata = iris)
  )

  expect_equal(
    iris_mvdf,
    mvdf(iris_test)
  )
})
