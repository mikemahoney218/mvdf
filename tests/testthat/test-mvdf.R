test_that("mvdf are stable", {

  load("testdata/iris_test.rds")
  load("testdata/iris_mvdf.rds")

  expect_equal(
    iris_test,
    mvdf_obj(iris, Sepal.Width, Sepal.Length, Petal.Length, metadata = iris)
  )

  expect_equal(
    iris_mvdf,
    mvdf(iris_test)
  )
})
