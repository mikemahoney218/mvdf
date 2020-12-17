test_that("head is stable", {
  iris_mvdf <- mvdf_obj(
    data = iris,
    x = Sepal.Length,
    y = Sepal.Width,
    z = Petal.Length
  )
  head_nomd <- head(iris_mvdf)

  expect_equal(
    head_nomd,
    readRDS("testdata/head_nomd.rds")
  )

  head_noapp <- head(
    mvdf_obj(
      data = iris,
      x = Sepal.Length,
      y = Sepal.Width,
      z = Petal.Length,
      metadata = iris
    )
  )

  expect_equal(
    head_noapp,
    readRDS("testdata/head_noapp.rds")
  )

  head_all <- head(
    mvdf_obj(
      data = iris,
      x = Sepal.Length,
      y = Sepal.Width,
      z = Petal.Length,
      metadata = iris,
      appendix = iris
    )
  )

  expect_equal(
    head_all,
    readRDS("testdata/head_all.rds")
  )
})

test_that("tail is stable", {
  iris_mvdf <- mvdf_obj(
    data = iris,
    x = Sepal.Length,
    y = Sepal.Width,
    z = Petal.Length
  )
  tail_nomd <- tail(iris_mvdf)

  expect_equal(
    tail_nomd,
    readRDS("testdata/tail_nomd.rds")
  )

  tail_noapp <- tail(
    mvdf_obj(
      data = iris,
      x = Sepal.Length,
      y = Sepal.Width,
      z = Petal.Length,
      metadata = iris
    )
  )

  expect_equal(
    tail_noapp,
    readRDS("testdata/tail_noapp.rds")
  )

  tail_all <- tail(
    mvdf_obj(
      data = iris,
      x = Sepal.Length,
      y = Sepal.Width,
      z = Petal.Length,
      metadata = iris,
      appendix = iris
    )
  )

  expect_equal(
    tail_all,
    readRDS("testdata/tail_all.rds")
  )
})
