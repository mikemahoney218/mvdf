test_that("assignment works", {
  load("testdata/iris_test.rds")

  fake_appendix <- list("3DEPElevation" = "junk_nonsense")
  appendix(iris_test) <- fake_appendix

  fake_metadata <- cbind(Orange, idx = seq(1, nrow(Orange), 1))
  metadata(iris_test) <- fake_metadata

  fake_mvdf <- mvdf(iris_test)
  fake_mvdf$x <- fake_mvdf$y
  mvdf(iris_test) <- fake_mvdf


  expect_equal(
    mvdf(iris_test),
    fake_mvdf
  )
  expect_equal(
    mvdf(iris_test)$x,
    mvdf(iris_test)$y
  )

  expect_equal(
    appendix(iris_test),
    fake_appendix
  )

  expect_equal(
    metadata(iris_test),
    fake_metadata
  )
})
