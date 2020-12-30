test_that("show is stable", {
  load("testdata/iris_test_output.rds")
  load("testdata/iris_test.rds")
  appendix(iris_test) <- list("a" = "hello")
  expect_equal(
    capture.output(iris_test),
    iris_test_output
  )
})
