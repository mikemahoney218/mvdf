test_that("multiplication works", {
  iris_mvdf <- mvdf_obj(iris, Sepal.Width, Sepal.Length, Petal.Length)
  expect_equal(
    add_blender_endmatter(
      add_empty(
        create_blender_frontmatter(),
        iris_mvdf,
        "PLAIN_AXES"
      ),
      filepath = "tmp.blend"
    ),
    gsub(
      "\\r", "",
      readChar(
        "testdata/empty.txt",
        file.info("testdata/empty.txt")$size
      )
    )
  )
})

