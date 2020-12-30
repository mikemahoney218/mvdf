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

  expect_match(
    add_empty("", mvdf_obj(NULL, 1, 1, 1), radius = 0.5),
    "\nbpy.ops.object.empty_add\\(type='PLAIN_AXES', radius=0.5, location=\\(1, 1, 1\\)\\)" # nolint
  )
})
