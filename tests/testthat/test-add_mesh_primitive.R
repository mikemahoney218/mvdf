test_that("add_mesh_primitive works", {
  iris_mvdf <- mvdf_obj(iris, Sepal.Width, Sepal.Length, Petal.Length)
  expect_equal(
    add_blender_endmatter(
      add_mesh_primitive(
        create_blender_frontmatter(),
        iris_mvdf,
        "ico_sphere"
      ),
      filepath = "tmp.blend"
    ),
    gsub("\\r", "",
         readChar(
           "testdata/mesh_prim.txt",
           file.info("testdata/mesh_prim.txt")$size
           )
         )
    )
})
