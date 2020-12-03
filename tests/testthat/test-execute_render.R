test_that("execute_render stops on failure", {
  skip_on_cran()
  skip_on_ci()
  trunk <- mvdf_obj(x = 0, y = 0, z = 5)
  expect_error(
    execute_render(
      create_blender_endmatter(
        filepath = "tree.blend",
        add_mesh_primitive(
          create_blender_frontmatter(),
          trunk,
          "this_is_nothing",
          depth = 10
        )
      )
    )
  )
})
