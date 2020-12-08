test_that("execute_render stops on failure", {
  skip_on_cran()
  skip_on_os("mac")
  skip_on_os("windows")
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
      ),
      flags = "-noaudio"
    )
  )


  expect_error(
    execute_render(
      add_blender_endmatter(
        add_render_image(
          add_mesh_primitive(
            add_light(
              add_light(
                add_camera(
                  create_blender_frontmatter(),
                  location = c(18.069, -6.7758, 5.2883),
                  rotation = c(86.6, 15.4, 53)
                ),
                location = c(2.9662, 1.8655, 5.9039),
                rotation = c(37.3, 3.16, 107),
                energy = 1000
              ),
              location = c(7.4562, 4.1455, 4.8439),
              rotation = c(37.3, 3.16, 107),
              energy = 1000
            ),
            object = mvdf_obj(
              data = iris,
              x = Sepal.Length,
              y = Sepal.Width,
              z = Petal.Length
            ),
            primitive = "ico_sphere",
            radius = 0.05
          ),
          tempfile(fileext = ".png")
        ),
        filepath = tempfile(fileext = ".blend")
      ),
      flags = "-noaudio"
    ),
    NA
  )

})
