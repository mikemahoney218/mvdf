test_that("simple_material render methods function properly", {
  skip_on_cran()
  skip_on_os("mac")
  skip_on_os("windows")
  sm_mvdf <- mvdf_simple_material(NULL,
                                  x = 0,
                                  y = 0,
                                  z = 0,
                                  diffuse_color = "red",
                                  translate_colors = TRUE
  )

  img_file <- tempfile(fileext = ".png")

  execute_render(
    add_blender_endmatter(
      add_render_image(
        add_mesh_primitive(
          create_blender_frontmatter(delete = "Cube"),
          sm_mvdf
        ),
        img_file
      ),
      tempfile(fileext = ".blend")
    ),
    flags = "-noaudio -E BLENDER_WORKBENCH")

  expect_equal(
    sum(
      !brio::read_file_raw(img_file) ==
        brio::read_file_raw("testdata/simple_material.png")
    ),
    0,
    tolerance = 100 # this is about 99.9911% matching
  )
})
