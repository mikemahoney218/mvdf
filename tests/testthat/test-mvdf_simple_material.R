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
        paste0(
          add_mesh_primitive(
            create_blender_frontmatter(delete = "Cube"),
            sm_mvdf
          ),
          "\n",
          "bpy.data.scenes['Scene'].render.engine = 'CYCLES'\n"
        ),
        img_file
      ),
      tempfile(fileext = ".blend")
    ),
    flags = "-noaudio -E CYCLES"
  )

  skip_on_ci()

  expect_equal(
    sum(
      !brio::read_file_raw(img_file) ==
        readRDS("testdata/simple_material.rds")
    ),
    0,
    tolerance = 100 # this is about 99.9911% matching
  )
})

test_that("simple_material functions as expected", {
  expect_equal(
    mvdf_simple_material(data.frame(x = c(1, 1), y = c(1, 1), z = c(1, 1))),
    mvdf_simple_material(
      x = c(1, 1),
      y = c(1, 1),
      z = c(1, 1),
      metallic = c(0, NA),
      roughness = c(NA, 0),
      diffuse_color = c("0.8,0.8,0.8,0.8", NA)
    )
  )
})
