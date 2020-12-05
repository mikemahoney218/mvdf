test_that("add_render_image is stable", {
  expect_equal(
    add_render_image("", "out.png"),
    "\n\nbpy.context.scene.render.image_settings.file_format = 'PNG'\nbpy.context.scene.render.filepath = 'out.png'\nbpy.ops.render.render(write_still = 1)\n" # nolint
  )

  expect_equal(
    add_render_image("", "out.png", extra_args = "work"),
    "\n\nbpy.context.scene.render.image_settings.file_format = 'PNG'\nbpy.context.scene.render.filepath = 'out.png'\nbpy.ops.render.render(extra_args=work, write_still = 1)\n" # nolint
  )

  expect_equal(
    add_render_image("", "out.jpg"),
    "\n\nbpy.context.scene.render.image_settings.file_format = 'JPEG'\nbpy.context.scene.render.filepath = 'out.jpg'\nbpy.ops.render.render(write_still = 1)\n" # nolint
  )
})
