test_that("add_camera is stable", {
  expect_true(
    add_camera("") ==
      "\n\nbpy.ops.object.camera_add(align='WORLD', location=(0,0,0), rotation=(0,0,0))\nbpy.context.scene.camera = bpy.context.object\n" # nolint
  )
  expect_true(
    add_camera("") == add_camera("", convert_rotations = FALSE)
  )
})
