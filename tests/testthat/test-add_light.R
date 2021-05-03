test_that("add_light is stable", {
  expect_true(
    add_light("") ==
      "\n\nbpy.ops.object.light_add(type='POINT', radius=0, align='WORLD', location=(0,0,0), rotation=(0,0,0))\nbpy.context.object.data.energy=10\n" # nolint
  )
  expect_true(
    add_light("") == add_light("", convert_rotations = FALSE)
  )
})
