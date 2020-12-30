test_that("multiplication works", {
  expect_match(
    add_mesh_primitive(
      create_blender_frontmatter(),
      mvdf_simple_material(x = 1, y = 1, z = 1),
      "this_is_nothing",
      depth = 10,
      location = "location = (0, 0, 0)"
    ),
    "depth=10, location = \\(0, 0, 0\\)"
  )

  expect_match(
    add_surface_primitive(
      create_blender_frontmatter(),
      mvdf_obj(x = 1, y = 1, z = 1),
      "torus"
    ),
    "bpy.ops.surface.primitive_nurbs_surface_torus_add\\(location=\\(1, 1, 1\\)\\)"
  )

  expect_match(
    add_curve_primitive(
      create_blender_frontmatter(),
      mvdf_obj(x = 1, y = 1, z = 1),
    ),
    "bpy.ops.curve.primitive_bezier_circle_add\\(location=\\(1, 1, 1\\)\\)"
  )
})
