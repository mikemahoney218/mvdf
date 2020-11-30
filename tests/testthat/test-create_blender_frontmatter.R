test_that("create_blender_frontmatter works", {
  expect_equal(
    create_blender_frontmatter(
      NULL,
      NULL,
      "this goes before",
      "this goes after"
    ),
    "this goes before\n\n\nthis goes after"
  )

  expect_equal(
    create_blender_frontmatter(
      "bpy",
      "Cube",
      "this goes before",
      "this goes after"
    ),
    "this goes before\nimport bpy\n\nbpy.data.objects.remove(bpy.data.objects[\"Cube\"], do_unlink=True)\n\nthis goes after" # nolint
  )
})

test_that("the default create_blender_frontmatter returns the same thing", {
  expect_equal(
    create_blender_frontmatter(),
    "import bpy\nimport mathutils\nimport math\n\nbpy.data.objects.remove(bpy.data.objects[\"Cube\"], do_unlink=True)\nbpy.data.objects.remove(bpy.data.objects[\"Camera\"], do_unlink=True)\nbpy.data.objects.remove(bpy.data.objects[\"Light\"], do_unlink=True)\n" # nolint
  )
})
