test_that("default argument create_blender_endmatter is stable", {
  expect_equal(create_blender_endmatter(create_blender_frontmatter(),
                                        "fake.blend"),
               "import bpy\nimport mathutils\nimport math\n\nbpy.data.objects.remove(bpy.data.objects[\"Cube\"], do_unlink=True)\nbpy.data.objects.remove(bpy.data.objects[\"Camera\"], do_unlink=True)\nbpy.data.objects.remove(bpy.data.objects[\"Light\"], do_unlink=True)\n\n\nbpy.ops.wm.save_as_mainfile(filepath='fake.blend')\n") # nolint
})

test_that("create_blender_endmatter fails as expected", {
  expect_error(create_blender_endmatter(create_blender_frontmatter(),
                                        "fake.py"))
  expect_error(create_blender_endmatter(240))
  expect_error(create_blender_endmatter(c("script 1", "script 2")))
})

