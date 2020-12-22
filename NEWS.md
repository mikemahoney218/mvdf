# mvdf 0.0.0.9000

* This is the development version that will become version 0.1.0
* Added a `NEWS.md` file to track changes to the package.
* Added two classes:
  * `mvdf_obj` objects have an x, y, and z coordinate plus a unique identifier
  * `mvdf_simple_material` objects contain `mvdf_obj` and add diffuse color, 
    metallic, and roughness fields.
* Added methods for adding primitives to a render (`add_primitive`), with 
  convenience wrappers for meshes, surfaces, and curves.
* Added ways to manipulate blender scripts including 
  `create_blender_frontmatter`, `add_light`, `add_camera`, `add_render_image`,
  and `add_blender_endmatter`.
