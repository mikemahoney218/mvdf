#' Add code to create mesh primitives to a Blender script
#'
#' This function generates code that will, when run inside Blender, create mesh
#' primitives within a scene.
#'
#' @template script
#' @template amp_object
#' @param primitive The primitive mesh type to create. Most popular options
#' include "cone", "cube", "cylinder", "ico_sphere", "monkey", and "uv_sphere";
#' see the official Blender documentation at
# nolint start
#' \url{https://docs.blender.org/api/blender_python_api_current/bpy.ops.mesh.html}
# nolint end
#' for a full list of available primitives.
#' @param location Either `NULL` (the default) or a vector of strings (in the
#' format `location=(x, y, z)`) specifying the location for the origin of each
#' primitive. If `NULL`, this vector will be automatically calculated using the
#' `x`, `y`, and `z` values in `object`.
#' @param ... Additional arguments to pass to the primitive creation call. The
#' available arguments are different for each primitive, and are documented in
#' the official Blender documentation at
# nolint start
#' \url{https://docs.blender.org/api/blender_python_api_current/bpy.ops.mesh.html}.
# nolint end
#'
#' @include mvdf.R
#'
#' @return A length 1 character vector containing the Blender Python script with
#' code for creating mesh primitives added.
#'
#' @export
add_mesh_primitive <- function(script,
                               object,
                               primitive = "ico_sphere",
                               location = NULL,
                               ...) {
  stopifnot(is.character(script) && (length(script) == 1))
  add_mesh_primitive_method(
    object,
    script,
    primitive,
    location,
    ...
  )
}

#' @rdname add_mesh_primitive
setGeneric(
  "add_mesh_primitive_method",
  function(object,
           script,
           primitive = "ico_sphere",
           location = NULL,
           ...) {
    standardGeneric("add_mesh_primitive_method")
  }
)

#' @rdname add_mesh_primitive
setMethod(
  "add_mesh_primitive_method",
  "mvdf_obj",
  function(object,
           script,
           primitive = "ico_sphere",
           location = NULL,
           ...) {
    dots <- list(...)

    if (length(dots) == 0) {
      dots <- NULL
    } else {
      dots <- pythonize_booleans(dots)
      dots <- create_options(dots)
    }

    if (is.null(location)) {
      location <- build_location(object)
    }

    arguments <- paste0(dots, location)

    paste0(
      script,
      "\n",
      glue::glue_collapse(
        glue::glue(
          "bpy.ops.mesh.primitive_{primitive}_add({arguments})",
          primitive = primitive,
          arguments = arguments
        ),
        sep = "\n"
      )
    )
  }
)

#' @include mvdf_simple_material.R
#' @rdname add_mesh_primitive
setMethod(
  "add_mesh_primitive_method",
  "mvdf_simple_material",
  function(object,
           script,
           primitive = "ico_sphere",
           location = NULL,
           ...) {
    dots <- list(...)

    if (length(dots) == 0) {
      dots <- NULL
    } else {
      dots <- pythonize_booleans(dots)
      dots <- create_options(dots)
    }

    mvdf_df <- mvdf(object)
    material_df <- unique(mvdf_df[c(
      "diffuse_color",
      "metallic",
      "roughness"
    )])
    # TODO: Swap to proceduralnames
    material_df$material_name <- uuid::UUIDgenerate(
      n = nrow(material_df)
    )
    # This should almost never be tripped, but just in case we get extremely
    # unlucky while generating random names...
    while (length(material_df$material_name) !=
      length(unique(material_df$material_name))) {
      material_df$material_name <- uuid::UUIDgenerate(
        n = nrow(material_df)
      )
    }

    mvdf_df <- merge(mvdf_df,
      material_df,
      by = c("roughness", "metallic", "diffuse_color")
    )

    material_df$material_call <- glue::glue(
      "bpy.data.materials.new('{material_name}')\n",
      "bpy.data.materials['{material_name}'].diffuse_color = {diffuse_color}\n",
      "bpy.data.materials['{material_name}'].roughness = {roughness}\n",
      "bpy.data.materials['{material_name}'].metallic = {metallic}\n\n",
      material_name = material_df$material_name,
      diffuse_color = material_df$diffuse_color,
      roughness = material_df$roughness,
      metallic = material_df$metallic
    )

    if (is.null(location)) {
      mvdf_df$location <- glue::glue(
        "location=({xloc}, {yloc}, {zloc})",
        xloc = mvdf_df$x,
        yloc = mvdf_df$y,
        zloc = mvdf_df$z
      )
    } else {
      mvdf_df$location <- location
    }

    mvdf_df$arguments <- paste0(dots, mvdf_df$location)
    mvdf_df$object_call <- glue::glue(
      "bpy.ops.mesh.primitive_{primitive}_add({arguments})\n",
      "bpy.context.object.data.materials.append(bpy.data.materials['{material_name}'])\n",
      primitive = primitive,
      arguments = mvdf_df$arguments,
      material_name = mvdf_df$material_name
    )

    paste0(
      script,
      "\n",
      paste0(material_df$material_call, collapse = "\n"),
      "\n",
      paste0(mvdf_df$object_call, collapse = "\n"),
      "\n"
    )
  }
)
