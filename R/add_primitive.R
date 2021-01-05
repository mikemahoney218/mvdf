#' Add code to add primitives to a Blender script
#'
#' This function generates code that will, when run inside Blender, create mesh
#' primitives within a scene.
#'
#' @template amp_object
#' @template script
#' @param category What category of primitive to add. This function works well
#' with `mesh`, `curve`, and `surface` primitives.
#' @param primitive The primitive type to create. See the official Blender
#' documentation at
# nolint start
#' \url{https://docs.blender.org/api/blender_python_api_current/bpy.ops.html}
# nolint end
#' for a full list of available primitives. Each category has a different set of
#' primitives available.
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
#' code for creating primitives added.
#'
#' @name add_primitive
#'
setGeneric(
  "add_primitive",
  function(object,
           script,
           category,
           primitive,
           location = NULL,
           ...) {
    standardGeneric("add_primitive")
  }
)

#' @rdname add_primitive
setMethod(
  "add_primitive",
  "mvdf_obj",
  function(object,
           script,
           category,
           primitive = "ico_sphere",
           location = NULL,
           ...) {
    stopifnot(is.character(script) && (length(script) == 1))
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
          "bpy.ops.{category}.primitive_{primitive}_add({arguments})",
          category = category,
          primitive = primitive,
          arguments = arguments
        ),
        sep = "\n"
      )
    )
  }
)

#' @include mvdf_simple_material.R
#' @rdname add_primitive
setMethod(
  "add_primitive",
  "mvdf_simple_material",
  function(object,
           script,
           category,
           primitive = "ico_sphere",
           location = NULL,
           ...) {
    stopifnot(is.character(script) && (length(script) == 1))
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
    material_df$material_name <- proceduralnames::make_english_names(
      n = nrow(material_df)
    )
    # This should almost never be tripped, but just in case we get extremely
    # unlucky while generating random names...
    while (length(material_df$material_name) !=
      length(unique(material_df$material_name))) {
      # nocov start
      material_df$material_name <- proceduralnames::make_english_names(
        n = nrow(material_df)
      )
      # nocov end
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
      "bpy.ops.{category}.primitive_{primitive}_add({arguments})\n",
      "bpy.context.object.data.materials.append(bpy.data.materials['{material_name}'])\n", # nolint
      category = category,
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

#' @rdname add_primitive
#' @export
add_mesh_primitive <- function(script,
                               object,
                               primitive = "ico_sphere",
                               location = NULL,
                               ...) {
  add_primitive(
    object,
    script,
    category = "mesh",
    primitive,
    location,
    ...
  )
}

#' @rdname add_primitive
#' @export
add_surface_primitive <- function(script,
                                  object,
                                  primitive = "torus",
                                  location = NULL,
                                  ...) {
  if (!grepl("nurbs_surface_", primitive)) {
    primitive <- paste0(
      "nurbs_surface_",
      primitive
    )
  }
  add_primitive(
    object,
    script,
    category = "surface",
    primitive,
    location,
    ...
  )
}

#' @rdname add_primitive
#' @export
add_curve_primitive <- function(script,
                                object,
                                primitive = "bezier_circle",
                                location = NULL,
                                ...) {
  add_primitive(
    object,
    script,
    category = "curve",
    primitive,
    location,
    ...
  )
}
