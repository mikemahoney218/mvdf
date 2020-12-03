#' Add code to add mesh primitives to a Blender script
#'
#' This function generates code that will, when run inside Blender, create mesh
#' primitives within a scene.
#'
#' @template script
#' @template amp_object
#' @param primitive The primitive mesh type to create. See the official Blender
# nolint start
#' documentation at \url{https://docs.blender.org/api/blender_python_api_current/bpy.ops.mesh.html}
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
      # friendly conversion from R logicals to Python
      dots[which(is.logical(dots) && dots == FALSE)] <- "False"
      dots[which(is.logical(dots) && dots == TRUE)] <- "True"
      dots <- create_options(dots)
    }

    if (is.null(location)) {
      mvdf_df <- mvdf(object)
      location <- glue::glue(
        "location=({xloc}, {yloc}, {zloc})",
        xloc = mvdf_df$x,
        yloc = mvdf_df$y,
        zloc = mvdf_df$z
      )
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
