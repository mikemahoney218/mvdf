#' Add code to create empties to a Blender script
#'
#' This function generates code that will, when run inside Blender, create
#' empties within a scene. See the official Blender documentation at
# nolint start
#' \url{https://docs.blender.org/api/blender_python_api_current/bpy.ops.object.html}
# nolint end
#' for a full list of available options.
#'
#' @template script
#' @template amp_object
#' @param type The empty type to create. Options include 'PLAIN_AXES', 'ARROWS',
#' 'SINGLE_ARROW', 'CIRCLE', 'CUBE', 'SPHERE', 'CONE', and 'IMAGE'.
#' @param location Either `NULL` (the default) or a vector of strings (in the
#' format `location=(x, y, z)`) specifying the location for the origin of each
#' empty. If `NULL`, this vector will be automatically calculated using the
#' `x`, `y`, and `z` values in `object`.
#' @param ... Additional arguments to pass to the empty creation call.
#'
#' @include mvdf.R
#'
#' @return A length 1 character vector containing the Blender Python script with
#' code for creating mesh primitives added.
#'
#' @export
add_empty <- function(script,
                      object,
                      type = "PLAIN_AXES",
                      location = NULL,
                      ...) {
  stopifnot(is.character(script) && (length(script) == 1))
  add_empty_method(
    object,
    script,
    type,
    location,
    ...
  )
}

#' @rdname add_empty
setGeneric(
  "add_empty_method",
  function(object,
           script,
           type = "PLAIN_AXES",
           location = NULL,
           ...) {
    standardGeneric("add_empty_method")
  }
)

#' @rdname add_empty
setMethod(
  "add_empty_method",
  "mvdf_obj",
  function(object,
           script,
           type = "PLAIN_AXES",
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
          "bpy.ops.object.empty_add(type='{type}', {arguments})",
          type = type,
          arguments = arguments
        ),
        sep = "\n"
      )
    )
  }
)

