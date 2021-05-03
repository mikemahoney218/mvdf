#' Add a camera object to a Blender scene
#'
#' @template script
#' @param align Character: alignment of the new camera object. Options include
#' "WORLD" (align the new object to the world), "VIEW" (align the new object to
#' the view), and "CURSOR" (use the 3D cursor orientation for the new object).
#' @param location A numeric vector with length 3 specifying the x, y, and z
#' grid coordinates for the new camera.
#' @param rotation A numeric vector with length 3 specifying the x, y, and z
#' rotation for the new camera.
#' @param convert_rotations Logical: convert `rotation` to radians? Set to
#' `FALSE` if `rotation` is already in radians.
#'
#' @return A length 1 character vector containing the Blender Python script with
#' code to add a camera object added.
#'
#' @export
add_camera <- function(script,
                       align = c("WORLD", "VIEW", "CURSOR"),
                       location = c(0, 0, 0),
                       rotation = c(0, 0, 0),
                       convert_rotations = TRUE) {
  align <- align[[1]]

  if (convert_rotations) rotation <- deg_to_rad(rotation)

  paste0(
    script,
    "\n\n",
    "bpy.ops.object.camera_add(align='",
    align,
    "', location=(",
    paste0(location, collapse = ","),
    "), rotation=(",
    paste0(rotation, collapse = ","),
    "))\n",
    "bpy.context.scene.camera = bpy.context.object\n"
  )
}
