#' Add a light object to a Blender scene
#'
#' @template script
#' @param type Character: Type of light source to use. Options include "POINT"
#' (omnidirectional point light source), "SUN" (constant direction parallel ray
#' light source), "SPOT" (directional cone light source), and "AREA"
#' (directional area light source).
#' @param radius Numeric: Radius of the light source.
#' @param align Character: alignment of the new light object. Options include
#' "WORLD" (align the new object to the world), "VIEW" (align the new object to
#' the view), and "CURSOR" (use the 3D cursor orientation for the new object).
#' @param location A numeric vector with length 3 specifying the x, y, and z
#' grid coordinates for the new light.
#' @param rotation A numeric vector with length 3 specifying the x, y, and z
#' rotation for the new light.
#' @param convert_rotations Logical: convert `rotation` to radians? Set to
#' `FALSE` if `rotation` is already in radians.
#' @param energy Numeric: The power ("wattage") of the light object.
#'
#' @return A length 1 character vector containing the Blender Python script with
#' code to add a light object added.
#'
#' @export
add_light <- function(script,
                      type = c("POINT", "SUN", "SPOT", "AREA"),
                      radius = 0,
                      align = c("WORLD", "VIEW", "CURSOR"),
                      location = c(0, 0, 0),
                      rotation = c(0, 0, 0),
                      convert_rotations = TRUE,
                      energy = 10) {
  type <- type[[1]]
  align <- align[[1]]

  if (convert_rotations) rotation <- deg_to_rad(rotation)

  paste0(
    script,
    "\n\n",
    "bpy.ops.object.light_add(type='",
    type,
    "', radius=",
    radius,
    ", align='",
    align,
    "', location=(",
    paste0(location, collapse = ","),
    "), rotation=(",
    paste0(rotation, collapse = ","),
    "))\n",
    "bpy.context.object.data.energy=",
    energy
  )
}
