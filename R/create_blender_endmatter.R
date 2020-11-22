#' Add standard ending boilerplate to a Blender rendering script
#'
#' @param script The Python script to append the boilerplate code onto.
#' @param filepath The file path to save the render to. Should end with
#' `.blend`.
#'
#' @return A length 1 character vector containing the Blender Python script with
#' ending boilerplate added.
#'
#' @export
create_blender_endmatter <- function(script,
                                     filepath = tempfile(fileext = ".blend")
                                         ) {

  paste0(
    script,
    "\n\n",
    "bpy.ops.wm.save_as_mainfile(filepath='",
    filepath,
    "')\n"
  )

}
