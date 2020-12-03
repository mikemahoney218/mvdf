#' Add standard ending boilerplate to a Blender rendering script
#'
#' @template script
#' @param filepath The file path to save the output file to. Must end with
#' `.blend`.
#'
#' @return A length 1 character vector containing the Blender Python script with
#' ending boilerplate added.
#'
#' @export
add_blender_endmatter <- function(script,
                                  filepath = tempfile(fileext = ".blend")) {
  stopifnot(is.character(script) && (length(script) == 1))
  stopifnot(grepl("\\.blend$", filepath))

  paste0(
    script,
    "\n\n",
    "bpy.ops.wm.save_as_mainfile(filepath='",
    filepath,
    "')\n"
  )
}
