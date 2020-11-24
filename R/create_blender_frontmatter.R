#' Create a Blender rendering script with standard beginning boilerplate
#'
#' @param imports A character vector of Python packages to import at the start
#' of the script. Packages are imported without aliases, so that script-building
#' functions may always expect to use the full package name. Defaults to
#' `bpy`, `mathutils`, and `math`. Set to `NULL` to not import any packages.
#' @param delete A character vector of objects in the default Blender scene to
#' delete. Defaults to all objects in the default scene (`Cube`, `Camera`, and
#' `Light`). Set to `NULL` to not delete any objects.
#' @param before Optionally, a character vector containing lines of code to add
#' before package imports.
#' @param after Optionally, a character vector containing lines of code to add
#' after object deletion.
#'
#' @return A length 1 character vector containing the introductory boilerplate
#' for a Blender Python script.
#'
#' @export
create_blender_frontmatter <- function(imports = c("bpy",
                                                   "mathutils",
                                                   "math"),
                                       delete = c(
                                         "Cube",
                                         "Camera",
                                         "Light"
                                         ),
                                       before = NULL,
                                       after = NULL) {

  if (is.null(imports)) {
    out_imports <- ""
  } else {
    out_imports <- vector("list", length = length(imports) + 1)
    for (i in seq_len(length(imports))) {
      out_imports[[i]] <- paste("import", imports[[i]])
      out_imports[[i + 1]] <- ""
    }
  }

  if (is.null(delete)) {
    out_delete <- ""
  } else {
    out_delete <- vector("list", length = length(delete) + 1)
    for (i in seq_len(length(delete))) {
      out_delete[[i]] <- paste0('bpy.data.objects.remove(bpy.data.objects["',
                                delete[[i]],
                                '"], do_unlink=True)')
      out_delete[[i + 1]] <- ""
    }
  }

  out_script <- paste0(
    paste0(out_imports, collapse = "\n"),
    "\n",
    paste0(out_delete, collapse = "\n"),
    "",
    collapse = "\n")

  if (length(before) > 0) out_script <- paste0(
    paste0(before, collapse = "\n"),
    "\n",
    out_script,
    collapse = "\n"
  )

  if (length(after) > 0) out_script <- paste0(
    out_script,
    "\n",
    paste0(after, collapse = "\n"),
    collapse = "\n"
  )

  return(out_script)

}
