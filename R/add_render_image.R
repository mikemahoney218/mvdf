#' Add steps to render a scene to an image file to a Blender script
#'
#' This function will add the necessary steps to render a Blender scene and
#' save the rendering to an image file. Note that this process requires you have
#' a camera and a light source in your scene!
#'
#' @template script
#' @param filepath The file path to save the output file to. Note that the file
#' format to use is inferred from the file extension; using non-standard or
#' abbreviated extensions might result in render errors. Allowed formats include
#' BMP, IRIS, PNG, JPEG, JPEG2000, TARGA, TARGA_RAW, CINEON, DPX,
#' OPEN_EXR_MULTILAYER, OPEN_EXR, HDR, TIFF, AVI_JPEG, AVI_RAW, FRAMESERVER,
#' H264, FFMPEG, THEORA, XVID.
#' @param ... Any additional options to pass to `bpy.ops.render.render`.
#'
#' @return A length 1 character vector containing the Blender Python script with
#' render-to-image code added.
#'
#' @export
add_render_image <- function(script,
                             filepath,
                             ...) {
  dots <- list(...)
  if (length(dots) == 0) {
    dots <- ""
  } else {
    dots <- paste(create_options(dots))
  }

  stopifnot(is.character(script) && (length(script) == 1))

  form <- regmatches(
    filepath,
    regexpr("(?<=\\.)[a-z]*$", # matches extensions
      filepath,
      perl = TRUE
    )
  )
  form <- toupper(form)

  official_names <- c(
    "JPEG" = "JPG",
    "TIFF" = "TIF"
  )
  if (form %in% official_names) {
    form <- names(official_names)[which((official_names) %in% form)]
  }

  paste0(
    script,
    "\n\n",
    "bpy.context.scene.render.image_settings.file_format = '",
    form,
    "'\n",
    "bpy.context.scene.render.filepath = '",
    filepath,
    "'\nbpy.ops.render.render(",
    dots,
    "write_still = 1",
    ")\n"
  )
}
