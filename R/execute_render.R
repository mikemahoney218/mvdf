#' Execute a Python script in Blender
#'
#' This function takes a Python script and executes it via the Blender 3D
#' rendering program. While the intent of this function is to take scripts
#' generated by `mvdf` and related packages and use them to produce 3D
#' renderings, neither this function nor Blender validates the script -- so
#' be careful when executing Python scripts you haven't generated yourself, as
#' scripts run in Blender have the same filesystem permissions as scripts you
#' run on the terminal!
#'
#' @param script The Python script (either as a file path or as a character
#' vector with length 1) to execute.
#' @param blender Path to the Blender executable to execute the Python script.
#' If `NULL`, the default, uses the first result from `Sys.which("blender")`.
#' @param flags Additional command-line arguments to pass to `blender`.
#' @param addons A vector of Blender add-ons to enable on the command line.
#'
#' @return A length 1 character vector with the output file path is returned
#' invisibly if the function can identify the file the Python script saves to.
#' If the output file can't be identified, returns `NULL`, invisibly.
#'
#' @export
execute_render <- function(script,
                           blender = NULL,
                           flags = NULL,
                           addons = NULL) {
  if (is.null(blender)) blender <- Sys.which("blender")[[1]]

  if (blender == "") {
    stop( # nocov start
      "Couldn't find Blender.\n",
      "Please install Blender from https://www.blender.org/ \n",
      "or, if already installed, pass the path to the executable to `blender`."
    ) # nocov end
  }

  if (file.exists(script)) {
    scriptfile <- script
  } else {
    scriptfile <- tempfile(fileext = ".py")
    writeLines(script, scriptfile)
  }

  argstring <- paste("-b -P", scriptfile)
  flags <- c(flags, extract_param(scriptfile, "flag"))
  if (length(flags) > 0) argstring <- paste(argstring, flags, collapse = " ")
  addons <- c(addons, extract_param(scriptfile, "addon"))
  if (length(addons) > 0) {
    argstring <- paste(argstring,
      "--addons",
      paste0(addons, collapse = ","),
      collapse = " "
    )
  }

  # we want to raise errors in Blender as errors in R, so capture & grep
  systemputs <- paste0(system2(blender,
    args = argstring,
    stdout = TRUE,
    stderr = TRUE
  ),
  collapse = "\n"
  )

  if (any(grepl("Traceback", systemputs))) stop(systemputs)

  message(paste(systemputs, "(Success!)"))

  outfile <- regmatches(
    script,
    regexpr("(?<=mainfile\\(filepath=\\').*(?=\\')",
      script,
      perl = TRUE
    )
  )

  if (length(outfile) == 0) {
    return(invisible(NULL))
  }

  invisible(outfile)
}

extract_param <- function(scriptfile, param) {
  
  grepstr <- paste0("# ?%mvdf:", param, "s? ")
  script <- readLines(scriptfile)
  script <- grep(grepstr, script, value = TRUE)
  gsub(grepstr, "", script)

}
