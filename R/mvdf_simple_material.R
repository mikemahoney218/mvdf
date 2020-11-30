#' Class to attach basic material data to `mvdf_obj` objects
#'
#' @template mvdfdoc
#' @slot diffuse_color Diffuse color of the material, as an RGBA array of floats
#' scaled from 0-1.
#' @slot metallic Amount of mirror reflection for raytrace, as a float from
#' 0-1.
#' @slot roughness Roughness of the material, as a float from 0-1.
#'
#' @family classes and related functions
#'
#' @exportClass mvdf_simple_material
methods::setClass("mvdf_simple_material",
                  contains = "mvdf_obj",
                  slots = c(
                    diffuse_color = "character",
                    metallic = "numeric",
                    roughness = "numeric"
                  )
)

setValidity("mvdf_simple_material", function(object) {
  error <- vector("character")
  n_issue <- 1

  if (length(object@idx) != length(object@diffuse_color) ||
      length(object@idx) != length(object@metallic) ||
      length(object@idx) != length(object@roughness)) {
    error[n_issue] <- "All slots must be the same length."
    n_issue <- n_issue + 1
  }

  if (n_issue > 1) {
    return(paste0(error, collapse = "\n"))
  }
  return(TRUE)
})

#' Create a `mvdf_simple_material` object
#'
#' @param data Optionally, a data frame containing all the data necessary to
#' create a `mvdf_simple_material`. If `NULL`, all other arguments are
#' interpreted as data to use in constructing the object; if not `NULL`,
#' arguments are interpreted as the names of columns in `data` containing the
#' values for each slot.
#' @param diffuse_color Diffuse color of the material, in either a RGBA array
#' (if `translate_colors` is `TRUE`) or in any of the formats
#' understood by [grDevices::col2rgb] (if `translate_colors` is `FALSE`).
#' @param metallic Amount of mirror reflection for raytrace, as a float from
#' 0-1.
#' @param roughness Roughness of the material, as a float from 0-1.
#' @param translate_colors Logical: use `grDevices` to create RGBA arrays from
#' `diffuse_color`?
#' @param ... Additional arguments passed to [mvdf_obj]
#'
#' @export
mvdf_simple_material <- function(data = NULL,
                                 diffuse_color = "diffuse_color",
                                 metallic = "metallic",
                                 roughness = "roughness",
                                 translate_colors = FALSE,
                                 ...) {

  res <- mvdf_obj(data = data, ...)
  res_mvdf <- mvdf(res)

  if (!is.null(data)) {
    diffuse_color <- tryCatch(diffuse_color,
                              error = function(e) rlang::ensym(diffuse_color))
    diffuse_color <- data[[diffuse_color]]

    metallic <- tryCatch(metallic,
                              error = function(e) rlang::ensym(metallic))
    metallic <- data[[metallic]]

    roughness <- tryCatch(roughness,
                              error = function(e) rlang::ensym(roughness))
    roughness <- data[[roughness]]
  } else {
    if (diffuse_color == "diffuse_color") diffuse_color <- NA
    if (metallic == "metallic") metallic <- NA
    if (roughness == "roughness") roughness <- NA
  }

  if (all_missing(metallic)) {
    metallic <- 0
  }

  if (length(metallic) == 1) {
    metallic <- rep(metallic, length(res_mvdf$idx))
  }

  if (all_missing(roughness)) {
    roughness <- 0
  }

  if (length(roughness) == 1) {
    roughness <- rep(roughness, length(res_mvdf$idx))
  }

  if (all_missing(diffuse_color)) {
    diffuse_color <- "#CCCCCCCC"
  }

  if (length(diffuse_color) == 1) {
    diffuse_color <- rep(diffuse_color, length(res_mvdf$idx))
  }

  if (translate_colors) {
    diffuse_color <- vapply(diffuse_color,
                            function(x) {
                              paste0(
                                as.vector(grDevices::col2rgb(x, TRUE) / 255),
                                collapse = ",")
                            },
                            character(1),
                            USE.NAMES = FALSE)
  }

  methods::new("mvdf_simple_material",
               x = as.double(res_mvdf$x),
               y = as.double(res_mvdf$y),
               z = as.double(res_mvdf$z),
               idx = as.character(res_mvdf$idx),
               metadata = as.data.frame(metadata(res)),
               appendix = as.list(appendix(res)),
               diffuse_color = as.character(diffuse_color),
               metallic = as.numeric(metallic),
               roughness = as.numeric(roughness)
  )

}
