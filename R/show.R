#' Show an object inheriting from `mvdf_obj`
#'
#' Display the object, by printing, plotting or whatever suits its class.
#'
#' @param object Any object inheriting from `mvdf_obj`
#'
#' @return `show` returns an invisible `NULL`.
#'
#' @include mvdf.R
#'
#' @exportMethod show
methods::setMethod("show", "mvdf_obj", function(object) {
  print(as.data.frame(object))
  if (nrow(object@metadata) > 0) print(object@metadata)
  if (length(object@appendix) > 0) print(object@appendix)
  return(NULL)
})
