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
  print(object@metadata)
  print(object@appendix)
  return(NULL)

})
