#' Retrieve the appendix from a `forthetrees` object
#'
#' @param object The object to retrieve the appendix from.
#'
#' @include mvdf.R
#'
#' @rdname get_appendix
#'
#' @export
setGeneric("appendix",
           function(object) standardGeneric("appendix"))

#' @rdname get_appendix
#' @exportMethod appendix
setMethod("appendix", "mvdf_obj", function(object) {
  object@appendix
})

