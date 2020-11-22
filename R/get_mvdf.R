#' Retrieve mvdf values from a `mvdf_obj` object.
#'
#' @param object The `mvdf_obj` object to retrieve the mvdf for.
#'
#' @name mvdf
#'
#' @export
setGeneric("mvdf", function(object) standardGeneric("mvdf"))

#' @rdname mvdf
#' @exportMethod mvdf
setMethod("mvdf", "mvdf_obj", function(object) as.data.frame(object))
