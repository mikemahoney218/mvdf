#' Set mvdf values for an `mvdf_obj` object.
#'
#' @param x The `forthetrees` object to set the mvdf within.
#' @param value The data to replace the mvdf with.
#'
#' @include mvdf.R
#'
#' @rdname mvdfassign
#'
#' @export
setGeneric("mvdf<-", function(x, value) standardGeneric("mvdf<-"))

#' @rdname mvdfassign
#' @exportMethod mvdf<-
setMethod("mvdf<-", "mvdf_obj", function(x, value) {
  x <- do.call(class(x)[[1]], list(data = value,
                                   metadata = x@metadata,
                                   appendix = x@appendix))
  methods::validObject(x)
  x
})
