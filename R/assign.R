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

#' Set slot values for objects subclassing `mvdf`
#'
#' This function returns a new object of the same class as `object` with the
#' same metadata and appendix slots as `object`, but with other slots taking
#' values from `mvdf`. Use it as a convenient, pipe-able way to set slot values
#' for objects subclassing `mvdf`
#'
#' @param mvdf The minimum viable data frame required by the S4 class
#' @param object The
#' @param metadata The metadata to include in the new object. If `NULL`
#' (the default), uses the metadata from `object`.
#' @param appendix The appendix to include in the new object. If `NULL`
#' (the default), uses the appendix from `object`.
#'
#' @export
set_mvdf <- function(mvdf, object, metadata = NULL, appendix = NULL) {
  if (is.null(metadata)) metadata <- object@metadata
  if (is.null(appendix)) appendix <- object@appendix
  res <- do.call(class(object)[[1]], list(data = mvdf,
                                          metadata = metadata,
                                          appendix = appendix))
  methods::validObject(res)
  return(res)
}
