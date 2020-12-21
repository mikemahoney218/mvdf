#' Set values for objects subclassing `mvdf_obj`
#'
#' This function returns a new object of the same class as `object` with updated
#' values. Use it as a convenient, pipe-able way to set values for
#' objects subclassing `mvdf_obj`.
#'
#' @param object The object to update.
#' @param mvdf The minimum viable data frame required by the S4 class. If `NULL`
#' (the default), uses the mvdf from `object`.
#' @param metadata The metadata to include in the new object. If `NULL`
#' (the default), uses the metadata from `object`.
#' @param appendix The appendix to include in the new object. If `NULL`
#' (the default), uses the appendix from `object`.
#' @param newclass The class of the object to return. If `NULL` (the default),
#' returns an object of class `class(object)`.
#' @param ... Any additional arguments used in the constructor function being
#' called.
#'
#' @name setvalues
#'
#' @return An S4 object (of class `newclass` if specified or `class(object)` if
#' not) with updated values.
#'
#' @export
set_values <- function(object,
                       mvdf = NULL,
                       metadata = NULL,
                       appendix = NULL,
                       newclass = NULL,
                       ...) {
  if (is.null(mvdf)) mvdf <- mvdf(object)
  if (is.null(metadata)) metadata <- metadata(object)
  if (is.null(appendix)) appendix <- appendix(object)
  class_fun <- class(object)[[1]]
  if (!is.null(newclass)) class_fun <- newclass

  res <- do.call(class_fun, list(
    data = mvdf,
    metadata = metadata,
    appendix = appendix,
    ...
  ))
  methods::validObject(res)
  return(res)
}

################################################################################
##
## mvdf
##
################################################################################

#' @name setvalues
#' @export
set_mvdf <- function(mvdf,
                     object,
                     metadata = NULL,
                     appendix = NULL,
                     newclass = NULL,
                     ...) {
  set_values(
    object = object,
    mvdf = mvdf,
    metadata = metadata,
    appendix = appendix,
    newclass = newclass,
    ...
  )
}

#' Set mvdf values for an `mvdf_obj` object.
#'
#' @param x The object to set the mvdf within.
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
  set_mvdf(value, x)
})

################################################################################
##
## Metadata
##
################################################################################

#' @name setvalues
#' @export
set_metadata <- function(metadata,
                         object,
                         mvdf = NULL,
                         appendix = NULL,
                         newclass = NULL,
                         ...) {
  set_values(
    object = object,
    mvdf = mvdf,
    metadata = metadata,
    appendix = appendix,
    newclass = newclass,
    ...
  )
}

#' Set metadata values for an `mvdf_obj` object.
#'
#' @param x The object to set the metadata within.
#' @param value The data to replace the metadata with.
#'
#' @include mvdf.R
#'
#' @rdname metadataassign
#'
#' @export
setGeneric("metadata<-", function(x, value) standardGeneric("metadata<-"))

#' @rdname metadataassign
#' @exportMethod metadata<-
setMethod("metadata<-", "mvdf_obj", function(x, value) {
  set_metadata(value, x)
})

################################################################################
##
## Appendix
##
################################################################################

#' @name setvalues
#' @export
set_appendix <- function(appendix,
                         object,
                         mvdf = NULL,
                         metadata = NULL,
                         newclass = NULL,
                         ...) {
  set_values(
    object = object,
    mvdf = mvdf,
    metadata = metadata,
    appendix = appendix,
    newclass = newclass,
    ...
  )
}

#' Set appendix values for an `mvdf_obj` object.
#'
#' @param x The object to set the appendix within.
#' @param value The data to replace the appendix with.
#'
#' @include mvdf.R
#'
#' @rdname appendixassign
#'
#' @export
setGeneric("appendix<-", function(x, value) standardGeneric("appendix<-"))

#' @rdname appendixassign
#' @exportMethod appendix<-
setMethod("appendix<-", "mvdf_obj", function(x, value) {
  set_appendix(value, x)
})
