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
#' @param ... Any additional arguments used in the constructor function being
#' called.
#'
#' @export
set_values <- function(object,
                       mvdf = NULL,
                       metadata = NULL,
                       appendix = NULL,
                       ...) {
  if (is.null(mvdf)) mvdf <- mvdf(object)
  if (is.null(metadata)) metadata <- metadata(object)
  if (is.null(appendix)) appendix <- appendix(object)

  res <- do.call(class(object)[[1]], list(
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

#' Set slot values for objects subclassing `mvdf`
#'
#' This function returns a new object of the same class as `object` with the
#' same metadata and appendix slots as `object`, but with other slots taking
#' values from `mvdf`. Use it as a convenient, pipe-able way to set slot values
#' for objects subclassing `mvdf`.
#'
#' @param mvdf The minimum viable data frame required by the S4 class
#' @param object The object to update.
#' @param metadata The metadata to include in the new object. If `NULL`
#' (the default), uses the metadata from `object`.
#' @param appendix The appendix to include in the new object. If `NULL`
#' (the default), uses the appendix from `object`.
#' @param ... Any additional arguments used in the constructor function being
#' called.
#'
#' @export
set_mvdf <- function(mvdf, object, metadata = NULL, appendix = NULL, ...) {
  set_values(
    object = object,
    mvdf = mvdf,
    metadata = metadata,
    appendix = appendix,
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

#' Set metadata values for objects subclassing `mvdf`
#'
#' This function returns a new object of the same class as `object` with the
#' same mvdf and appendix slots as `object`, but with a new metadata. Use it as
#' a convenient, pipe-able way to set metadata values for objects subclassing
#' `mvdf`.
#'
#' @param metadata The metadata to include in the new object.
#' @param object The object to update.
#' @param mvdf The minimum viable data frame required by the S4 class. If `NULL`
#' (the default), uses the mvdf from `object`.
#' @param appendix The appendix to include in the new object. If `NULL`
#' (the default), uses the appendix from `object`.
#' @param ... Any additional arguments used in the constructor function being
#' called.
#'
#' @export
set_metadata <- function(metadata, object, mvdf = NULL, appendix = NULL, ...) {
  set_values(
    object = object,
    mvdf = mvdf,
    metadata = metadata,
    appendix = appendix,
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

#' Set appendix values for objects subclassing `mvdf`
#'
#' This function returns a new object of the same class as `object` with the
#' same mvdf and metadata slots as `object`, but with a new appendix. Use it as
#' a convenient, pipe-able way to set appendix values for objects subclassing
#' `mvdf`.
#'
#' @param appendix The appendix to include in the new object.
#' @param object The object to update.
#' @param mvdf The minimum viable data frame required by the S4 class. If `NULL`
#' (the default), uses the mvdf from `object`.
#' @param metadata The metadata to include in the new object. If `NULL`
#' (the default), uses the metadata from `object`.
#' @param ... Any additional arguments used in the constructor function being
#' called.
#'
#' @export
set_appendix <- function(appendix, object, mvdf = NULL, metadata = NULL, ...) {
  set_values(
    object = object,
    mvdf = mvdf,
    metadata = metadata,
    appendix = appendix,
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
