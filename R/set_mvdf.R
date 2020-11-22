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
