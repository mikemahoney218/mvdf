#' Retrieve the metadata data frame from a `forthetrees` object
#'
#' @param object The object to retrieve the metadata data frame from.
#'
#' @rdname get_metadata
#'
#' @export
setGeneric("metadata",
           function(object) standardGeneric("metadata"))

#' @rdname get_metadata
#' @exportMethod  metadata
setMethod("metadata", "mvdf_obj", function(object) {
  object@metadata
})
