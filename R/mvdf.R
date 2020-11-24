#' The Minimum Viable Data Frame S4 class
#'
#' @slot x,y,z Numeric: distance of the origin of the object from the origin of
#' the grid system (the central point at 0, 0, 0) in meters in the given
#' direction. Must have no `NA`, `NULL`, `NaN`, `Inf`, or `-Inf` values.
#' @slot idx Character: a unique identifier (or "index") for each object to be
#' modeled. Must be unique with no `NA` or `NULL` values, but otherwise is
#' not validated.
#' @slot metadata Data frame: a table containing additional information on the
#' objects to be modeled. Optional, but if this slot is used then the data frame
#' must contain a column named `idx` which should correspond to the `idx` slot.
#' Only the existence of this column is validated.
#' @slot appendix List: additional data produced in the generation of the
#' object. Not validated; any additional outputs that don't map to modeled
#' objects may be inserted here.
#'
#' @family classes and related functions
#'
#' @exportClass mvdf_obj
methods::setClass("mvdf_obj",
  slots = c(
    x = "numeric",
    y = "numeric",
    z = "numeric",
    idx = "character",
    metadata = "data.frame",
    appendix = "list"
  )
)

setValidity("mvdf_obj", function(object) {
  error <- vector("character")
  n_issue <- 1

  any_missing <- function(obj) {
    any(is.null(obj) |
      is.na(obj) |
      is.infinite(obj) |
      is.nan(obj))
  }

  if (length(object@idx) != length(unique(object@idx))) {
    error[n_issue] <- "@idx must be unique."
    n_issue <- n_issue + 1
  }
  if (any_missing(object@idx)) {
    error[n_issue] <- "@idx must not have any NULL or NA values."
    n_issue <- n_issue + 1
  }
  if (any_missing(object@x)) {
    error[n_issue] <- "@x must not have any missing values."
    n_issue <- n_issue + 1
  }
  if (any_missing(object@y)) {
    error[n_issue] <- "@y must not have any missing values."
    n_issue <- n_issue + 1
  }
  if (any_missing(object@z)) {
    error[n_issue] <- "@z must not have any missing values."
    n_issue <- n_issue + 1
  }
  if (length(object@idx) != length(object@x) ||
    length(object@idx) != length(object@y) ||
    length(object@idx) != length(object@z)) {
    error[n_issue] <- "All slots must be the same length."
    n_issue <- n_issue + 1
  }
  if (nrow(object@metadata) > 0 && !("idx" %in% names(object@metadata))) {
    error[n_issue] <- "@metadata must have an index column named 'idx'."
  }

  if (n_issue > 1) {
    return(paste0(error, collapse = "\n"))
  }
  return(TRUE)
})
