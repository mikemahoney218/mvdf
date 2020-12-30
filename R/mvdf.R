#' The Minimum Viable Data Frame S4 class
#'
#' @template mvdfdoc
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

#' Construct a Minimum Viable Data Frame object
#'
#' @param data Optionally, a data frame containing all the data necessary to
#' create a `mvdf_obj`. If left `NULL`, then `x`, `y`, `z`, and `idx` are
#' interpreted as the values to use for each slot; if not `NULL`, `x`, `y`, `z`,
#' and `idx` are interpreted as the names of columns in `data` containing the
#' values for each slot.
#' @param x,y,z Numeric: distance of the origin of the object from the origin of
#' the grid system (the central point at 0, 0, 0) in meters in the given
#' direction. Must have no `NA`, `NULL`, `NaN`, `Inf`, or `-Inf` values. If
#' `data` is not `NULL`, the names of columns in `data` with values for the
#' respective slot. Coordinates are assumed to be on a right-handed coordinate
#' system with Z oriented as the natural "vertical" direction.
#' @param idx Character: a unique identifier (or "index") for each object to be
#' modeled. Must be unique with no `NA` or `NULL` values, but otherwise is
#' not validated. If  `data` is not `NULL`, the names of columns in `data` with
#' values for the slot. If left `NULL`, a sequential index is generated.
#' @param metadata Data frame: a table containing additional information on the
#' objects to be modeled. Optional, but if this slot is used then the data frame
#' must contain a column named `idx` which should correspond to the `idx` slot.
#' Only the existence of this column is validated.
#' @param appendix List: additional data produced in the generation of the
#' object. Not validated; any additional outputs that don't map to modeled
#' objects may be inserted here.
#'
#' @export
mvdf_obj <- function(data = NULL,
                     x = "x",
                     y = "y",
                     z = "z",
                     idx = "idx",
                     metadata = NULL,
                     appendix = NULL) {
  if (!is.null(data)) {
    x <- eval_arg(data, rlang::ensym(x))
    y <- eval_arg(data, rlang::ensym(y))
    z <- eval_arg(data, rlang::ensym(z))

    if (any_missing(x)) {
      if (all_missing(x)) stop("Couldn't determine x values.")
      stop("x may not contain any missing values.")
    }

    idx <- eval_arg(data, rlang::ensym(idx))
  }

  if (is.null(idx) |
    (length(idx) == 1 && idx == "idx")) {
    idx <- seq(1, length(x), 1)

    if (!is.null(metadata)) {
      metadata$idx <- as.character(seq(1, nrow(metadata), 1))
    }
  }

  arg_list <- list(
    Class = "mvdf_obj",
    x = as.double(x),
    y = as.double(y),
    z = as.double(z),
    idx = as.character(idx),
    metadata = as.data.frame(metadata)
  )

  if (!is.null(appendix)) arg_list[["appendix"]] <- as.list(appendix)

  do.call(methods::new, arg_list)
}


setValidity("mvdf_obj", function(object) {
  error <- vector("character")
  n_issue <- 1

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
