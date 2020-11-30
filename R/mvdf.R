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
