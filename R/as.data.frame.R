#' Coerce to a data frame
#'
#' Coerce any object inheriting from `mvdf` to a data frame.
#'
#' @param x An object inheriting from `mvdf`.
#' @param row.names,optional,... Arguments passed to [base::as.data.frame].
#'
#' @include mvdf.R
#'
#' @exportMethod as.data.frame
methods::setMethod("as.data.frame", "mvdf_obj", function(x,
                                                         row.names,
                                                         optional,
                                                         ...) {

  nms <- setdiff(methods::slotNames(x), c("metadata", "appendix"))
  lst <- lapply(nms, function(nm) methods::slot(x, nm))
  return(as.data.frame(stats::setNames(lst, nms),
                       row.names = row.names,
                       optional = optional,
                       ...))

})
