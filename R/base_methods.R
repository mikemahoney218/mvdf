#' Return the first or last parts of objects inheriting from `mvdf_obj`
#'
#' @rdname headtail
#'
#' @param x An object inheriting from `mvdf_obj`
#' @param n an integer vector of length up to nrow(mvdf(x)). Values specify the
#' indices to be selected along the rows of the object. A positive value of n[i]
#' includes the first/last n[i] indices in that dimension, while a negative
#' value excludes the last/first abs(n[i]), including all remaining indices.
#' NA or non-specified values (when length(n) < length(dim(x))) select all
#' indices in that dimension. Must contain at least one non-missing value.
#' @param ... Ignored.
#'
#' @include mvdf.R
#'
#' @exportMethod head
methods::setMethod("head", "mvdf_obj", function(x, n = 6L, ...) {
  set_values(x,
    mvdf = head(mvdf(x), n),
    metadata = head(metadata(x), n),
    appendix = appendix(x)
  )
})

#' @rdname headtail
#' @exportMethod tail
methods::setMethod("tail", "mvdf_obj", function(x, n = 6L, ...) {
  set_values(x,
    mvdf = tail(mvdf(x), n),
    metadata = tail(metadata(x), n),
    appendix = appendix(x)
  )
})

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
    ...
  ))
})

#' Generic X-Y Plotting
#'
#' Draw a basic paired scatter plot from the mvdf of an object inheriting from
#' `mvdf_obj`.
#'
#' @param x Any object inheriting from mvdf_obj.
#' @param y,... Arguments passed to base `plot`.
#'
#' @exportMethod plot
methods::setMethod(
  "plot",
  "mvdf_obj",
  function(x, y, ...) plot(mvdf(x), y, ...)
)

#' The Number of Rows/Columns of an mvdf
#' 
#' `nrow` and `ncol` return the number of rows or columns present in `mvdf(x)`.
#' 
#' @param x An object inheriting from `mvdf_obj`
#' 
#' #' @return An integer of length 1 or NULL.
#' 
#' @rdname nrowncol
#' @exportMethod nrow
methods::setMethod(
  "nrow",
  "mvdf_obj",
  function(x) nrow(mvdf(x))
)

#' @rdname nrowncol
#' @exportMethod ncol
methods::setMethod(
  "ncol",
  "mvdf_obj",
  function(x) ncol(mvdf(x))
)