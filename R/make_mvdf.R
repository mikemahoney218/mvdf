#' Construct a Minimum Viable Data Frame object
#'
#' @param data Optionally, a data frame containing all the data necessary to
#' create a `mvdf`. If left `NULL`, then `x`, `y`, `z`, and `idx` are
#' interpreted as the values to use for each slot; if not `NULL`, `x`, `y`, `z`,
#' and `idx` are interpreted as the names of columns in `data` containing the
#' values for each slot.
#' @param x,y,z Numeric: distance of the origin of the object from the origin of
#' the grid system (the central point at 0, 0, 0) in meters in the given
#' direction. Must have no `NA`, `NULL`, `NaN`, `Inf`, or `-Inf` values. If
#' `data` is not `NULL`, the names of columns in `data` with values for the
#' respective slot.
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
                     idx = NULL,
                     metadata = NULL,
                     appendix = NULL) {

  if (!is.null(data)) {
    x <- tryCatch(x, error = function(e) rlang::ensym(x))
    y <- tryCatch(y, error = function(e) rlang::ensym(y))
    z <- tryCatch(z, error = function(e) rlang::ensym(z))

    x <- data[[x]]
    y <- data[[y]]
    z <- data[[z]]

    if (is.null(idx)) {
      idx <- seq(1, length(x), 1)

      if (!is.null(metadata)) {
        metadata$idx <- idx
      }

    } else {
      idx <- tryCatch(idx, error = function(e) rlang::ensym(idx))
      idx <- data[idx]
    }

  }

  methods::new("mvdf_obj",
               x = as.double(x),
               y = as.double(y),
               z = as.double(z),
               idx = as.character(idx),
               metadata = as.data.frame(metadata),
               appendix = as.list(appendix)
  )
}
