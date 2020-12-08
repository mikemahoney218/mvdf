#' @slot x,y,z Numeric: distance of the origin of the object from the origin of
#' the grid system (the central point at 0, 0, 0) in meters in the given
#' direction. Must have no `NA`, `NULL`, `NaN`, `Inf`, or `-Inf` values.
#' Coordinates are assumed to be on a right-handed coordinate system with Z
#' oriented as the natural "vertical" direction.
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
