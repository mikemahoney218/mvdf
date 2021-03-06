% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mvdf_simple_material.R
\name{mvdf_simple_material}
\alias{mvdf_simple_material}
\title{Create a `mvdf_simple_material` object}
\usage{
mvdf_simple_material(
  data = NULL,
  diffuse_color = "diffuse_color",
  metallic = "metallic",
  roughness = "roughness",
  translate_colors = NULL,
  ...
)
}
\arguments{
\item{data}{Optionally, a data frame containing all the data necessary to
create a `mvdf_simple_material`. If `NULL`, all other arguments are
interpreted as data to use in constructing the object; if not `NULL`,
arguments are interpreted as the names of columns in `data` containing the
values for each slot.}

\item{diffuse_color}{Diffuse color of the material, in either a RGBA array
(if `translate_colors` is `TRUE`) or in any of the formats
understood by [grDevices::col2rgb] (if `translate_colors` is `FALSE`).
If colors are missing, they are set to gray80. If `translate_colors` is 
`NULL`, the default, this function attempts to infer if values are already
RGBA arrays.}

\item{metallic}{Amount of mirror reflection for raytrace, as a float from
0-1. If missing, set to 0.}

\item{roughness}{Roughness of the material, as a float from 0-1. If missing,
set to 0.}

\item{translate_colors}{Logical: use `grDevices` to create RGBA arrays from
`diffuse_color`?}

\item{...}{Additional arguments passed to [mvdf_obj]}
}
\description{
Create a `mvdf_simple_material` object
}
