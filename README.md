
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mvdf

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of mvdf is to provide a standardized approach to using R as a
frontend for the [Blender](https://www.blender.org/) 3D rendering
program.

Please note that this package is in early development; breaking changes
can and will happen as better approaches become evident.

## What’s an MVDF anyway?

The `mvdf` package proposes a standard approach to using R as a frontend
interface for the Blender 3D rendering program. By breaking the
development of rendering into three distinct steps – *importing* data
from disparate sources into a standardized format, *manipulating* that
format through a standard set of tools, and creating 3D renderings from
standardized formats via *exporter* functions – `mvdf` forms the
cornerstone of an easily-extensible framework for producing data-driven
3D renders.

## Installation

You can install the development version of `mvdf` from GitHub via:

``` r
remotes::install_github("mikemahoney218/mvdf")
```
