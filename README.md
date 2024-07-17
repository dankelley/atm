# The atmosphere R package

<!-- badges: start -->

[![GitHub last
commit](https://img.shields.io/github/last-commit/dankelley/atmosphere)](https://img.shields.io/github/last-commit/dankelley/atmosphere)
[![R-CMD-check](https://github.com/dankelley/atmosphere/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dankelley/atmosphere/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `atmosphere` package provides R functions for dealing with atmospheric
properties. It is *very* preliminary, and function names and actions may change
a lot at any moment.

*Warning.* The package is designed to work with files provided via the GUI at a
University of Wyoming server
([https://weather.uwyo.edu/upperair/sounding.html](https://weather.uwyo.edu/upperair/sounding.html))
and it will be rendered non-functional if that server changes its URL structure
or the format of the data files it provides. Given sufficient interest, the code could be altered to accommodate changes to the server, but since this is all based on reverse engineering, there is no way to be sure of success.
