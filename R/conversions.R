# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Convert from millibars to Pascals
#' @param p numeric value of pressure in millibars.
#' @return `mbar2Pa` returns pressure in Pa.
#' @export
mbar2Pa <- function(mbar) {
    mbar * 100
}

#' Convert from Pascls to millibars
#' @param p numeric value of pressure in Pascals.
#' @return `Pa2mbar` returns pressure in millibars.
#' @export
Pa2mbar <- function(Pa) {
    Pa / 100
}

#' Convert degrees C to Kelvin
#' @param TC numeric value of temperature in degrees C.
#' @return `TC2TK` returns the equivalent temperature in Kelvin.
#' @author Dan Kelley
#' @export
TC2TK <- function(TC) TC + 273.15

#' Convert Kelvin to degrees C
#' @param TK numeric value of temperature in K.
#' @return `TK2TC` returns the equivalent temperature in degrees C.
#' @author Dan Kelley
#' @export
TK2TC <- function(TK) TK - 273.15
