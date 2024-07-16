#' Dry lapse rate in degC / m
#'
#' @param g numeric value of the acceleration due to gravity at sea level,
#' with the default being as listed in Reference 1, converted to m/s^2.
#'
#' @param C_P numeric value of the specific heat of air at sea level,
#' with the default being as listed in Reference 1, converted to J/(kg*K).
#'
#' @return `dryLapseRate` returns the lapse rate, in degrees C (or Kelvin) per
#' metre.
#'
#' @references
#' 1. <https://pds-atmospheres.nmsu.edu/education_and_outreach/encyclopedia/adiabatic_lapse_rate.htm>
#'
#' @author Dan Kelley, based on Reference 1.
#'
#' @export
dryLapseRate <- function(g = 9.7986, C_P = 1004.0) {
    g / C_P
}
