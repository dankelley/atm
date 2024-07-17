# the values are as in pyMeteo (http://cwebster2.github.io/pyMeteo)

pressureReference <- 1000 # reference pressure in mbar

#' Convert degrees C to Kelvin
#' @param C numeric value of temperature in degrees C.
#' @return `C2K` returns the equivalent temperature in Kelvin.
#' @author Dan Kelley
#' @export
C2K <- function(C) C + 273.15

#' Convert Kelvin to degrees C
#' @param K numeric value of temperature in K.
#' @return `K2C` returns the equivalent temperature in degrees C.
#' @author Dan Kelley
#' @export
K2C <- function(K) K - 273.15

#' Compute air in-situ temperature from potential temperature
#'
#' In-situ temperature is computed as in Reference 1, by multiplying the
#' potential temperature by the ratio of actual to reference pressure,
#' raised to the power R/Cp.  Although both `R` and `Cp` can be specified in
#' this function, the default values are recommended for consistency.
#'
#' @param theta numeric value indicating potential temperature in
#' degrees C.
#'
#' @param pressure numeric value indicating pressure in mbar (equivalently,
#' in hPa).
#'
#' @param R dry-air gas constant, with the default value of
#' 287.052874 from Reference 2.
#'
#' @param Cp dry-air mass-specific heat, with the default value of
#' 1003.5 J/kg/K from Reference 3.  (Note that 1005 is given in some
#' websites.)
#'
#' @return `theta2T` returns in-situ temperature, in degrees Celcius.
#'
#' @author Dan Kelley, based on reference 1.
#'
#' @references
#' 1. <https://en.wikipedia.org/wiki/Potential_temperature>
#' 2. <https://en.wikipedia.org/wiki/Gas_constant>
#' 3. <https://en.wikipedia.org/wiki/Table_of_specific_heat_capacities>
#'
#' @export
theta2T <- function(theta, pressure,
                    R = 287.052874, Cp = 1003.5) {
    K2C(C2K(theta) * (pressure / pressureReference)^(R / Cp))
}


#' Compute air potential temperature from in-situ temperature
#'
#' Potential temperature is computed as in Reference 1, by multiplying the
#' in-situ temperature by the ratio reference pressure to the actual pressure,
#' raised to the power R/Cp.  Although both `R` and `Cp` can be specified in
#' this function, the default values are recommended for consistency.
#'
#' @param temperature numeric value indicating in-situ temperature in
#' degrees C.
#'
#' @param pressure numeric value indicating pressure in mbar (equivalently,
#' in hPa).
#'
#' @param R dry-air gas constant, with the default value of
#' 287.052874 from Reference 2.
#'
#' @param Cp dry-air mass-specific heat, with the default value of
#' 1003.5 J/kg/K from Reference 3.  (Note that 1005 is given in some
#' websites.)
#'
#' @return `T2theta` returns potential temperature, in degrees Celcius.
#'
#' @author Dan Kelley, based on reference 1.
#'
#' @references
#' 1. <https://en.wikipedia.org/wiki/Potential_temperature>
#' 2. <https://en.wikipedia.org/wiki/Gas_constant>
#' 3. <https://en.wikipedia.org/wiki/Table_of_specific_heat_capacities>
#'
#' @export
T2theta <- function(temperature, pressure,
                    R = 287.052874, Cp = 1003.5) {
    K2C(C2K(temperature) * (pressureReference / pressure)^(R / Cp))
}
