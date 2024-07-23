# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

# Below, WC stands for Wallace, John M., and Peter Victor Hobbs. Atmospheric
# Science: An Introductory Survey. 2nd ed. International Geophysics Series, v.
# 92. Amsterdam ; Boston: Elsevier Academic Press, 2006.

g <- 9.81 # acceleration due to gravity (depends on altitude and latitude)
pressureReference <- 1e3 # reference pressure in mbar (use 1e5 if in Pa)
TC0 <- 273.15 # add to TC (Celcius) to get TK (Kelvin)
Rd <- 287.04 # WH (3.11) dry-air gas constant
Rv <- 461.5 # WH (3.13) gas constant for 1kg water vapour
epsilon <- Rd / Rv # WH (3.14)
L <- 2.501e6 # pymeteo: latent heat of vaporization
cp <- 1005.7 # pymeteo: what about cpd vs cpv
cpd <- 1005.7 # pymeteo: what about cpd vs cpv
cpv <- 1875.0 # pymeteo
cpl <- 4190.0 # pymeteo
cpi <- 2118.636 # pymeteo
cv <- 718. # pymeteo

#' Compute air in-situ temperature from potential temperature
#'
#' In-situ temperature is computed as in Chapter 3 of Reference 1, i.e. by
#' multiplying the potential temperature by the ratio of actual pressure to reference
#' pressure, raised to the power `Rd`/`Cp`. (The computation is done in Absolute
#' Temperature, with conversions to and from Celcius being done with [TK2TC()]
#' and [TC2TK()], respectively.)  See also [T2theta()], which is the inverse
#' of `theta2T()`.
#'
#' @param theta numeric value indicating potential temperature in
#' degrees C.
#'
#' @param pressure numeric value indicating pressure in mbar (equivalently,
#' in hPa).
#'
#' @param Rd,Cp dry-air gas constant and mass-specific heat capacity,
#' with the default values as in Reference 2.
#'
#' @return `theta2T` returns in-situ temperature, in degrees Celcius.
#'
#' @author Dan Kelley, with the default values for `Rd` and `cp`
#' taken from Reference 1.
#'
#' @references
#' 1. Wallace, John M., and Peter Victor Hobbs. Atmospheric Science: An
#'    Introductory Survey. 2nd ed. International Geophysics Series, volume 92.
#'    Amsterdam ; Boston: Elsevier Academic Press, 2006.
#' 2. <https://github.com/cwebster2/pyMeteo>
#'
#' @export
theta2T <- function(theta, pressure,
                    Rd = 287.04, Cp = 1005.7) {
    TK2TC(TC2TK(theta) * (pressure / pressureReference)^(Rd / Cp))
}


#' Compute air potential temperature from in-situ temperature
#'
#' Potential temperature is computed as in Chapter 3 of Reference 1, i.e. by
#' dividing the in-situ temperature by the ratio of actual pressure to reference
#' pressure, raised to the power `Rd`/`Cp`. (The computation is done in Absolute
#' Temperature, with conversions to and from Celcius being done with [TK2TC()]
#' and [TC2TK()], respectively.)  See also [theta2T()], which is the inverse
#' of `T2theta()`.
#'
#' @param temperature numeric value indicating in-situ temperature in
#' degrees C.
#'
#' @param pressure numeric value indicating pressure in mbar (equivalently,
#' in hPa).
#'
#' @param Rd,Cp dry-air gas constant and mass-specific heat capacity,
#' with the default values as in Reference 2.
#'
#' @return `T2theta` returns potential temperature, in degrees Celcius.
#'
#' @author Dan Kelley, with the default values for `Rd` and `Cp`
#' taken from Reference 1.
#'
#' @references
#' 1. Wallace, John M., and Peter Victor Hobbs. Atmospheric Science: An
#'    Introductory Survey. 2nd ed. International Geophysics Series, volume 92.
#'    Amsterdam ; Boston: Elsevier Academic Press, 2006.
#' 2. <https://github.com/cwebster2/pyMeteo>
#'
#' @export
T2theta <- function(temperature, pressure,
                    Rd = 287.04, Cp = 1005.7) {
    TK2TC(TC2TK(temperature) * (pressureReference / pressure)^(Rd / Cp))
}

#' Dry lapse rate in degC / m
#'
#' This is the ratio of the acceleration due to gravity and the specific
#' heat of the air, according to (3.53) of Reference 2.
#'
#' @param g acceleration due to gravity, with the default as listed in
#' Reference 2.
#'
#' @param C_P numeric value of the specific heat of air at sea level,
#' with the default being as listed in Reference 2, converted to J/(kg*K).
#'
#' @return `dryLapseRate` returns the lapse rate, in degrees C (or Kelvin) per
#' metre.
#'
#' @references
#'

#' 1. Wallace, John M., and Peter Victor Hobbs. Atmospheric Science: An
#'    Introductory Survey. 2nd ed. International Geophysics Series,
# volume 92. Amsterdam; Boston: Elsevier Academic Press, 2006.
#'
#' 2. <https://pds-atmospheres.nmsu.edu/education_and_outreach/encyclopedia/adiabatic_lapse_rate.htm>
#'
#' @author Dan Kelley
#'
#' @export
dryLapseRate <- function(g = 9.7986, C_P = 1004.0) {
    g / C_P
}

# from pymeteo; I cannot find this in WH
es <- function(TK) {
    611.2 * exp(17.67 * (TK - TC0) / (TK - 29.65))
}

# WH (3.63), approximation of (3.62)
w_vs <- function(TK, pd) {
    epsilon * (es(TK) / pd)
}

Lv <- function(TK) {
    # TODO: Temp dependance
    L
}

#' dT/dz for moist adiabat
#'
#' This is from pymeteo; I cannot find the equivalent formula at
#' in WH.
#'
#' @param TK temperature in K.
#'
#' @param p pressure in Pa (100*mbar)
#'
#' @return `dTdz_moist` returns the derivative in K/m.
#'
#' @export
dTdz_moist <- function(TK, p) {
    pd <- p - es(TK)
    tmp1 <- Lv(TK)
    tmp2 <- w_vs(TK, pd)
    num <- 1 + ((tmp1 * tmp2) / (Rd * TK))
    den <- 1 + ((tmp1^2 * tmp2) / (cpd * Rv * TK^2))
    -(g / cpd) * (num / den)
}

#' dT/dp for moist adiabat
#'
#' This is from pymeteo; I cannot find the equivalent formula at
#' in WH.
#'
#' @param TK temperature in K.
#'
#' @param p pressure in Pa (100*mbar)
#'
#' @return `dTdp_moist` returns the derivative in K/Pa.
#'
#' @export
dTdp_moist <- function(TK, p) {
    -dTdz_moist(TK, p) * (Rd * TK) / (p * g)
}

#' Determine skew for skew-T diagrams
#'
#' Based on the plotted data range and figure geometry, this determines a
#' temperature skew that be equivalent to a 45 degree rotation of isotherms.  A
#' warning will be issued if the graph is far from square or the axes
#' cover unexpected ranges.
#'
#' @param pressure numeric vector holding the pressures in the
#' sounding dataset.
#'
#' @return `skew` returns the value to be added to temperature, as a
#' function of the supplied pressure.
#'
#' @author Dan Kelley
#'
#' @export
skew <- function(pressure) {
    pin <- par("pin")
    usr <- par("usr")
    # cat("skew()...\n")
    # cat("  ", oce::vectorShow(pin))
    # cat("  ", oce::vectorShow(usr))
    dxscaled <- (usr[2] - usr[1]) / pin[1]
    dyscaled <- (usr[4] - usr[3]) / pin[2]
    # cat("  ", oce::vectorShow(dxscaled))
    # cat("  ", oce::vectorShow(dyscaled))
    ratio <- -dxscaled / dyscaled
    # cat("  ", oce::vectorShow(ratio))
    # if (ratio < 60 || ratio > 120) {
    #    warning("The ratio, ", ratio, " is outside the expected range of 60 to 120")
    # }
    ratio * log10(100 * pressure / 1e5) - ratio * log10(100 * 1050 / 1e5)
}

#' Compute moist adiabat curve for sounding plot
#'
#' This is done by integrating [dTdp_moist()] from p=100.5e3 Pa (1005 mbar)
#' to 1e3 Pa (100 mbar). Note that the computation is done in Pa and Kelvin,
#' so we need to do conversions at start and end, because this is used by
#' [plot.sounding()], which works in those units.
## FIXME: explore sensitivity to dp.
#'
#' @param TC temperature in degrees Celcius.
#'
#' @return `moistAdiabat' returns a data frame with `p` (in mbar) and
#' temperature `T` (in degC).
#'
#' @export
moistAdiabat <- function(TC) {
    TK <- TC2TK(TC)
    p <- 105e3
    dp <- 1e3
    pp <- p
    TKTK <- TK
    for (i in 1:100) {
        TK <- TK - dTdp_moist(TK, p) * dp
        p <- p - dp
        pp <- c(pp, p)
        TKTK <- c(TKTK, TK)
    }
    # return in mbar and degC, for plotting
    data.frame(p = pp / 100, T = TK2TC(TKTK))
}
