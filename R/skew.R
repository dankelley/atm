#' Determine skew skew-T diagrams
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
