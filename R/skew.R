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
    fin <- par("fin")
    usr <- par("usr")
    # message(oce::vectorShow(fin))
    # message(oce::vectorShow(usr))
    dxscaled <- (usr[2] - usr[1]) / fin[1]
    dyscaled <- (usr[4] - usr[3]) / fin[2]
    # message(oce::vectorShow(dxscaled))
    # message(oce::vectorShow(dyscaled))
    ratio <- -dxscaled / dyscaled
    # message(oce::vectorShow(ratio))
    if (ratio < 60 || ratio > 120) {
        warning("The ratio, ", ratio, " is outside the expected range of 60 to 120")
    }
    ratio * log10(100 * pressure / 1e5) - ratio * log10(100 * 1050 / 1e5)
}
