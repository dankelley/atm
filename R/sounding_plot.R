#' Plot atmospheric sounding data
#'
#' This is a preliminary version.  Over time, it might be evolved to
#' plot something nearer what is shown at Reference 1.
#'
#' @param sounding the result of a call to [readAtmosphericSounding()].
#'
#' @param item character value indicating what to plot. At the moment,
#' the only choices is the default, `"DWPT+TEMP"`.
#'
#' @param legend logical value indicating whether to show a legend in the top-
#' left corner.  This is FALSE by default.
#'
#' @param mar 4-element numeric vector specifying margins, in the standard way
#' (i.e. this is handed to [par()] by this function). The default
#' is set to give space for height indications in the right margin.
#'
#' @param mgp 3-element numeric vector specifying axis spacing, in the usual way
#' (i.e. this is handed to [par()] by this function). The default removes some
#' whitespace in the margins.
#'
#' @name plot
#' @aliases plot.atmosphericSounding
#'
#' @examples
#' library(atmosphere)
#' # Normally, would download the file with downloadAtmosphericSounding()
#' file <- system.file("extdata", "Shearwater_2024-07-11.txt.gz", package = "atmosphere")
#' sounding <- readAtmosphericSounding(file)
#' plot(sounding)
#'
#' @references
#' 1. <https://www.noaa.gov/jetstream/upperair/skew-t-log-p-diagrams>
#' 2. Visual Explanation of Meteorological Skew T Log P Sounding Diagrams, 2019.
#' <https://www.youtube.com/watch?v=7p7c85hhgOo>.
#'
#' @importFrom graphics abline legend lines mtext par rug
#' @importFrom stats approx
#'
#' @export
#'
#' @author Dan Kelley
S7::method(`plot`, atmosphere:::sounding) <- function(
    x, item = "skewT", legend = FALSE,
    mar = c(3, 3, 1, 3),
    mgp = c(2, 0.7, 0)) {
    pressure <- x@data$PRES
    dewpoint <- x@data$DWPT
    temperature <- x@data$TEMP
    height <- x@data$HGHT
    par(mar = mar, mgp = mgp)
    if (item == "skewT") {
        par(mar = mar, mgp = mgp)
        # print(par('mar'))
        plot(dewpoint, pressure,
            log = "y",
            ylim = c(1050, 100), xlim = c(-40, 35),
            yaxs = "i",
            xlab = expression("Temperature and Dew Point [" * degree * "C]"),
            ylab = "Pressure [hPa]",
            type = "l", col = 4
        )
        rug(seq(100, 1000, by = 100), side = 2, ticksize = -0.02, lwd = par("lwd"))
        lines(temperature, pressure, col = 2)
        # dry adiabats slope up to the left
        # FIXME: why straight? Doesn't C_P change?
        # FIXME: sites say T sloped 45deg but how can that make sense generally?
        usr <- par("usr")
        pressureBottom <- 10^usr[3]
        for (TT in seq(-80, 300, 20)) {
            lapseRate <- dryLapseRate() # the factor is because h is in km, not m
            Tdry <- TT - lapseRate * (height - height[1])
            # print(head(data.frame(Tdry=Tdry, height=height, pressure=pressure)))
            lines(Tdry, pressure - pressure[1] + pressureBottom, col = "darkgray")
        }
        # height axis at RHS
        h <- 1e3 * c(1, 2, 5, 10)
        hp <- approx(height, pressure, h)$y
        abline(h = hp, col = "gray")
        ok <- is.finite(hp)
        # print(data.frame(h = h[ok], hp = hp[ok], labels = labels[ok]))
        mtext(paste0("   ", h[ok] / 1e3, " km"), at = hp[ok], side = 4, las = 2, cex = 0.8)
        abline(h = hp, col = "gray")
        h <- 1e3 * (1:20)
        hp <- approx(height, pressure, h)$y
        rug(hp, side = 4, ticksize = -0.02, lwd = par("lwd"))
        if (legend) {
            legend("topleft",
                lwd = par("lwd"), col = c(2, 4), cex = 0.8,
                legend = c("Temperature", "Dew Point"),
                bg = "white"
            )
        }
        mtext(sprintf(
            "Station %s on %s", x@metadata[["Station identifier"]],
            x@metadata[["Observation time"]]
        ), adj = 0)
        # add isotherms
        usr <- par("usr")
        p0 <- seq(10^usr[3], 10^usr[4], length.out = 100)
        print(range(p0))
        for (isotherm in seq(-200, 40, 5)) {
            T0 <- isotherm - skew(p0)
            lines(T0, p0, col = "darkgray")
        }
        # mtext("Red: temperature, blue: dew point", adj = 1)
    }
}
