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
#' @param debug an integer specifying whether information is to be printed
#' during processing. The default, 0, means to avoid such printing. Any
#' positive value means to print some information.  (That information will
#' vary over the course of evolution of the code, so do not rely on
#' particular behaviour.)
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
    mgp = c(2, 0.7, 0), debug = 0) {
    aes <- list(
        height = list(col = "gray", lwd = 1, lty = 1),
        temperature = list(col = 2, lwd = 3, lty = 1),
        isotherm = list(col = 2, lwd = 1, lty = 1),
        dewpoint = list(col = 4, lwd = 3, lty = 1),
        adiabatDry = list(col = "Dark Orange", lwd = 2, lty = 3),
        adiabatWet = list(col = "Dark Olive Green", lwd = 1, lty = 3)
    )
    pressure <- x@data$PRES
    dewpoint <- x@data$DWPT
    temperature <- x@data$TEMP
    height <- x@data$HGHT
    if (item == "skewT") {
        par(mar = mar, mgp = mgp)
        # print(par('mar'))
        plot(0, 1000,
            log = "y",
            ylim = c(1050, 100), xlim = c(-40, 40),
            xaxs = "i", yaxs = "i",
            xlab = expression("Temperature and Dew Point [" * degree * "C]"),
            ylab = "Pressure [hPa]",
            type = "n"
        )
        rug(seq(-40, 40, 10), side = 1, ticksize = -0.02, lwd = par("lwd"))
        usr <- par("usr")
        SKEW <- skew(pressure)
        lines(dewpoint - SKEW, pressure, col = aes$dewpoint$col, lwd = aes$dewpoint$lwd, lty = aes$dewpoint$lty)
        lines(temperature - SKEW, pressure, col = aes$temperature$col, lwd = aes$temperature$lwd, lty = aes$temperature$lty)
        # dry adiabats slope up to the left
        for (TT in seq(-80, 300, 10)) {
            lines(theta2T(TT, pressure) - SKEW, pressure,
                col = aes$adiabatDry$col, lwd = aes$adiabatDry$lwd, lty = aes$adiabatDry$lty
            )
        }
        # report height at same pressures as used by Wisconson
        pressureReport <- c(1000, 925, 850, 700, 600, 500, 400, 300, 200, 150)
        heightReport <- approx(pressure, height, pressureReport, ties = mean)$y
        if (debug > 0) {
            cat("pressureReport:\n")
            print(data.frame(heightReport = heightReport, pressureReport = pressureReport))
        }
        abline(h = pressureReport, lwd = aes$height$lwd, col = aes$height$col, lty = aes$height$lty)
        # print(data.frame(h = h[ok], hp = hp[ok], labels = labels[ok]))
        xlabel <- rep(usr[1] + 0 * (usr[2] - usr[1]), length(pressureReport))
        text(xlabel, pressureReport, sprintf("%.0f m", heightReport), cex = 0.8, pos = 4)
        # abline(h = hp, col = "gray")
        # h <- 1e3 * (1:20)
        # hp <- approx(height, pressure, h[ok])$y
        # rug(hp, side = 4, ticksize = -0.02, lwd = par("lwd"))
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
        ), side = 3, adj = 0)
        # Skewed isotherms
        p0 <- seq(10^usr[3], 10^usr[4], length.out = 100)
        for (isotherm in seq(-200, 40, 5)) {
            T0 <- isotherm - skew(p0)
            lines(T0, p0, col = aes$isotherm$col, lwd = aes$isotherm$lwd, lty = aes$isotherm$lty)
        }
        # mtext("Red: temperature, blue: dew point", adj = 1)
    }
}
