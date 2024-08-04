#' Plot atmospheric sounding data
#'
#' Display sounding data in profile form.  Red lines sloping from bottom-left
#' to top-right indicate temperatures. The thicker red curve is in-situ temperature.
#' The thinner blue curve is dew-point temperature.  The curved olive green
#' lines indicate wet and dry adiabats, with dashed lines used for
#' the former. The vertical axis represents pressure, and thin horizontal
#' lines are drawn to indicate standard heights. The wind barbs drawn to the right
#' of the profile indicate velocities at those heights in meteorological
#' convention for speeds in knots.
#'
#' @param sounding the result of a call to [readSounding()].
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
#' @aliases plot.sounding
#'
#' @examples
#' library(atm)
#' # Normally, would download the file with downloadSounding()
#' file <- system.file("extdata", "Shearwater_2024-07-11.txt.gz", package = "atm")
#' sounding <- readSounding(file)
#' plot(sounding)
#'
#' @references
#' 1. <https://www.noaa.gov/jetstream/upperair/skew-t-log-p-diagrams>
#' 2. Visual Explanation of Meteorological Skew T Log P Sounding Diagrams, 2019.
#' <https://www.youtube.com/watch?v=7p7c85hhgOo>.
#'
#' @importFrom graphics abline legend lines mtext par rug text
#' @importFrom stats approx
#'
#' @export
#'
#' @author Dan Kelley
S7::method(`plot`, atm:::sounding) <- function(
    x, item = "skewT", legend = FALSE,
    mar = c(2, 3, 1, 0),
    mgp = c(2, 0.7, 0), debug = 0) {
    layout(matrix(1:2, nrow = 1), widths = c(0.85, 0.15))

    aes <- list(
        height = list(col = "gray", lwd = 1, lty = 1),
        temperature = list(col = 2, lwd = 3, lty = 1), # red
        isotherm = list(col = 2, lwd = 1, lty = 1), # red
        dewpoint = list(col = 4, lwd = 2, lty = 1), # blue
        adiabatDry = list(col = "DarkKhaki", lwd = 1.6, lty = 1),
        adiabatWet = list(col = "DarkKhaki", lwd = 1.8, lty = 2)
    )
    dewpoint <- x@data$DWPT
    height <- x@data$HGHT
    pressure <- x@data$PRES
    temperature <- x@data$TEMP
    windDirection <- x@data$DRCT
    windSpeed <- x@data$SKNT
    if (item == "skewT") {
        par(mar = mar, mgp = mgp)
        if (FALSE) {
            layout(matrix(1:2, ncol = 2), widths = c(0.9, 0.1))
        }
        # print(par('mar'))
        plot(0, 1000,
            log = "y",
            ylim = c(1050, 100), xlim = c(-40, 40),
            xaxs = "i", yaxs = "i",
            xlab = "", # expression("Temperature and Dew Point [" * degree * "C]"),
            ylab = "Pressure [hPa]",
            type = "n", axes = FALSE
        )
        usr <- par("usr")
        box()
        axis(2)
        # rug(seq(-40, 40, 10), side = 1, ticksize = -0.02, lwd = par("lwd"))
        for (temp in seq(-30, 30, 10)) {
            text(temp - 2, 10^usr[3] * 1.1, bquote(.(temp) * degree), srt = 45, xpd = NA, col = 2, font = 2)
        }
        SKEW <- skew(pressure)
        lines(dewpoint - SKEW, pressure,
            col = aes$dewpoint$col, lwd = aes$dewpoint$lwd, lty = aes$dewpoint$lty
        )
        lines(temperature - SKEW, pressure,
            col = aes$temperature$col, lwd = aes$temperature$lwd, lty = aes$temperature$lty
        )
        # dry adiabats slope up to the left
        for (TT in seq(-80, 300, 10)) {
            lines(theta2T(TT, pressure) - SKEW, pressure,
                col = aes$adiabatDry$col, lwd = aes$adiabatDry$lwd, lty = aes$adiabatDry$lty
            )
        }
        # moist adiabats also slope up to the left (mostly) and approach dry adiabats
        # at high altitudes.
        for (TT in seq(-80, 300, 5)) {
            wa <- moistAdiabat(TT) # computed by integrating dTdp_moist()
            lines(wa$T - skew(wa$p), wa$p,
                col = aes$adiabatWet$col, lwd = aes$adiabatWet$lwd, lty = aes$adiabatWet$lty
            )
        }
        # report height at same pressures as used by Wisconsin server
        pressureReport <- c(1000, 925, 850, 700, 600, 500, 400, 300, 200, 150)
        heightReport <- approx(pressure, height, pressureReport, ties = mean)$y
        windDirectionReport <- approx(pressure, windDirection, pressureReport, ties = mean)$y
        windSpeedReport <- approx(pressure, windSpeed, pressureReport, ties = mean)$y
        if (debug > 0) {
            cat("pressureReport:\n")
            print(data.frame(heightReport = heightReport, pressureReport = pressureReport))
        }
        abline(h = pressureReport, lwd = aes$height$lwd, col = aes$height$col, lty = aes$height$lty)
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
        for (isotherm in seq(-200, 40, 10)) {
            T0 <- isotherm - skew(p0)
            lines(T0, p0,
                col = aes$isotherm$col, lwd = aes$isotherm$lwd, lty = aes$isotherm$lty
            )
        }
        # label heights (with white below)
        xlabel <- rep(usr[1] + 0.08 * (usr[2] - usr[1]), length(pressureReport))
        textInBox(xlabel, pressureReport, sprintf("%.0f m", heightReport), cex = 0.8)
        # Panel 2: wind barbs
        ylim <- -par("usr")[3:4]
        par(mar = c(mar[1], 0, mar[3], 0)) # no axes so remove left-right margins
        par(xpd = NA) # permit barbs outside box
        plot(rep(0, 2), ylim,
            xaxs = "i", yaxs = "i",
            ylim = ylim, asp = 1, type = "n",
            axes = FALSE, xlab = "", ylab = ""
        )
        #box()
        # FIXME: perhaps scale and length ought to be parameters to this function
        scale <- 0.07
        length <- 0.02
        theta <- 180 - (90 - windDirectionReport) # use math conversion
        u <- windSpeedReport * cospi(theta / 180)
        v <- windSpeedReport * sinpi(theta / 180)
        if (debug > 0) {
            print(data.frame(p = pressureReport, dir = windDirectionReport, sp = windSpeedReport, u = u, v = v),
                digits = 2
            )
        }
        points(rep(0, length(pressureReport)), -log10(pressureReport),
            cex = 0.5, pch = 20, col = gray(0.5)
        )
        windBarbs(rep(0, length(pressureReport)), -log10(pressureReport),
            u, -v,
            scale = scale, length = length,
            col = gray(0.5),
            colFlag = gray(0.9),
            debug = debug - 1
        )
        # windBarbs(0, log10(1005), 10, 0, scale = scale, length = length)
    }
}
