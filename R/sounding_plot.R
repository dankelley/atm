#' Plot atmospheric sounding data
#'
#' @param sounding the result of a call to [readAtmosphericSounding()].
#'
#' @param item character value indicating what to plot. At the moment,
#' the only choices is the default, `"DWPT+TEMP"`.
#'
#' @examples
#' library(atmosphere)
#' soundingFile <- downloadAtmosphericSounding(stationName = "Shearwater")
#' sounding <- readAtmosphericSounding(soundingFile)
#' par(mar = c(3, 3, 1, 3), mgp = c(2, 0.7, 0))
#' plotAtmosphericSounding(sounding)
#' unlink(soundingFile)
#'
#' @importFrom graphics abline legend lines mtext par
#' @importFrom stats approx
#'
#' @export
#'
#' @author Dan Kelley
plotAtmosphericSounding <- function(sounding, item = "DWPT+TEMP") {
    pressure <- sounding$PRES
    dewpoint <- sounding$DWPT
    temperature <- sounding$TEMP
    height <- sounding$HGHT
    if (item == "DWPT+TEMP") {
        plot(dewpoint, pressure,
            log = "y",
            ylim = c(1050, 10), xlim = c(-90, 35),
            xlab = expression("Temperature and Dew Point [" * degree * "C]"),
            ylab = "Pressure [hPa]",
            type = "l", col = 4
        )
        lines(temperature, pressure, col = 2)
        h <- c(200, 2e3, 5e3, 10e3, 20e3, 50e3)
        hp <- approx(height, pressure, h)$y
        abline(h = hp, col = "gray")
        labels <- ifelse(h < 1e3, paste(" ", h, "m"),
            paste(" ", h / 1e3, "km")
        )
        ok <- is.finite(hp)
        # print(data.frame(h = h[ok], hp = hp[ok], labels = labels[ok]))
        mtext(labels[ok], at = hp[ok], side = 4, las = 2, cex = 0.8)
        legend("topright",
            lwd = par("lwd"), col = c(2, 4), cex = 0.8,
            legend = c("Temperature", "Dew Point"),
            bg = "white"
        )
        mtext(
            sprintf(
                "%s Sounding on %s", attr(sounding, "stationName"),
                attr(sounding, "date")
            ),
            adj = 0
        )
        # mtext("Red: temperature, blue: dew point", adj = 1)
    }
}
