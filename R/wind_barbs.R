# support for mapDrawDirectionField(): vectorized feather computation
feathers <- function(u, step = 10, debug = 0) {
    debug <- max(debug, 0)
    if (debug) {
        message("feathers(u, step=", step, ", debug)")
        message("  originally, u=", paste(u, collapse = " "))
    }
    fac <- 5 * step
    pennant <- floor(u / fac)
    if (debug) print(pennant)
    u <- u - fac * pennant
    u <- ifelse(u < 0, 0, u)
    if (debug) message("  after pennant, u=", paste(u, collapse = " "))
    fac <- step
    barb <- floor(u / fac)
    u <- u - fac * barb
    u <- ifelse(u < 0, 0, u)
    if (debug) print(barb)
    u <- ifelse(u < 0, 0, u)
    if (debug) message("  after barb, u=", paste(u, collapse = " "))
    fac <- step / 2
    demibarb <- floor(u / fac)
    cbind(pennant, barb, demibarb)
}

#' Draw wind barbs
#'
#' Draw wind barbs on an existing plot.  This (at the moment) requires
#' that the plot aspect ratio be 1.
#'
#' @param x,y numeric values indicating the locations at which to
#' draw starting points of barbs.
#'
#' @param u,v numeric values indicating the velocity components in
#' the x and y directions.
#'
#' @param scale numeric value indicating the length of barbs,
#' as measured in the axis scale.
#'
#' @param length numeric value indicating the ratio of flag length
#' to barb length.
#'
#' @param step numeric value indicating the step between speed categories.
#' The default value of 10 is somewhat standard for speeds in knots.
#'
#' @param lwd line width of barbs.
#'
#' @param col colour of barbs.
#'
#' @param debug integer that controls tell degree of debugging.  If this is
#' zero, work proceeds silently.  Otherwise, some debugging information will
#' be printed.
#'
#' @examples
#' library(atm)
#' n <- 10
#' L <- 10
#' x <- seq(-L, L, length.out = n)
#' y <- seq(-L, L, length.out = n)
#' plot(x, y, type = "n", xlab = "x", ylab = "y", asp = 1)
#' g <- expand.grid(x = x, y = y)
#' R <- sqrt(g$x^2 + g$y^2)
#' U <- 30 * R * exp(-R / (L / 2))
#' theta <- atan2(g$y, g$x)
#' g$u <- U * sin(theta)
#' g$v <- -U * cos(theta)
#' windBarbs(g$x, g$y, g$u, g$v, length=0.5, scale = 2)
#'
#' @author Dan Kelley
#'
#' @export
windBarbs <- function(x, y, u, v,
                      scale = 1, length = 0.5,
                      step = 10, lwd = 1, col = 1,
                      debug = FALSE) {
    debug <- max(debug, 0)
    usr <- par("usr")
    pin <- par("pin")
    dx <- usr[2] - usr[1]
    dy <- usr[4] - usr[3]
    asp <- (dy / dx) * (pin[1] / pin[2])
    if (debug) cat("asp = ", asp, " (FIXME: use this)")
    theta <- 180 / pi * atan2(v, u)
    S <- sinpi(theta / 180)
    C <- cospi(theta / 180)
    x0 <- x
    y0 <- y
    y1 <- y - S * scale
    x1 <- x - C * scale
    thetaPage <- 180 / pi * atan2(y1 - y0, x1 - x0)
    phi <- 70 # degrees to rotate barbs (70 by eye)
    barbLwd <- lwd
    barbCol <- col
    flagCol <- col
    D <- length
    barbLength <- D
    triangleWidth <- 2 * barbLength * cospi(phi / 180)
    triangleLength <- 0.5 * triangleWidth / cospi(phi / 180) # or sin?
    barbSpace <- barbLength * cospi(phi / 180)
    ds <- D * cospi(phi / 180)
    if (debug > 1) {
        cat(sprintf(
            "scale: %.4f, D: %.4f, ds: %.4f, barbLength: %.4f, barbSpace: %.4f, triangleWidth: %.4f\ntriangleLength: %.4f\n",
            scale, D, ds, barbLength, barbSpace, triangleWidth, triangleLength
        ))
    }
    knots <- TRUE
    speed <- sqrt(u^2 + v^2)
    speedOrig <- speed
    speed <- speedOrig * if (knots) 1 else 1.94384 # FIXME: factor likely wrong
    speed <- 5L * as.integer(round(speed / 5))
    Spage <- sinpi(thetaPage / 180)
    Cpage <- cospi(thetaPage / 180)
    x1 <- x0 + scale * Cpage
    y1 <- y0 + scale * Spage
    notStill <- speed != 0
    # points(x0[notStill], y0[notStill])
    segments(x0[notStill], y0[notStill], x1[notStill], y1[notStill], col = barbCol, lwd = barbLwd)
    f <- feathers(speed, step = step, debug = debug - 1)
    angleBarb <- thetaPage - phi
    for (i in seq_along(x0)) {
        fi <- f[i, ]
        j <- sum(fi)
        code <- c(rep(1, fi[["pennant"]]), rep(2, fi[["barb"]]), rep(3, fi[["demibarb"]]))
        lowestNonzero <- identical(as.numeric(fi), c(0, 0, 1))
        stagnant <- speedOrig[i] == 0
        verySlow <- speedOrig[i] < 2.5
        if (stagnant) {
            points(x0[i], y0[i], col = barbCol, cex = 0.5)
        } else if (verySlow) {
            segments(x0[i], y0[i], x1[i], y1[i], col = barbCol, lwd = barbLwd)
        } else {
            lastCode <- 0 # used to nudge triangles together
            BLC <- barbLength * cospi(angleBarb[i] / 180)
            BLS <- barbLength * sinpi(angleBarb[i] / 180)
            s <- 1 # position of next item (0 at x0, 1 at x1)
            for (jj in seq_len(j)) {
                thisCode <- code[jj]
                x0i <- x0[i]
                y0i <- y0[i]
                x1i <- x1[i]
                y1i <- y1[i]
                delta <- if (thisCode == 1) 2 else if (thisCode == 2) 1.0 else if (thisCode == 3) 0.6
                if (debug) {
                    cat(sprintf(
                        "  jj: %d, code: %d, lastCode:%d, delta: %.3f, s: %.4f, ds: %.4f\n",
                        jj, code[jj], lastCode, delta, s, ds
                    ))
                }
                if (thisCode == 1) { # triangle
                    if (debug > 0) {
                        cat("drawing with thisCode=", thisCode, "\n")
                        cat(sprintf(
                            "theta=%.4f, thetaPage=%.4f, barbSpace=%.4f, barbLength=%.4f, triangleLength=%.4f, triangleWidth=%.4f\n",
                            theta[i], thetaPage[i],
                            barbSpace,
                            barbLength,
                            triangleLength,
                            triangleWidth
                        ))
                    }
                    if (lastCode == 1) { # nudge triangles together
                        s <- s + ds
                    }
                    x <- x0i + s * (x1i - x0i)
                    y <- y0i + s * (y1i - y0i)
                    xt <- x + c(
                        0,
                        -triangleLength * cospi((phi + thetaPage[i]) / 180),
                        -triangleWidth * cospi(thetaPage[i] / 180)
                    )
                    yt <- y + c(
                        0,
                        -triangleLength * sinpi((phi + thetaPage[i]) / 180),
                        -triangleWidth * sinpi(thetaPage[i] / 180)
                    )
                    if (debug > 1) {
                        cat(
                            sprintf(
                                "delta=%.3f s=%.3f thetaPage=%.3f scale=%.3f triangleWidth=%.4f triangleLength=%.4f\n",
                                delta, s, thetaPage, scale, triangleWidth, triangleLength
                            )
                        )
                    }
                    polygon(xt, yt, col = flagCol, border = flagCol)
                    s <- s - 3 * ds # (triangleWidth + barbSpace)
                } else if (thisCode == 2 || thisCode == 3) { # barb
                    if (lowestNonzero) {
                        s <- s - ds # barbSpace / length
                    }
                    x <- x0i + s * (x1i - x0i)
                    y <- y0i + s * (y1i - y0i)
                    segments(x, y, x + delta * BLC, y + delta * BLS, col = barbCol, lwd = barbLwd)
                    s <- s - ds
                }
                lastCode <- thisCode
            }
        }
    }
}

#' Plot atmospheric sounding data
#'
#' This is a preliminary version.  Over time, it might be evolved to
#' plot something nearer what is shown at Reference 1.
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
    mar = c(2, 3, 1, 0.5),
    mgp = c(2, 0.7, 0), debug = 0) {
    layout(matrix(1:2, nrow = 1), widths = c(0.9, 0.1))

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
        if (FALSE) { # FIXME: add wind barbs (tricky to use oce code, since y is log)
            # mtext("Red: temperature, blue: dew point", adj = 1)
            mar <- par("mar")
            mar[c(2, 4)] <- 0
            par(mar = mar)
            # points(rep(0, 3), c(1000, 500, 200), pch = 20, cex = 2) # test that RHS is okay
            plot(c(-1, 1), c(500, 500),
                ylim = 10^usr[3:4],
                yaxs = "i", axes = FALSE, xlab = "", ylab = "", type = "n",
                log = "y"
            )
            points(rep(0, length(pressureReport)), pressureReport)
        }
        # Panel 2: wind barbs
        ylim <- par("usr")[3:4]
        par(mar = c(mar[1], 0, mar[3], 0))
        plot(rep(0, 2), ylim,
            ylim = ylim, asp = 1, type = "n",
            axes = FALSE, xlab = "", ylab = ""
        )
        box()
        scale <- 0.05
        windBarbs(0, log10(1005), 10, 0, scale = scale, length = 0.01)
        windBarbs(0, log10(900), 30, 30, scale = scale, length = 0.01)
        windBarbs(0, log10(500), 40, 40, scale = scale, length = 0.01)
    }
}
