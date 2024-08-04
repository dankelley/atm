# support for mapDrawDirectionField(): vectorized feather computation
feathers <- function(u, step = 10, debug = 0) {
    debug <- max(debug, 0)
    if (debug) {
        cat("feathers(u, step=", step, ", debug)\n")
        cat("  originally, u=", paste(u, collapse = " "), "\n")
    }
    fac <- 5 * step
    pennant <- floor(u / fac)
    if (debug) print(pennant)
    u <- u - fac * pennant
    u <- ifelse(u < 0, 0, u)
    if (debug) cat("  after pennant, u=", paste(u, collapse = " "), "\n")
    fac <- step
    barb <- floor(u / fac)
    u <- u - fac * barb
    u <- ifelse(u < 0, 0, u)
    if (debug) print(barb)
    u <- ifelse(u < 0, 0, u)
    if (debug) cat("  after barb, u=", paste(u, collapse = " "), "\n")
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
#' @param colFlags colour of interior of flags.  This is 1 by default,
#' to fill the flags with black ink, but lighter colours may be preferable
#' to some eyes.
#'
#' @param debug integer that controls tell degree of debugging.  If this is
#' zero, work proceeds silently.  Otherwise, some debugging information will
#' be printed.
#'
#' @examples
#' library(atm)
#' L <- 8
#' plot(c(-L, L), c(-L, L), asp = 1, type = "n", xlab = "", ylab = "")
#' y <- -7
#' windBarbs(0, y, 70 * cospi(45 / 180), 70 * sinpi(45 / 180), scale = 2, colFlags = grey(0.9))
#' text(1, y, 70)
#' y <- y + 1
#' windBarbs(0, y, 70, 0, scale = 2, colFlags = grey(0.9))
#' text(1, y, 70)
#' y <- y + 1
#' for (i in 0:13) {
#'     windBarbs(0, y, 65 - 5 * i, 0, scale = 2, colFlags = grey(0.9))
#'     text(1, y, 65 - 5 * i)
#'     y <- y + 1
#' }
#'
#' @importFrom graphics points polygon segments
#'
#' @author Dan Kelley
#'
#' @export
windBarbs <- function(x, y, u, v,
                      scale = 1, length = 0.5,
                      step = 10, lwd = 1, col = 1, colFlags = 1,
                      debug = 0) {
    debug <- max(debug, 0)
    usr <- par("usr")
    pin <- par("pin")
    dx <- usr[2] - usr[1]
    dy <- usr[4] - usr[3]
    asp <- (dy / dx) * (pin[1] / pin[2])
    # FIXME: handle case where asp is not 1
    if (abs(1 - asp) > 0.01) {
        stop("must plot() with asp=1, but asp is ", asp)
    }
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
    flagCol <- colFlags
    D <- length
    barbLength <- length
    triangleWidth <- 2 * barbLength * cospi(phi / 180)
    triangleLength <- 0.5 * triangleWidth / cospi(phi / 180) # or sin?
    barbSpace <- barbLength * cospi(phi / 180)
    #ds <- 0.5 * D * cospi(phi / 180) # FIXME: oce factor is 1
    #ds <- D * cospi(phi / 180) # FIXME: oce factor is 1
    #ds <- triangleWidth / 2
    #ds <- D * cospi(phi / 180)
    #ds <- barbSpace
    ds <- length / scale * cospi(phi / 180)
    if (debug) {
        cat(sprintf(
            "windBarbs():\n  scale: %.4f, length: %.4f, ds: %.4f,\n  barbLength: %.4f, barbSpace: %.4f,\n  triangleWidth: %.4f triangleLength: %.4f\n",
            scale, length, ds, barbLength, barbSpace, triangleWidth, triangleLength
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
    if (debug > 1) {
        cat("next is f, returned by feathers()\n")
        print(f)
    }
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
                        "  jj=%d code=%d=%s lastCode=%d: ",
                        jj, code[jj],
                        switch(as.character(code[jj]),
                            "0" = "null      ",
                            "1" = "pennant   ",
                            "2" = "long barb ",
                            "3" = "short barb"
                        ),
                        lastCode
                    ))
                }
                if (thisCode == 1) { # triangle
                    if (debug > 1) {
                        # cat("thisCode=", thisCode, " (triangle)\n")
                        cat(sprintf(
                            "\n  triangle case ... theta=%.1f, thetaPage=%.1f, barbSpace=%.2f, barbLength=%.2f,\n   triangleLength=%.2f, triangleWidth=%.2f\n",
                            theta[i], thetaPage[i],
                            barbSpace,
                            barbLength,
                            triangleLength,
                            triangleWidth
                        ))
                    }
                    if (lastCode == 1) { # nudge triangles together
                        if (debug) {
                            cat(sprintf("changing s from %.4f to %4f (to snuggle triangles closer)\n", s, s + ds))
                        }
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
                    #if (debug > 1) {
                    #    cat(sprintf("s = %.3f ds=%.3f thetaPage=%.3f\n", s, ds, thetaPage))
                    #}
                    polygon(xt, yt, col = flagCol, border = col)
                    # ? message(sprintf(
                    # ?     "reducing s from %.2f to %.2f",
                    # ?     s, s - 3 * ds
                    # ? ))
                    # ? message(sprintf(
                    # ?     "3*ds = %.2f, triangleWidth+barbSpace = %.2f",
                    # ?     3 * ds, triangleWidth + barbSpace
                    # ? ))
                    # ? <- s - 3 * ds # (triangleWidth + barbSpace)
                    if (debug) {
                        cat(sprintf(
                            "changing s from %.4f to %.4f\n",
                            #s, s - triangleWidth + ds
                            s, s - 3 * ds
                        ))
                    }
                    s <- s - 3 * ds # triangleWidth # + ds # FIXME: oce: was -3*ds
                } else if (thisCode == 2 || thisCode == 3) { # barb
                    if (lowestNonzero) {
                        if (debug) {
                            cat(sprintf("changing s from %.4f to %.4f (lowestNonzero case)\n", s, s - ds))
                        }
                        s <- s - ds # barbSpace / length
                    }
                    x <- x0i + s * (x1i - x0i)
                    y <- y0i + s * (y1i - y0i)
                    segments(x, y, x + delta * BLC, y + delta * BLS, col = barbCol, lwd = barbLwd)
                    if (debug) {
                        cat(sprintf("changing s from %.4f to %.4f\n", s, s - ds))
                    }
                    s <- s - ds
                }
                lastCode <- thisCode
            }
        }
    }
}
