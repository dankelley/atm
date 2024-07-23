#' Draw a box with text on top
#'
#' This is useful for adding text to busy plots, as the altitude labels in
#' skew-T diagrams created with [plot.atmosphericSounding()].
#'
#' @param x,y,labels,font,cex,col as for [text()].
#'
#' @param bg colour for the box underlay.
#'
#' @examples
#' plot(c(1, 2), c(10, 20), bg = "gray", type = "n")
#' rect(xleft = 0, xright = 3, ybottom = 5, ytop = 25, col = "gray")
#' abline(h = 15)
#' abline(v = 1.5)
#' textInBox(1.5, 15, "12345 m")
#'
#' @importFrom graphics rect strheight strwidth
#' @author Dan Kelley
#'
#' @export
textInBox <- function(x, y, labels, font = 1, cex = 1, col = 1, bg = "white") {
    w <- 1.1 * strwidth(labels, cex = cex)
    h <- 3.0 * strheight(labels, cex = cex)
    if (par("xlog")) {
        xleft <- x * (1 - w / 2)
        xright <- x + (1 + w / 2)
    } else {
        xleft <- x - w / 2
        xright <- x + w / 2
    }
    if (par("ylog")) {
        ybottom <- y * (1 - h / 2)
        ytop <- y * (1 + h / 2)
    } else {
        ybottom <- y - h / 2
        ytop <- y + h / 2
    }
    rect(
        xleft = xleft, xright = xright, ybottom = ybottom, ytop = ytop,
        border = bg, col = bg
    )
    text(x, y, labels, col = col, font = font, cex = cex)
}
