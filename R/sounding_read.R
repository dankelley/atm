#' Read atmospheric sounding data
#'
#' @param file character value indicating the name of a sounding file.
#' This file must have been downloaded with [downloadAtmosphericSounding()].
#'
#' @author Dan Kelley
#'
#' @export
readAtmosphericSounding <- function(file) {
    lines <- readLines(file) # keep in case of problems
    # Find preformatted region (which holds the data)
    start <- grep("<PRE>", lines)[1] + 1
    end <- grep("</PRE>", lines)[1] - 1
    col.names <- scan(text = lines[start + 1], what = character())
    stopifnot(11 == length(col.names))
    skip <- start + 3
    data <- read.fwf(file,
        skip = skip, n = end - skip,
        text = lines, col.names = col.names, header = FALSE,
        widths = c(8, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7)
    )
    attr(data, "stationName") <- attr(file, "stationName")
    attr(data, "stationNumber") <- attr(file, "stationNumber")
    attr(data, "date") <- attr(file, "date")
    data
}
