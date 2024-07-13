#' Read atmospheric sounding data
#'
#' If the file was downloaded with [downloadAtmosphericSounding()] then the
#' filename will hold information on the station and date, so those things will
#' be inferred from and stored in the `metadata` of the return value.
#'
#' @param file character value indicating the name of a sounding file.
#' This file must have been downloaded with [downloadAtmosphericSounding()].
#'
#' @examples
#' library(atmosphere)
#' file <- system.file("extdata", "Shearwater_2024-07-11.txt.gz", package = "atmosphere")
#' sounding <- readAtmosphericSounding(file)
#'
#' @author Dan Kelley
#'
#' @export
readAtmosphericSounding <- function(file) {
    # Attempt to infer things from the filename
    stationName <- NULL
    stationNumber <- NULL
    date <- NULL
    tmp <- gsub("^.*/", "", file)
    tmp <- gsub("\\..*$", "", tmp)
    tmps <- strsplit(tmp, "")[[1]]
    if (2 == length(grep("-", tmps)) && 1 == length(grep("_", tmps))) {
        items <- strsplit(tmp, "_")[[1]]
        stationName <- items[1]
        stationNumber <- stationNumber(stationName)
        date <- items[2]
    }
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
    res <- sounding()
    res@data <- as.list(data)
    res@metadata <- list(
        stationName = stationName,
        stationNumber = stationNumber,
        date = date
    )
    res
}
