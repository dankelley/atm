#' Read atmospheric sounding data
#'
#' Read an HTML file downloaded by [downloadAtmosphericSounding()].  This
#' code works by recognizing portions of the file as preformatted blocks, which
#' is a brittle method that will fail if the HTML format used on the server website
#' changes.  Obviously, it would be better to work with csv or json files, but
#' we have no way to know how to download such files, since the server provides
#' no documentation.
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
    # Read whole file into a buffer, so we can find <PRE> and </PRE> tags,
    # which denote the two blocks that hold in profile data and meta data,
    # respectively.
    lines <- readLines(file)
    preLines <- grep("<PRE>", lines)
    endpreLines <- grep("</PRE>", lines)
    if (length(preLines) < 2) stop("cannot decode this file (did not find 2 or more <PRE> lines)")
    if (length(endpreLines) < 2) stop("cannot decode this file (did not find 2 or more </PRE> lines)")

    # Read the profile data from first PRE block
    startData <- preLines[1] + 1
    endData <- endpreLines[1] - 1
    col.names <- scan(text = lines[startData + 1], what = character(), quiet = TRUE)
    stopifnot(11 == length(col.names))
    data <- read.fwf(file,
        skip = startData + 3, n = endData - (startData + 3),
        text = lines, col.names = col.names, header = FALSE,
        widths = c(8, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7)
    )
    res <- sounding() # create return value skeleton, since things seem ok so far
    res@data <- as.list(data)
    # Read the metadata from second PRE block
    startMetadata <- preLines[2] + 1
    endMetadata <- endpreLines[2] - 1
    md <- trimws(lines[startMetadata:endMetadata])
    for (mdi in md) {
        tmp <- strsplit(mdi, ":")[[1]]
        name <- tmp[1]
        value <- trimws(tmp[2])
        #message("name=", name)
        if (identical("Observation time", name)) {
            # Handle date format e.g. 2024-07-11T00:00:00 is 240711/0000
            #message("  handle ", name, "=", value, " as time")
            res@metadata[[name]] <- as.POSIXct(value, format = "%y%m%d/%H%M", tz = "UTC")
        } else if (identical("Station identifier", name)) {
            #message("  handle ", name, "=", value, " as station identifer")
            res@metadata[[name]] <- value
        } else {
            #message("  handle ", name, "=", value, " as non-special ")
            res@metadata[[name]] <- as.numeric(value)
        }
    }
    res
}
