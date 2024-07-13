#' Summarize a sounding
#'
#' @param x a `sounding` object, created with [readAtmosphericSounding()].
#'
#' @name summary
#'
#' @examples
#' library(atmosphere)
#' file <- system.file("extdata", "Shearwater_2024-07-11.txt.gz", package = "atmosphere")
#' sounding <- readAtmosphericSounding(file)
#' summary(sounding)
#'
#' @export
#'
#' @author Dan Kelley
S7::method(`summary`, atmosphere:::sounding) <- function(x) {
    cat("@data entries:     ", paste(names(x@data), sep = " "), "\n")
    cat("@metadata entries: ", paste(names(x@metadata), sep = " "), "\n")
    invisible(x)
}
