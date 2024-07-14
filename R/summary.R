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
    cat(sprintf(
        "Station %s (#%s at %.2fE, %.2fN) on %s: %d data and %d metadata\n",
        x@metadata[["Station identifier"]],
        x@metadata[["Station number"]],
        x@metadata[["Station longitude"]],
        x@metadata[["Station latitude"]],
        x@metadata[["Observation time"]],
        length(x@data), length(x@metadata)
    ))
    invisible(x)
}
