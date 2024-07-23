#' Summarize a sounding
#'
#' @param x a `sounding` object, created with [readSounding()].
#'
#' @name summary
#'
#' @examples
#' library(atm)
#' file <- system.file("extdata", "Shearwater_2024-07-11.txt.gz", package = "atm")
#' sounding <- readSounding(file)
#' summary(sounding)
#'
#' @export
#'
#' @author Dan Kelley
S7::method(`summary`, atm:::sounding) <- function(x) {
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
