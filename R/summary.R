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
S7::method(`summary`, atm:::sounding) <- function(object, ...) {
    cat(sprintf(
        "Station %s (#%s at %.2fE, %.2fN) on %s: %d data and %d metadata\n",
        object@metadata[["Station identifier"]],
        object@metadata[["Station number"]],
        object@metadata[["Station longitude"]],
        object@metadata[["Station latitude"]],
        object@metadata[["Observation time"]],
        length(object@data), length(object@metadata)
    ))
    invisible(object)
}
