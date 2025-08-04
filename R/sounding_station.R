# See
# https://weather.uwyo.edu/upperair/soundings.shtml
# https://weather.uwyo.edu/cgi-bin/sounding?region=naconf&TYPE=TEXT%3ALIST&YEAR=2024&MONTH=07&FROM=1012&TO=1012&STNM=73110
# https://www.weather.gov/source/zhu/ZHU_Training_Page/convective_parameters/skewt/skewtinfo.html

#' Find station number from name
#'
#' @param name character value indicating the name of the station.
#' Use `name=NULL` (the default) to get a list of known names.
#' This list is *very* short, and not necessarily up-to-date!
#'
#' @author Dan Kelley
#'
#' @export
stationNumber <- function(name = NULL) {
    choices <- list(
        "Shearwater, NS" = "73110", # fails Aug 4, 2025
        "Mount Pearl, NF" = "71802",
        "Nashville, TE" = "72327",
        "Yarmouth, NS" = "71603"
    )
    if (!is.null(name) && name %in% names(choices)) {
        return(choices[[name]])
    } else {
        stop(
            "Please supply a station name from this list of choices: \"",
            paste(names(choices), collapse = "\", \""), "\""
        )
    }
}
