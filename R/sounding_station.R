# See
# https://weather.uwyo.edu/upperair/sounding.html
# https://weather.uwyo.edu/cgi-bin/sounding?region=naconf&TYPE=TEXT%3ALIST&YEAR=2024&MONTH=07&FROM=1012&TO=1012&STNM=73110
# https://www.weather.gov/source/zhu/ZHU_Training_Page/convective_parameters/skewt/skewtinfo.html

#' Find station number from name
#'
#' @param name character value indicating the name of the station.
#' So far, there are only two choices: `"Shearwater"`` and `"Nashville"`.
#'
#' @author Dan Kelley
#'
#' @export
stationNumber <- function(name = "Shearwater") {
    list("Shearwater" = "73110", "Nashville" = "72327")[[name]]
}
