#' Download atmospheric sounding data
#'
#' The file is downloaded from Reference 1, by constructing a URL
#' that queries the server for a particular station and time. The
#' construction of that URL is based on reverse-engineering the
#' URLs that get created by clicking on a map.  This means
#' that `downloadAtmosphericSounding` is brittle, and will
#' fail if the server setup changes.  Unfortunately, this
#' guessing game seems to be the best we can do, since the website
#' does not provide information on the URL construction, let alone
#' an API.
#'
#' @importFrom utils download.file read.fwf
#'
#' @param date A string for a date, in yyyy-mm-dd format, or some other form
#' that [as.POSIXlt()] can convert to a date or datetime.
#' If not provided, the previous day is used (because an early-morning call
#' to this function might yield a failed download).
#'
#' @param region a character value indicating the region. This defaults
#' to `"naconf"`.
#'
#' @param stationName a character value indicating the station name, which
#' the present function converts to a station number by [stationNumber()].
#'
#' @references
#'
#' 1. <https://weather.uwyo.edu/upperair/sounding.html>
#'
#' @export
#'
#' @author Dan Kelley
downloadAtmosphericSounding <- function(
    date = Sys.Date() - 1,
    region = "naconf",
    stationName = "Shearwater") {
    stationNumber <- stationNumber(stationName)
    t <- as.POSIXlt(date)
    type <- "TEXT" # I am not sure what other values use, but the code needs this
    year <- 1900 + t$year
    month <- 1 + t$mon
    day <- -1 + t$mday # yesterday
    hour <- 0 # FIXME: what are the choices for this at the data provider?
    url <- paste0(
        "https://weather.uwyo.edu/cgi-bin/sounding?",
        sprintf("region=%s&", region),
        sprintf("TYPE=%s%%3ALIST&", type),
        sprintf("YEAR=%4d&", year),
        sprintf("MONTH=%02d&", month),
        sprintf("FROM=%02d%02d&%02d%02d&", day, hour, day, hour),
        sprintf("STNM=%s", stationNumber)
    )
    file <- sprintf("%s_%04d-%02d-%02d.txt", stationName, year, month, day)
    if (!file.exists(file)) {
        download.file(url, file)
        message("downloaded file ", file)
    } else {
        message("using cached file ", file)
    }
    file
}
