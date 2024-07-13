#' Download atmospheric sounding data
#'
#' @author Dan Kelley
#'
#' @importFrom utils download.file read.fwf
#'
#' @param date A string for a date, in yyyy-mm-dd format, or some other form
#' that [as.POSIXlt()] can convert to a date or datetime.
#' If not provided, the previous day is used (because an early-morning call
#' to this function might yield a failed download).
#'
#' @param region a character value indicating the region. This defaults
#' to `"naconf`.
#'
#' @param stationName a character value indicating the station name, which
#' the present function converts to a station number by [stationNumber()].
#'
#' @export
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
    # attr(file, "stationName") <- stationName
    # attr(file, "stationNumber") <- stationNumber
    # attr(file, "date") <- sprintf("%4d-%02d-%02d", year, month, day)
    file
}
