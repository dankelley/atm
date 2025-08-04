# https://grafana.v2.sondehub.org/d/bbaa7894-e5f4-4c0d-be96-897b4ffde43b/radiosonde-telemetry-dashboard?var-Serial=X0813869&from=2025-08-04T11:36:36.000Z&to=2025-08-04T13:36:53.000Z&orgId=1&timezone=utc&refresh=1m
# https://sondehub-open-data.s3.amazonaws.com/export/d03b65fd-3058-4e5f-9e35-647eda7f012e
f <- "radiosonde.json"
library(jsonlite)
library(oce)
data(coastlineWorldFine, package = "ocedata")

d <- read_json(f)
getVar <- function(d, name) {
    var <- sapply(d, \(x) {
        t <- x[[name]]
        if (is.null(t)) NA else t
    }) |> unlist()
}
lat <- getVar(d, "lat")
lon <- getVar(d, "lon")
lat <- getVar(d, "lat")
alt <- getVar(d, "alt")
temp <- getVar(d, "temp")
humidity <- getVar(d, "humidity")
datetime <- gsub(".000Z", "", getVar(d, "datetime"))
time <- as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
if (!interactive()) {
    png("radiosonde_path.png",
        units = "in",
        width = 7, height = 5, pointsize = 11, res = 200
    )
}
l <- layout(rbind(c(1, 2), c(5, 3), c(6, 4)))
asp <- 1 / cos(mean(lat) * pi / 180)
dt <- 30 * 60 # change colour on 30-min interval
col <- floor((as.numeric(time) - as.numeric(trunc(time[1], "hour"))) / dt)
cex <- 0.33
pch <- 20
dist <- geodDist(lon, lat, lon[1], lat[1])
span <- 1.2 * max(3 * range(dist))
plot(coastlineWorldFine, clon = lon[1], clat = lat[1], span = span, mar = c(2, 2, 1, 1))
points(lon, lat, type = "p", col = col, cex = 2 * cex, pch = pch)
mar <- c(2.0, 3.5, 1, 1)
oce.plot.ts(time, lon,
    mar = mar,
    type = "p", col = col, cex = cex, pch = pch,
    ylab = expression("Longitude [" * degree * "E]"),
    drawTimeRange = FALSE
)
mtext(paste("Radiosonde", d[[1]]$serial), adj = 0, cex = par("cex"))
mtext(format(time[1], "%Y-%m-%d"), adj = 1, cex = par("cex"))
oce.plot.ts(time, lat,
    mar = mar,
    type = "p", col = col, cex = cex, pch = pch,
    ylab = expression("Latitude [" * degree * "N]"),
    drawTimeRange = FALSE
)
oce.plot.ts(time, alt,
    ylim = c(0, max(alt)),
    mar = mar,
    type = "p", col = col, cex = cex, pch = pch,
    ylab = "Altitude [km]",
    drawTimeRange = FALSE
)
# oce.plot.ts(time, temp,
#    mar = mar,
#    type = "p", col = col, cex = cex, pch = pch,
#    ylab = expression("Temperature [" * degree * "C]"),
#    drawTimeRange = FALSE
# )
par(mar = c(3, 3, 1, 1), mgp = c(2, 0.7, 0))
plot(temp, alt,
    col = col, cex = cex, pch = pch,
    xlab = resizableLabel("T"),
    ylab = "Altitude [km]"
)
plot(humidity, alt,
    col = col, cex = cex, pch = pch,
    xlab = "Humidity",
    ylab = "Altitude [km]"
)
# print(sort(names(d[[100]])))
