# https://grafana.v2.sondehub.org/d/bbaa7894-e5f4-4c0d-be96-897b4ffde43b/radiosonde-telemetry-dashboard?var-Serial=X0813869&from=2025-08-04T11:36:36.000Z&to=2025-08-04T13:36:53.000Z&orgId=1&timezone=utc&refresh=1m
# https://sondehub-open-data.s3.amazonaws.com/export/d03b65fd-3058-4e5f-9e35-647eda7f012e
f <- "sonde.json"
library(jsonlite)
library(oce)
data(coastlineWorldFine, package = "ocedata")
if (!exists("lat")) {
    d <- read_json(f)
    lat <- sapply(d, \(x) x$lat)
    lon <- sapply(d, \(x) x$lon)
    alt <- sapply(d, \(x) x$alt)
    time <- gsub(".000Z", "", sapply(d, \(x) x$datetime)) |> as.POSIXct(format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
}
if (!interactive())
    png("radiosonde_path.png", units = "in", width = 7, height = 3, pointsize = 9, res = 200)
l <- layout(rbind(c(1, 2), c(1, 3)))
asp <- 1 / cos(mean(lat) * pi / 180)
dt <- 30 * 60 # change colour on 30-min interval
col <- floor((as.numeric(time) - as.numeric(trunc(time[1], "hour"))) / dt)
cex <- 0.5
pch <- 20
dist <- geodDist(lon, lat, lon[1], lat[1])
span <- 2 * max(3 * range(dist))
plot(coastlineWorldFine, clon = lon[1], clat = lat[1], span = span, mar = c(2, 2, 1, 1))
points(lon, lat, type = "p", col = col, cex = cex, pch = pch)
mar <- c(2.0, 3.5, 1, 1)
oce.plot.ts(time, lon,
    mar = mar,
    type = "p", col = col, cex = cex, pch = pch,
    ylab = expression("Longitude [" * degree * "E]"),
    drawTimeRange = FALSE
)
mtext(format(time[1], "%Y-%m-%d"), cex = par("cex"))
oce.plot.ts(time, lat,
    mar = mar,
    type = "p", col = col, cex = cex, pch = pch,
    ylab = expression("Latitude [" * degree * "N]"),
    drawTimeRange = FALSE
)
