## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----echo = TRUE--------------------------------------------------------------
library(atmosphere)
dayOffsets <- seq(-2, -1)
soundingFile <- try(downloadAtmosphericSounding())
file <- system.file("extdata", "Shearwater_2024-07-11.txt.gz", package = "atmosphere")
par(mar = c(3, 3, 1, 1), mgp = c(2, 0.7, 0))
sounding <- readAtmosphericSounding(file)

