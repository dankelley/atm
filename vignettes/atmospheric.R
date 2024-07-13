## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----echo = TRUE--------------------------------------------------------------
library(atmosphere)
dayOffsets <- seq(-2, -1)
file <- system.file("extdata", "Shearwater_2024-07-11.txt.gz", package = "atmosphere")
sounding <- readAtmosphericSounding(file)
plot(sounding)

