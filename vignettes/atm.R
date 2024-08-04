## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----echo = TRUE, fig.width = 7, fig.height = 5, fig.cap = "Skew-T log-P sounding profile at Shearwater, Nova Scotia, Canada, on July 11, 2024. The red line represents temperature and the slighly thinner blue line represents dew point. The solid dark green curves are dry adiabats, and the dashed ones are moist adiabats (think of dash as representing rain). All of these curves are skewed, so that to read values, find one of the red 45 degree lines and trace it down to a temperature on the bottom axis."----
library(atm)
file <- system.file("extdata", "Shearwater_2024-07-11.txt.gz", package = "atm")
sounding <- readSounding(file)
plot(sounding)

