## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----echo = TRUE, fig.width = 5, fig.height = 5, fig.cap = "Skew-T log-P sounding profile at Shearwater, Nova Scotia, Canada, on July 11, 2024. The wider line represents temperature and the  thinner line represents dew point. The curves are skewed to the right; to read values, trace the 45 degree lines down to the horizontal axis. Orange curved lines are dry adiabats, and green curved lines are moist adiabats; note how they arpproach at high altitudes."----
library(atmosphere)
file <- system.file("extdata", "Shearwater_2024-07-11.txt.gz", package = "atmosphere")
sounding <- readAtmosphericSounding(file)
plot(sounding)

