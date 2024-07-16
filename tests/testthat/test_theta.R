# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(atmosphere)

test_that("conversion to potential temperature and back", {
    temperature <- 300
    pressure <- 0.5e5
    theta <- thetaFromTemperature(temperature, pressure)
    expect_equal(theta, 365.78963)
    temperatureFromTheta(thetaFromTemperature(theta, pressure), pressure)
    expect_equal(temperature, temperatureFromTheta(theta, pressure))
})
