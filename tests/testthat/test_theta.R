# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(atmosphere)

test_that("conversion between degrees C and Kelvin", {
    expect_equal(20, K2C(C2K(20)))
})

test_that("conversion to potential temperature and back", {
    temperature <- 20
    pressure <- 500
    theta <- T2theta(temperature, pressure)
    expect_equal(theta, 84.287437907213)
    expect_equal(temperature, theta2T(theta, pressure))
})
