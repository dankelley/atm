# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(atmosphere)

test_that("dTdp_moist cf pymeteo result", {
    TK <- 300 # K
    p <- 1e5
    expect_equal(0.000322484750959419, dTdp_moist(TK, p))
})

test_that("dTdz_moist cf pymeteo result", {
    TK <- 300 # K
    p <- 1e5
    expect_equal(-0.00367379158179104, dTdz_moist(TK, p))
})

test_that("conversion to potential temperature and back", {
    temperature <- 20 # degC
    pressure <- 500 # mbar
    theta <- T2theta(temperature, pressure)
    expect_equal(theta, 84.1292683343724)
    expect_equal(temperature, theta2T(theta, pressure))
})
