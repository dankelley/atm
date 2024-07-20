# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(atmosphere)

test_that("conversion between Pa and millibar", {
    expect_equal(50000, mbar2Pa(500))
    expect_equal(500, Pa2mbar(50000))
})

test_that("conversion between degrees C and Kelvin", {
    expect_equal(20, TK2TC(TC2TK(20)))
})
