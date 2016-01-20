context("Test color palettes")


## test data ##
test_that("test palettes", {
    ## skip on CRAN
    skip_on_cran()
    rm(list=ls())

    ## generate data
    a <- vimes.pal1(3)
    b <- vimes.pal1(10)

    ## check expected length
    expect_equal(length(a), 3)
    expect_equal(length(b), 10)

})

